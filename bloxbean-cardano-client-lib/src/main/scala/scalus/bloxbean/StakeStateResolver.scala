package scalus.bloxbean

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.time.Duration
import scala.util.control.NonFatal

final class StakeStateResolver(
    apiKey: String,
    cachePath: Path,
    network: Network = Network.Mainnet,
    baseUrl: String = "https://cardano-mainnet.blockfrost.io/api/v0"
) {
    import StakeStateResolver.*

    private val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(20)).build()

    def resolveForTx(tx: Transaction, epoch: Int, defaultDeposit: Coin = Coin.zero): CertState = {
        val credentials = collectStakeCredentials(tx)
        if credentials.isEmpty then CertState.empty
        else {
            val resolved = credentials.flatMap { cred =>
                resolveCredential(cred, epoch, defaultDeposit)
            }

            val rewards = resolved.collect { case Resolved(cred, rewards, _) =>
                cred -> Coin(rewards)
            }.toMap

            val deposits = resolved.collect { case Resolved(cred, _, Some(deposit)) =>
                cred -> Coin(deposit)
            }.toMap

            CertState(
              VotingState(Map.empty),
              PoolsState(),
              DelegationState(
                rewards = rewards,
                deposits = deposits,
                stakePools = Map.empty,
                dreps = Map.empty
              )
            )
        }
    }

    private def collectStakeCredentials(tx: Transaction): Set[Credential] = {
        val certCreds = tx.body.value.certificates.toSeq.flatMap {
            case Certificate.RegCert(credential, _)              => Some(credential)
            case Certificate.UnregCert(credential, _)            => Some(credential)
            case Certificate.StakeDelegation(credential, _)      => Some(credential)
            case Certificate.StakeRegDelegCert(credential, _, _) => Some(credential)
            case Certificate.StakeVoteRegDelegCert(credential, _, _, _) =>
                Some(credential)
            case Certificate.VoteDelegCert(credential, _)         => Some(credential)
            case Certificate.StakeVoteDelegCert(credential, _, _) => Some(credential)
            case Certificate.VoteRegDelegCert(credential, _, _)   => Some(credential)
            case _                                                => None
        }

        val withdrawalCreds = tx.body.value.withdrawals.toSeq
            .flatMap(_.withdrawals.keys)
            .map(_.address.credential)

        (certCreds ++ withdrawalCreds).toSet
    }

    private def resolveCredential(
        credential: Credential,
        epoch: Int,
        defaultDeposit: Coin
    ): Option[Resolved] = {
        val cacheFile = cachePath.resolve(s"stake-$epoch-${credentialId(credential)}.json")
        readCache(cacheFile)
            .orElse {
                val result = fetchFromBlockfrost(credential, epoch, defaultDeposit)
                result.foreach(state => writeCache(cacheFile, state))
                result
            }
            .map { state =>
                Resolved(credential, state.rewards, state.deposit)
            }
    }

    private def fetchFromBlockfrost(
        credential: Credential,
        epoch: Int,
        defaultDeposit: Coin
    ): Option[CachedStakeState] = {
        val stakeAddress = credentialToStakeAddress(credential) match
            case Some(addr) => addr
            case None =>
                println(s"Could not derive stake address for $credential")
                return None

        // Check if account exists
        val accountInfo = getAccountInfo(stakeAddress)
        if accountInfo.isEmpty then return None

        // Compute historical reward balance:
        // balance = sum(rewards up to epoch-2) - sum(withdrawals before epoch)
        // Rewards from epoch N become available at epoch N+2
        val rewardBalance = computeRewardBalanceAtEpoch(stakeAddress, epoch)

        Some(
          CachedStakeState(
            epoch = epoch,
            deposit = Some(defaultDeposit.value),
            rewards = rewardBalance
          )
        )
    }

    private def computeRewardBalanceAtEpoch(stakeAddress: String, epoch: Int): Long = {
        // NOTE: This is an approximation. The accurate historical reward balance would be:
        //   sum(rewards where epoch <= E-2) - sum(withdrawals where withdrawal_epoch < E)
        // However, Blockfrost doesn't provide withdrawal epochs directly, requiring
        // expensive per-tx lookups. For most validation cases (especially zero-withdrawals),
        // summing available rewards is sufficient.
        val allRewards = getAllRewards(stakeAddress)
        val availableRewards = allRewards.filter(_.epoch <= epoch - 2)
        availableRewards.map(r => parseLong(r.amount).getOrElse(0L)).sum
    }

    private def getAllRewards(stakeAddress: String): List[RewardEntry] = {
        def fetchPage(page: Int): List[RewardEntry] = {
            val path = s"/accounts/$stakeAddress/rewards?order=asc&count=100&page=$page"
            getJson(path)
                .flatMap { json =>
                    try Some(readFromArray[List[RewardEntry]](json))
                    catch case NonFatal(_) => None
                }
                .getOrElse(Nil)
        }

        var allEntries = List.empty[RewardEntry]
        var page = 1
        var entries = fetchPage(page)
        while entries.nonEmpty do
            allEntries = allEntries ++ entries
            page += 1
            entries = fetchPage(page)
        allEntries
    }

    private def getAccountInfo(stakeAddress: String): Option[AccountInfo] = {
        val path = s"/accounts/$stakeAddress"
        getJson(path).flatMap { json =>
            try
                val info = readFromArray[AccountInfo](json)
                Some(info)
            catch
                case NonFatal(e) =>
                    None
        }
    }

    private def credentialToStakeAddress(credential: Credential): Option[String] = {
        val payload = credential match
            case Credential.KeyHash(hash) =>
                StakePayload.Stake(Hash.stakeKeyHash(hash))
            case Credential.ScriptHash(hash) =>
                StakePayload.Script(hash)
        StakeAddress(network, payload).toBech32.toOption
    }

    private def getJson(path: String): Option[Array[Byte]] = {
        val request = HttpRequest
            .newBuilder()
            .uri(URI.create(s"$baseUrl$path"))
            .timeout(Duration.ofSeconds(30))
            .header("project_id", apiKey)
            .GET()
            .build()

        try
            val response = client.send(request, HttpResponse.BodyHandlers.ofByteArray())
            response.statusCode() match
                case 200 => Some(response.body())
                case 404 => None
                case status =>
                    println(
                      s"Blockfrost $path failed: status=$status, body=${String(response.body())}"
                    )
                    None
        catch
            case NonFatal(e) =>
                println(s"Blockfrost $path failed: ${e.getMessage}")
                None
    }

    private def readCache(path: Path): Option[CachedStakeState] = {
        if Files.exists(path) then
            try Some(readFromArray[CachedStakeState](Files.readAllBytes(path)))
            catch
                case NonFatal(e) =>
                    println(s"Failed to read cache $path: ${e.getMessage}")
                    None
        else None
    }

    private def writeCache(path: Path, value: CachedStakeState): Unit = {
        try
            Files.createDirectories(path.getParent)
            Files.write(path, writeToArray(value))
        catch
            case NonFatal(e) =>
                println(s"Failed to write cache $path: ${e.getMessage}")
    }
}

object StakeStateResolver {
    case class Resolved(
        credential: Credential,
        rewards: Long,
        deposit: Option[Long]
    )

    case class CachedStakeState(
        epoch: Int,
        deposit: Option[Long],
        rewards: Long
    )

    case class AccountInfo(
        stake_address: String,
        active: Boolean,
        active_epoch: Option[Long] = None,
        controlled_amount: Option[String] = None,
        rewards_sum: Option[String] = None,
        withdrawals_sum: Option[String] = None,
        withdrawable_amount: Option[String] = None,
        pool_id: Option[String] = None
    ) {
        def activeEpoch: Option[Long] = active_epoch
        def withdrawableAmount: Option[String] = withdrawable_amount
    }

    case class RewardEntry(
        epoch: Int,
        amount: String,
        pool_id: String,
        `type`: String
    )

    private def parseLong(value: String): Option[Long] =
        try Some(value.toLong)
        catch case NonFatal(_) => None

    private def credentialId(credential: Credential): String = credential match
        case Credential.KeyHash(hash)    => s"key-${hash.toHex}"
        case Credential.ScriptHash(hash) => s"script-${hash.toHex}"

    given JsonValueCodec[CachedStakeState] = JsonCodecMaker.make
    given JsonValueCodec[AccountInfo] = JsonCodecMaker.make
    given JsonValueCodec[RewardEntry] = JsonCodecMaker.make
    given rewardEntryListCodec: JsonValueCodec[List[RewardEntry]] = JsonCodecMaker.make
}
