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
                warn(s"Could not derive stake address for $credential")
                return None

        getAccountHistory(stakeAddress, epoch) match
            case Some(entry) =>
                val rewards = rewardsFromHistory(entry)
                Some(
                  CachedStakeState(
                    epoch = epoch,
                    deposit = Some(defaultDeposit.value),
                    rewards = rewards
                  )
                )
            case None =>
                // Account was not active at this epoch
                None
    }

    private def credentialToStakeAddress(credential: Credential): Option[String] = {
        val payload = credential match
            case Credential.KeyHash(hash) =>
                StakePayload.Stake(Hash.stakeKeyHash(hash))
            case Credential.ScriptHash(hash) =>
                StakePayload.Script(hash)
        StakeAddress(network, payload).toBech32.toOption
    }

    private def getAccountHistory(
        stakeAddress: String,
        epoch: Int
    ): Option[AccountHistoryEntry] = {
        val path = s"/accounts/$stakeAddress/history?order=asc&count=100"
        getJson(path).flatMap { json =>
            try
                val entries = readFromArray[List[AccountHistoryEntry]](json)
                // Find entry for the exact epoch, or the closest earlier epoch if the account
                // was registered before but we're querying an epoch without explicit history
                entries.find(_.activeEpoch == epoch.toLong).orElse {
                    // Check if account was active at any point at or before this epoch
                    val earlierEntries = entries.filter(_.activeEpoch <= epoch.toLong)
                    earlierEntries.lastOption
                }
            catch
                case NonFatal(e) =>
                    warn(s"Failed to parse history for $stakeAddress: ${e.getMessage}")
                    None
        }
    }

    private def rewardsFromHistory(entry: AccountHistoryEntry): Long = {
        val rewardsSum = entry.rewardsSum.flatMap(parseLong).getOrElse(0L)
        val withdrawalsSum = entry.withdrawalsSum.flatMap(parseLong).getOrElse(0L)
        val fromSums = rewardsSum - withdrawalsSum
        if fromSums >= 0 then fromSums
        else entry.rewards.flatMap(parseLong).getOrElse(0L)
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
                    warn(
                      s"Blockfrost $path failed: status=$status, body=${String(response.body())}"
                    )
                    None
        catch
            case NonFatal(e) =>
                warn(s"Blockfrost $path failed: ${e.getMessage}")
                None
    }

    private def readCache(path: Path): Option[CachedStakeState] = {
        if Files.exists(path) then
            try Some(readFromArray[CachedStakeState](Files.readAllBytes(path)))
            catch
                case NonFatal(e) =>
                    warn(s"Failed to read cache $path: ${e.getMessage}")
                    None
        else None
    }

    private def writeCache(path: Path, value: CachedStakeState): Unit = {
        try
            Files.createDirectories(path.getParent)
            Files.write(path, writeToArray(value))
        catch
            case NonFatal(e) =>
                warn(s"Failed to write cache $path: ${e.getMessage}")
    }

    private def warn(message: String): Unit =
        println(s"[warn] $message")
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

    case class AccountHistoryEntry(
        active_epoch: Long,
        amount: Option[String] = None,
        pool_id: Option[String] = None,
        rewards: Option[String] = None,
        withdrawals: Option[String] = None,
        rewards_sum: Option[String] = None,
        withdrawals_sum: Option[String] = None
    ) {
        def activeEpoch: Long = active_epoch
        def rewardsSum: Option[String] = rewards_sum
        def withdrawalsSum: Option[String] = withdrawals_sum
    }

    private def parseLong(value: String): Option[Long] =
        try Some(value.toLong)
        catch case NonFatal(_) => None

    private def credentialId(credential: Credential): String = credential match
        case Credential.KeyHash(hash)    => s"key-${hash.toHex}"
        case Credential.ScriptHash(hash) => s"script-${hash.toHex}"

    given JsonValueCodec[CachedStakeState] = JsonCodecMaker.make
    given JsonValueCodec[AccountHistoryEntry] = JsonCodecMaker.make
    given JsonValueCodec[List[AccountHistoryEntry]] = JsonCodecMaker.make
}
