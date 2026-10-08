package scalus.bloxbean

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import scalus.cardano.address.{Address, Network, StakeAddress, StakePayload}
import scalus.uplc.builtin.ByteString
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

    private val koiosUrl = "https://api.koios.rest/api/v1"
    private val slotConfig = SlotConfig.mainnet

    /** The cert state a mainnet tx at `slot` sees, as of the start of its block.
      *
      * It holds every account the tx withdraws from or certifies that is registered then, with its
      * reward balance, the deposit its registration paid, and its pool and DRep delegations. It
      * also holds every pool and DRep those accounts and the tx's certificates delegate to that is
      * registered then. Registrations, deposits and pool delegations come from Blockfrost; vote
      * delegations, pool and DRep histories and the treasury from Koios, since Blockfrost reports
      * them only as of today. Every response is cached under `cachePath`.
      *
      * Updates in the tx's own block are not seen: both date them by slot or block time, which the
      * txs of one block share. Script accounts get no DRep: no rule of the replay reads it. The
      * reward balance is the rewards Koios lists as spendable, less the withdrawals before the tx.
      * A DRep's expiry is not modelled and is 0; a pool's parameters are those of its latest
      * registration, with the margin Koios prints as a decimal.
      */
    def resolveForTx(tx: Transaction, slot: SlotNo): CertState = {
        val epoch = slotConfig.epochOf(slot).toInt
        val time = slotConfig.slotToTime(slot) / 1000
        val accounts = collectStakeCredentials(tx).toSeq.flatMap { credential =>
            accountAt(credential, slot, time, epoch).map(credential -> _)
        }.toMap
        val pools = (delegatedPools(tx) ++ accounts.values.flatMap(_.stakePoolDelegation))
            .flatMap(pool => poolAt(pool, time, epoch).map(pool -> _))
            .toMap
        val dreps = (delegatedDReps(tx) ++ accounts.values.flatMap(_.dRepDelegation))
            .flatMap(drepCredential)
            .flatMap(drep => drepAt(drep, time).map(drep -> _))
            .toMap
        CertState(
          VotingState(dreps),
          PoolsState(
            stakePools = pools.view.mapValues(_._1).toMap,
            retiring = pools.collect { case (pool, (_, Some(epoch))) => pool -> epoch }
          ),
          DelegationState(accounts)
        )
    }

    /** The treasury at the start of mainnet `epoch`, from Koios `totals`, cached under `cachePath`.
      */
    def treasuryAt(epoch: Int): Coin = {
        val json = koios(s"koios-totals-$epoch.json", s"/totals?_epoch_no=$epoch", None)
            .getOrElse(sys.error(s"Koios has no totals for epoch $epoch"))
        Coin(readFromArray[List[KoiosTotals]](json).head.treasury.toLong)
    }

    private def accountAt(
        credential: Credential,
        slot: SlotNo,
        time: Long,
        epoch: Int
    ): Option[ConwayAccountState] = {
        val stakeAddress = credentialToStakeAddress(credential).get
        val id = credentialId(credential)
        // a deregistration and a registration in one slot: the registration came last
        val registration = blockfrostPages[BlockfrostRegistration](
          s"blockfrost-account-registrations-$id",
          s"/accounts/$stakeAddress/registrations"
        ).filter(_.tx_slot < slot)
            .sortBy(r => (r.tx_slot, r.action == "registered"))
            .lastOption
            .filter(_.action == "registered")
        registration.map { reg =>
            val pool = blockfrostPages[BlockfrostDelegation](
              s"blockfrost-account-delegations-$id",
              s"/accounts/$stakeAddress/delegations"
            ).filter(d => d.tx_slot >= reg.tx_slot && d.tx_slot < slot)
                .sortBy(_.tx_slot)
                .lastOption
                .map(d => PoolKeyHash.fromArray(Bech32.decode(d.pool_id).data))
                .filter(poolAt(_, time, epoch).isDefined)
            val drep = credential match
                case Credential.KeyHash(_) => drepDelegationAt(credential, reg.tx_slot, slot, time)
                // only key accounts need a DRep to withdraw, spec [SC-23]; Koios cannot list the
                // updates of a withdraw-zero script account, which withdraws in every batch
                case Credential.ScriptHash(_) => None
            val balance = rewardBalanceAt(credential, stakeAddress, slot, epoch)
            ConwayAccountState(Coin(balance), Coin(reg.deposit.fold(0L)(_.toLong)), pool, drep)
        }
    }

    /** The reward balance of `credential` at `slot`, in `epoch`: every reward Koios lists as
      * spendable by `epoch`, less every withdrawal Blockfrost lists before `slot`. Koios lists the
      * rewards the ledger paid, with the instant rewards and refunds; Blockfrost's reward list
      * lacks the instant rewards and its MIR list keeps the MIR certificates a later one in the
      * same epoch overrode. A deregistration needs a balance of 0, so the rewards and withdrawals
      * before the latest registration cancel out. An account with no reward skips the withdrawals:
      * a withdraw-zero script account can have thousands of them.
      */
    private def rewardBalanceAt(
        credential: Credential,
        stakeAddress: String,
        slot: SlotNo,
        epoch: Int
    ): Long = {
        val rewards = koiosPages[KoiosReward](
          s"koios-account-rewards-${credentialId(credential)}",
          "/account_reward_history",
          s"""{"_stake_addresses":["$stakeAddress"]}"""
        ).filter(_.spendable_epoch <= epoch).map(_.amount.toLong).sum
        if rewards == 0 then 0
        else
            val withdrawn = blockfrostPages[BlockfrostWithdrawal](
              s"blockfrost-account-withdrawals-${credentialId(credential)}",
              s"/accounts/$stakeAddress/withdrawals"
            ).filter(_.tx_slot < slot).map(_.amount.toLong).sum
            rewards - withdrawn
    }

    /** The DRep the key account of `credential`, registered at `registeredAt`, delegates to at
      * `slot`, from the dated vote delegations Koios lists. Blockfrost has no such history.
      */
    private def drepDelegationAt(
        credential: Credential,
        registeredAt: SlotNo,
        slot: SlotNo,
        time: Long
    ): Option[DRep] = {
        val stakeAddress = credentialToStakeAddress(credential).get
        koiosJson[List[KoiosAccountUpdates]](
          s"koios-account-updates-${credentialId(credential)}.json",
          "/account_updates",
          Some(s"""{"_stake_addresses":["$stakeAddress"]}""")
        ).flatMap(_.updates)
            .filter(u =>
                u.action_type == "delegation_drep" && u.absolute_slot >= registeredAt &&
                    u.absolute_slot < slot
            )
            .sortBy(_.absolute_slot)
            .lastOption
            .flatMap { update =>
                txCerts(update.tx_hash)
                    .filter(_.info.stake_address.contains(stakeAddress))
                    .flatMap(_.info.drep_id)
                    .lastOption
                    .map(drepOf)
                    .filter(
                      drepKeptSince(_, slotConfig.slotToTime(update.absolute_slot) / 1000, time)
                    )
            }
    }

    /** A delegation to `drep` made at `since` still stands at `time`: a DRep deregistration in
      * between clears it, as Conway GOVCERT does.
      */
    private def drepKeptSince(drep: DRep, since: Long, time: Long): Boolean =
        drepCredential(drep).forall { credential =>
            drepUpdates(credential).forall(u =>
                u.action != "deregistered" || u.block_time < since || u.block_time >= time
            ) && drepAt(credential, time).isDefined
        }

    /** The registration of `pool` at `time`, in `epoch`, and the epoch it retires in, if any. */
    private def poolAt(
        pool: PoolKeyHash,
        time: Long,
        epoch: Int
    ): Option[(Certificate.PoolRegistration, Option[Long])] = {
        val poolId = Bech32.encode("pool", pool.bytes)
        val updates = koiosJson[List[KoiosPoolUpdate]](
          s"koios-pool-updates-${pool.toHex}.json",
          s"/pool_updates?_pool_bech32=$poolId",
          None
        ).filter(_.block_time < time).sortBy(_.block_time)
        val lastRegistration = updates.lastIndexWhere(_.update_type == "registration")
        if lastRegistration < 0 then None
        else
            val retiring = updates
                .drop(lastRegistration + 1)
                .filter(_.update_type == "deregistration")
                .flatMap(_.retiring_epoch)
                .lastOption
            if retiring.exists(_ <= epoch) then None
            else Some(poolRegistration(pool, updates(lastRegistration)) -> retiring)
    }

    private def drepAt(credential: Credential, time: Long): Option[DRepState] = {
        val updates = drepUpdates(credential).filter(_.block_time < time).sortBy(_.block_time)
        val lastRegistration = updates.lastIndexWhere(_.action == "registered")
        val registered = lastRegistration >= 0 &&
            !updates.drop(lastRegistration + 1).exists(_.action == "deregistered")
        Option.when(registered) {
            val anchor = updates
                .drop(lastRegistration)
                .flatMap(u =>
                    for url <- u.meta_url; hash <- u.meta_hash
                    yield Anchor(url, DataHash.fromHex(hash))
                )
            DRepState(
              expiry = 0,
              anchor = anchor.lastOption,
              deposit = Coin(updates(lastRegistration).deposit.fold(0L)(_.toLong)),
              delegates = Set.empty
            )
        }
    }

    private def drepUpdates(credential: Credential): List[KoiosDRepUpdate] = {
        val (header, hash) = credential match
            case Credential.KeyHash(h)    => (0x22, h)
            case Credential.ScriptHash(h) => (0x23, h)
        val drepId = Bech32.encode("drep", header.toByte +: hash.bytes)
        koiosJson[List[KoiosDRepUpdate]](
          s"koios-drep-updates-${credentialId(credential)}.json",
          s"/drep_updates?_drep_id=$drepId",
          None
        )
    }

    private def txCerts(txHash: String): List[KoiosCert] =
        koiosJson[List[KoiosTxCerts]](
          s"koios-tx-certs-$txHash.json",
          "/tx_info",
          Some(
            s"""{"_tx_hashes":["$txHash"],"_inputs":false,"_metadata":false,"_assets":false,""" +
                """"_withdrawals":false,"_certs":true,"_scripts":false,"_bytecode":false,""" +
                """"_governance":false}"""
          )
        ).flatMap(_.certificates)

    private def koiosJson[A: JsonValueCodec](file: String, path: String, body: Option[String]): A =
        readFromArray[A](
          koios(file, path, body).getOrElse(sys.error(s"Koios $path failed, see the log above"))
        )

    /** The Koios response to `path`, a POST if `body` is given, cached as `file`. */
    private def koios(file: String, path: String, body: Option[String]): Option[Array[Byte]] = {
        val builder = HttpRequest.newBuilder().uri(URI.create(s"$koiosUrl$path"))
        val request = body.fold(builder.GET())(b =>
            builder
                .header("content-type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(b))
        )
        cached(file, s"Koios $path ${body.getOrElse("")}", request, notFoundIsEmpty = false)
    }

    /** Every page of the Koios list at `path`, a POST of `body`; each page of 1000 rows is cached
      * as `name-<page>.json`.
      */
    private def koiosPages[A](name: String, path: String, body: String)(using
        JsonValueCodec[List[A]]
    ): List[A] = {
        def page(n: Int): List[A] = {
            val entries = koiosJson[List[A]](
              s"$name-$n.json",
              s"$path?offset=${(n - 1) * 1000}&limit=1000",
              Some(body)
            )
            if entries.size < 1000 then entries else entries ++ page(n + 1)
        }
        page(1)
    }

    /** Every page of the Blockfrost list at `path`, oldest first; each page is cached as
      * `name-<page>.json`. A 404, for an unknown account, is an empty list.
      */
    private def blockfrostPages[A](name: String, path: String)(using
        JsonValueCodec[List[A]]
    ): List[A] = {
        def page(n: Int): List[A] = {
            val request = HttpRequest
                .newBuilder()
                .uri(URI.create(s"$baseUrl$path?order=asc&count=100&page=$n"))
                .header("project_id", apiKey)
                .GET()
            val bytes = cached(s"$name-$n.json", s"Blockfrost $path page $n", request, true)
                .getOrElse(sys.error(s"Blockfrost $path failed, see the log above"))
            val entries = readFromArray[List[A]](bytes)
            if entries.size < 100 then entries else entries ++ page(n + 1)
        }
        page(1)
    }

    /** The response to `request`, from the cache file `file` if present. Retries a rate-limited or
      * failed request a few times, and caches only a 200, or a 404 as `[]` if `notFoundIsEmpty`.
      */
    private def cached(
        file: String,
        label: String,
        request: HttpRequest.Builder,
        notFoundIsEmpty: Boolean
    ): Option[Array[Byte]] = {
        val cacheFile = cachePath.resolve(file)
        if Files.exists(cacheFile) then Some(Files.readAllBytes(cacheFile))
        else
            val built = request
                .timeout(Duration.ofSeconds(180))
                .header("accept", "application/json")
                .build()
            def attempt(n: Int): Option[Array[Byte]] =
                val response: Option[HttpResponse[Array[Byte]]] =
                    try Some(client.send(built, HttpResponse.BodyHandlers.ofByteArray()))
                    catch
                        case NonFatal(e) =>
                            println(s"$label failed: ${e.getMessage}")
                            None
                response match
                    case Some(r) if r.statusCode() == 200 => Some(r.body())
                    case Some(r) if r.statusCode() == 404 && notFoundIsEmpty =>
                        Some("[]".getBytes)
                    case other if n < 4 =>
                        other.foreach(r => println(s"$label: status ${r.statusCode()}, retrying"))
                        Thread.sleep(2000L << n)
                        attempt(n + 1)
                    case other =>
                        other.foreach(r => println(s"$label failed: ${r.statusCode()}"))
                        None
            val result = attempt(0)
            result.foreach { bytes =>
                Files.createDirectories(cachePath)
                Files.write(cacheFile, bytes)
            }
            result
    }

    private def delegatedPools(tx: Transaction): Seq[PoolKeyHash] =
        tx.body.value.certificates.toSeq.collect {
            case Certificate.StakeDelegation(_, pool)             => pool
            case Certificate.StakeRegDelegCert(_, pool, _)        => pool
            case Certificate.StakeVoteDelegCert(_, pool, _)       => pool
            case Certificate.StakeVoteRegDelegCert(_, pool, _, _) => pool
        }

    private def delegatedDReps(tx: Transaction): Seq[DRep] =
        tx.body.value.certificates.toSeq.collect {
            case Certificate.VoteDelegCert(_, drep)               => drep
            case Certificate.StakeVoteDelegCert(_, _, drep)       => drep
            case Certificate.VoteRegDelegCert(_, drep, _)         => drep
            case Certificate.StakeVoteRegDelegCert(_, _, drep, _) => drep
        }

    /** The cert state with every account of `tx` registered, with a cached reward balance. */
    @deprecated("use resolveForTx(tx, slot), which resolves the state as of the tx", "1.3.0")
    def resolveForTx(tx: Transaction, epoch: Int, defaultDeposit: Coin = Coin.zero): CertState = {
        val credentials = collectStakeCredentials(tx)
        if credentials.isEmpty then CertState.empty
        else {
            val resolved = credentials.flatMap { cred =>
                resolveCredential(cred, epoch, defaultDeposit)
            }

            // A cached state without a deposit gets a zero deposit; it still counts as
            // registered.
            val accounts = resolved.map { case Resolved(cred, rewards, deposit) =>
                cred -> ConwayAccountState(Coin(rewards), Coin(deposit.getOrElse(0L)), None, None)
            }.toMap

            CertState(VotingState(Map.empty), PoolsState(), DelegationState(accounts))
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

    private def drepCredential(drep: DRep): Option[Credential] = drep match
        case DRep.KeyHash(hash)    => Some(Credential.KeyHash(hash))
        case DRep.ScriptHash(hash) => Some(Credential.ScriptHash(hash))
        case _                     => None

    /** A DRep from its Koios id: CIP-129 (`drep1` with a 0x22 or 0x23 header), CIP-105 (`drep1`,
      * `drep_script1`), or one of the two predefined DReps.
      */
    private def drepOf(drepId: String): DRep = drepId match
        case "drep_always_abstain"       => DRep.AlwaysAbstain
        case "drep_always_no_confidence" => DRep.AlwaysNoConfidence
        case _ =>
            val decoded = Bech32.decode(drepId)
            val data = decoded.data
            if data.length == 29 then
                if data(0) == 0x22 then DRep.KeyHash(AddrKeyHash.fromArray(data.tail))
                else DRep.ScriptHash(ScriptHash.fromArray(data.tail))
            else if decoded.hrp == "drep_script" then DRep.ScriptHash(ScriptHash.fromArray(data))
            else DRep.KeyHash(AddrKeyHash.fromArray(data))

    private def poolRegistration(
        pool: PoolKeyHash,
        update: KoiosPoolUpdate
    ): Certificate.PoolRegistration = {
        def stakeAddress(bech32: String): StakeAddress =
            Address.fromBech32(bech32).asInstanceOf[StakeAddress]
        val margin = update.margin.get
        Certificate.PoolRegistration(
          operator = AddrKeyHash.fromArray(pool.bytes),
          vrfKeyHash = VrfKeyHash.fromHex(update.vrf_key_hash.get),
          pledge = Coin(update.pledge.get.toLong),
          cost = Coin(update.fixed_cost.get.toLong),
          margin = UnitInterval(
            margin.bigDecimal.unscaledValue.longValueExact,
            BigInt(10).pow(margin.scale).toLong
          ),
          rewardAccount = RewardAccount(stakeAddress(update.reward_addr.get)),
          poolOwners = update.owners.toList.flatten.toSet.flatMap(o =>
              stakeAddress(o).credential.keyHashOption
          ),
          relays = update.relays.toList.flatten.toIndexedSeq.map { relay =>
              (relay.dns, relay.srv) match
                  case (Some(dns), _) => Relay.SingleHostName(relay.port, dns)
                  case (_, Some(srv)) => Relay.MultiHostName(srv)
                  case _ =>
                      def ip(text: Option[String]) = text.map(t =>
                          ByteString.fromArray(java.net.InetAddress.getByName(t).getAddress)
                      )
                      Relay.SingleHostAddr(relay.port, ip(relay.ipv4), ip(relay.ipv6))
          },
          poolMetadata = for url <- update.meta_url; hash <- update.meta_hash
          yield PoolMetadata(url, ByteString.fromHex(hash))
        )
    }

    case class KoiosTotals(epoch_no: Int, treasury: String)
    case class KoiosAccountUpdate(
        tx_hash: String,
        action_type: String,
        absolute_slot: Long
    )
    case class BlockfrostRegistration(action: String, deposit: Option[String], tx_slot: Long)
    case class BlockfrostDelegation(pool_id: String, tx_slot: Long)
    case class BlockfrostWithdrawal(amount: String, tx_slot: Long)
    case class KoiosReward(spendable_epoch: Int, amount: String)
    case class KoiosAccountUpdates(stake_address: String, updates: List[KoiosAccountUpdate])
    case class KoiosCertInfo(
        stake_address: Option[String] = None,
        drep_id: Option[String] = None
    )
    case class KoiosCert(`type`: String, index: Int, info: KoiosCertInfo)
    case class KoiosTxCerts(tx_hash: String, certificates: List[KoiosCert])
    case class KoiosRelay(
        dns: Option[String] = None,
        srv: Option[String] = None,
        ipv4: Option[String] = None,
        ipv6: Option[String] = None,
        port: Option[Int] = None
    )
    case class KoiosPoolUpdate(
        tx_hash: String,
        block_time: Long,
        update_type: String,
        // null in a retirement
        vrf_key_hash: Option[String] = None,
        margin: Option[BigDecimal] = None,
        fixed_cost: Option[String] = None,
        pledge: Option[String] = None,
        reward_addr: Option[String] = None,
        owners: Option[List[String]] = None,
        relays: Option[List[KoiosRelay]] = None,
        meta_url: Option[String] = None,
        meta_hash: Option[String] = None,
        retiring_epoch: Option[Long] = None
    )
    case class KoiosDRepUpdate(
        block_time: Long,
        action: String,
        deposit: Option[String] = None,
        meta_url: Option[String] = None,
        meta_hash: Option[String] = None
    )

    given blockfrostRegistrationListCodec: JsonValueCodec[List[BlockfrostRegistration]] =
        JsonCodecMaker.make
    given blockfrostDelegationListCodec: JsonValueCodec[List[BlockfrostDelegation]] =
        JsonCodecMaker.make
    given blockfrostWithdrawalListCodec: JsonValueCodec[List[BlockfrostWithdrawal]] =
        JsonCodecMaker.make
    given koiosRewardListCodec: JsonValueCodec[List[KoiosReward]] = JsonCodecMaker.make
    given koiosTotalsListCodec: JsonValueCodec[List[KoiosTotals]] = JsonCodecMaker.make
    given koiosAccountUpdatesListCodec: JsonValueCodec[List[KoiosAccountUpdates]] =
        JsonCodecMaker.make
    given koiosTxCertsListCodec: JsonValueCodec[List[KoiosTxCerts]] = JsonCodecMaker.make
    given koiosPoolUpdateListCodec: JsonValueCodec[List[KoiosPoolUpdate]] = JsonCodecMaker.make
    given koiosDRepUpdateListCodec: JsonValueCodec[List[KoiosDRepUpdate]] = JsonCodecMaker.make
    given JsonValueCodec[CachedStakeState] = JsonCodecMaker.make
    given JsonValueCodec[AccountInfo] = JsonCodecMaker.make
    given JsonValueCodec[RewardEntry] = JsonCodecMaker.make
    given rewardEntryListCodec: JsonValueCodec[List[RewardEntry]] = JsonCodecMaker.make
}
