package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.NullOptions.given
import org.typelevel.paiges.Doc
import scalus.utils.{Pretty, Style}

import scala.collection.mutable

/** Represents a certificate for stake operations on the blockchain
  */
enum Certificate {
    // Legacy Shelley certificates
//    case StakeRegistration(credential: Credential)
//    case StakeDeregistration(credential: Credential)
    // delegate to a stake pool
    case StakeDelegation(credential: Credential, poolKeyHash: PoolKeyHash)

    case PoolRegistration(
        operator: AddrKeyHash,
        vrfKeyHash: VrfKeyHash,
        pledge: Coin,
        cost: Coin,
        margin: UnitInterval,
        rewardAccount: RewardAccount,
        poolOwners: Set[AddrKeyHash],
        relays: IndexedSeq[Relay],
        poolMetadata: Option[PoolMetadata]
    )
    case PoolRetirement(poolKeyHash: PoolKeyHash, epochNo: Long)
    // register staking credential and pay deposit, old StakeRegistration when coin is None
    case RegCert(credential: Credential, coin: Option[Coin]) // old StakeRegistration
    // deregister staking credential and pay deposit, old StakeDeregistration when coin is None
    case UnregCert(credential: Credential, coin: Option[Coin]) // old StakeDeregistration
    // delegate to dRep
    case VoteDelegCert(credential: Credential, drep: DRep)
    // delegate to stake pool and dRep
    case StakeVoteDelegCert(credential: Credential, poolKeyHash: PoolKeyHash, drep: DRep)
    // register staking credential and delegate to stake pool
    case StakeRegDelegCert(credential: Credential, poolKeyHash: PoolKeyHash, coin: Coin)
    // register voting credential and delegate to dRep
    case VoteRegDelegCert(credential: Credential, drep: DRep, coin: Coin)
    // register staking credential, delegate to stake pool and dRep
    case StakeVoteRegDelegCert(
        credential: Credential,
        poolKeyHash: PoolKeyHash,
        drep: DRep,
        coin: Coin
    )

    case AuthCommitteeHotCert(
        committeeColdCredential: Credential,
        committeeHotCredential: Credential
    )
    case ResignCommitteeColdCert(
        committeeColdCredential: Credential,
        anchor: Option[Anchor]
    )
    case RegDRepCert(drepCredential: Credential, coin: Coin, anchor: Option[Anchor])
    case UnregDRepCert(drepCredential: Credential, coin: Coin)
    case UpdateDRepCert(drepCredential: Credential, anchor: Option[Anchor])

    def lookupRegStakeTxCert: Option[Credential] = this match
        case Certificate.RegCert(c, _)                     => Some(c)
        case Certificate.StakeRegDelegCert(c, _, _)        => Some(c)
        case Certificate.VoteRegDelegCert(c, _, _)         => Some(c)
        case Certificate.StakeVoteRegDelegCert(c, _, _, _) => Some(c)
        case _                                             => None

    def lookupUnRegStakeTxCert: Option[Credential] = this match
        case Certificate.UnregCert(c, _) => Some(c)
        case _                           => None

    def keyHashes: Set[AddrKeyHash | PoolKeyHash] = {
        this match
            case cert: Certificate.StakeDelegation => cert.credential.keyHashOption.toSet
            case cert: Certificate.PoolRegistration =>
                cert.poolOwners.view.concat(Some(cert.operator)).toSet
            case cert: Certificate.PoolRetirement => Set(cert.poolKeyHash)
            case cert: Certificate.RegCert        =>
                // Conway era transitional behavior: witness only required for Conway-style
                // registration (coin=Some, CBOR tag 7), not for Shelley-style (coin=None, CBOR tag 0).
                // See cardano-ledger getVKeyWitnessConwayTxCert in Conway/TxCert.hs
                if cert.coin.nonEmpty then cert.credential.keyHashOption.toSet else Set.empty
            case cert: Certificate.UnregCert             => cert.credential.keyHashOption.toSet
            case cert: Certificate.VoteDelegCert         => cert.credential.keyHashOption.toSet
            case cert: Certificate.StakeVoteDelegCert    => cert.credential.keyHashOption.toSet
            case cert: Certificate.StakeRegDelegCert     => cert.credential.keyHashOption.toSet
            case cert: Certificate.VoteRegDelegCert      => cert.credential.keyHashOption.toSet
            case cert: Certificate.StakeVoteRegDelegCert => cert.credential.keyHashOption.toSet
            case cert: Certificate.AuthCommitteeHotCert =>
                cert.committeeColdCredential.keyHashOption.toSet
            case cert: Certificate.ResignCommitteeColdCert =>
                cert.committeeColdCredential.keyHashOption.toSet
            case cert: Certificate.RegDRepCert    => cert.drepCredential.keyHashOption.toSet
            case cert: Certificate.UnregDRepCert  => cert.drepCredential.keyHashOption.toSet
            case cert: Certificate.UpdateDRepCert => cert.drepCredential.keyHashOption.toSet
    }

    def scriptHashOption: Option[ScriptHash] = {
        this match
            case cert: Certificate.StakeDelegation => cert.credential.scriptHashOption
            case _: Certificate.PoolRegistration   => None
            case _: Certificate.PoolRetirement     => None
            case cert: Certificate.RegCert         =>
                // Conway era transitional behavior: Script witness is NOT required for
                // Shelley-style registration (coin=None, CBOR tag 0) for backward compatibility.
                // Script witness IS required for Conway-style registration (coin=Some, CBOR tag 7).
                // See cardano-ledger getScriptWitnessConwayTxCert in Conway/TxCert.hs:
                // "We preserve the old behavior of not requiring a witness for staking credential
                // registration, but only during the transitional period of Conway era and only for
                // staking credential registration certificates without a deposit."
                if cert.coin.nonEmpty then cert.credential.scriptHashOption else None
            case cert: Certificate.UnregCert             => cert.credential.scriptHashOption
            case cert: Certificate.VoteDelegCert         => cert.credential.scriptHashOption
            case cert: Certificate.StakeVoteDelegCert    => cert.credential.scriptHashOption
            case cert: Certificate.StakeRegDelegCert     => cert.credential.scriptHashOption
            case cert: Certificate.VoteRegDelegCert      => cert.credential.scriptHashOption
            case cert: Certificate.StakeVoteRegDelegCert => cert.credential.scriptHashOption
            case cert: Certificate.AuthCommitteeHotCert =>
                cert.committeeColdCredential.scriptHashOption
            case cert: Certificate.ResignCommitteeColdCert =>
                cert.committeeColdCredential.scriptHashOption
            case cert: Certificate.RegDRepCert    => cert.drepCredential.scriptHashOption
            case cert: Certificate.UnregDRepCert  => cert.drepCredential.scriptHashOption
            case cert: Certificate.UpdateDRepCert => cert.drepCredential.scriptHashOption
    }
}

object Certificate {

    def shelleyTotalRefundsTxCerts(
        lookupStakingDeposit: Credential => Option[Coin],
        params: ProtocolParams,
        certificates: Iterable[Certificate]
    ): Coin = {
        val keyDeposit = params.stakeAddressDeposit
        val regCreds = mutable.Set.empty[Credential]
        var totalRefunds = 0L
        certificates.foreach { cert =>
            cert.lookupRegStakeTxCert match
                case Some(cred) =>
                    // Need to track new delegations in case that the same key is later deregistered in the same transaction.
                    regCreds += cred
                case None =>
                    cert match
                        case Certificate.UnregCert(cred, maybeRefund) =>
                            // If the credential was registered in this same tx, refund that deposit
                            if regCreds.contains(cred) then
                                totalRefunds += keyDeposit
                                regCreds -= cred // Remove to avoid double refund
                            else
                                // Use explicit refund from certificate if available,
                                // otherwise look up the original deposit from ledger state
                                maybeRefund match
                                    case Some(refund) => totalRefunds += refund.value
                                    case None =>
                                        lookupStakingDeposit(cred) match
                                            case Some(deposit) => totalRefunds += deposit.value
                                            case None          =>

                        case _ =>
        }
        Coin(totalRefunds)
    }

    def conwayDRepRefundsTxCerts(
        lookupDRepDeposit: Credential => Option[Coin],
        certificates: Iterable[Certificate]
    ): Coin = {
        val drepRegsInTx = mutable.Map.empty[Credential, Coin]
        var totalRefund = 0L

        certificates.foreach {
            case Certificate.RegDRepCert(cred, deposit, _) =>
                // Track registrations
                drepRegsInTx.put(cred, deposit)
            case Certificate.UnregDRepCert(cred, _) =>
                // DRep previously registered in the same tx
                drepRegsInTx.remove(cred) match
                    case Some(deposit) =>
                        totalRefund += deposit.value
                    case None =>
                        // DRep previously registered in some other tx
                        lookupDRepDeposit(cred).foreach { deposit => totalRefund += deposit.value }

            case _ => // Do nothing for other certificate types
        }
        Coin(totalRefund)
    }

    /** Calculate total deposits required for Shelley-era certificates (stake pool and delegation)
      *
      * @param params
      *   Protocol parameters containing deposit amounts
      * @param certificates
      *   Certificates in the transaction
      * @return
      *   Total deposit amount
      */
    def shelleyTotalDeposits(
        params: ProtocolParams,
        certificates: Iterable[Certificate]
    ): Coin = {
        val keyDeposit = params.stakeAddressDeposit
        val poolDeposit = params.stakePoolDeposit
        var totalDeposits = 0L

        certificates.foreach {
            case Certificate.RegCert(_, Some(deposit)) =>
                // Conway-style registration with explicit deposit
                totalDeposits += deposit.value
            case Certificate.RegCert(_, None) =>
                // Shelley-style registration using protocol parameter
                totalDeposits += keyDeposit
            case Certificate.StakeRegDelegCert(_, _, deposit) =>
                // Registration and delegation with deposit
                totalDeposits += deposit.value
            case Certificate.VoteRegDelegCert(_, _, deposit) =>
                // Registration and vote delegation with deposit
                totalDeposits += deposit.value
            case Certificate.StakeVoteRegDelegCert(_, _, _, deposit) =>
                // Registration, pool delegation and vote delegation with deposit
                totalDeposits += deposit.value
            case Certificate.PoolRegistration(_, _, _, _, _, _, _, _, _) =>
                // Pool registration
                totalDeposits += poolDeposit
            case _ => // Other certificates don't require deposits
        }
        Coin(totalDeposits)
    }

    /** Calculate total deposits required for Conway-era DRep certificates
      *
      * @param params
      *   Protocol parameters containing DRep deposit amount
      * @param certificates
      *   Certificates in the transaction
      * @return
      *   Total DRep deposit amount
      */
    def conwayDRepDeposits(
        params: ProtocolParams,
        certificates: Iterable[Certificate]
    ): Coin = {
        val drepDeposit = params.dRepDeposit
        var totalDeposits = 0L

        certificates.foreach {
            case Certificate.RegDRepCert(_, deposit, _) =>
                // Use explicit deposit from certificate
                totalDeposits += deposit.value
            case _ => // Other certificates don't require DRep deposits
        }
        Coin(totalDeposits)
    }

    given Encoder[Certificate] with
        def write(w: Writer, value: Certificate): Writer = value match
            case Certificate.RegCert(credential, None) =>
                w.writeArrayHeader(2)
                    .writeInt(0)
                    .write(credential)

            case Certificate.UnregCert(credential, None) =>
                w.writeArrayHeader(2)
                    .writeInt(1)
                    .write(credential)

            case Certificate.StakeDelegation(credential, poolKeyHash) =>
                w.writeArrayHeader(3)
                    .writeInt(2)
                    .write(credential)
                    .write(poolKeyHash)

            case Certificate.PoolRegistration(
                  operator,
                  vrfKeyHash,
                  pledge,
                  cost,
                  margin,
                  rewardAccount,
                  poolOwners,
                  relays,
                  poolMetadata
                ) =>
                w.writeArrayHeader(10)
                w.writeInt(3)
                // Write operator
                w.write(operator)

                // Write VRF key hash
                w.write(vrfKeyHash)

                // Write pledge
                w.writeLong(pledge.value)

                // Write cost
                w.writeLong(cost.value)

                // Write margin
                w.write(margin)

                // Write reward account
                w.write(rewardAccount)

                // Write pool owners as a set
                writeSet(w, poolOwners)

                // Write relays as an array
                w.writeIndexedSeq(relays)

                // Write pool metadata or null
                w.write(poolMetadata)

            case Certificate.PoolRetirement(poolKeyHash, epochNo) =>
                w.writeArrayHeader(3)
                    .writeInt(4)
                    .write(poolKeyHash)
                    .writeLong(epochNo)

            case Certificate.RegCert(credential, Some(coin)) =>
                w.writeArrayHeader(3)
                    .writeInt(7)
                    .write(credential)
                    .write(coin)

            case Certificate.UnregCert(credential, Some(coin)) =>
                w.writeArrayHeader(3)
                    .writeInt(8)
                    .write(credential)
                    .write(coin)

            case Certificate.VoteDelegCert(credential, drep) =>
                w.writeArrayHeader(3)
                    .writeInt(9)
                    .write(credential)
                    .write(drep)

            case Certificate.StakeVoteDelegCert(credential, poolKeyHash, drep) =>
                w.writeArrayHeader(4)
                    .writeInt(10)
                    .write(credential)
                    .write(poolKeyHash)
                    .write(drep)

            case Certificate.StakeRegDelegCert(credential, poolKeyHash, coin) =>
                w.writeArrayHeader(4)
                    .writeInt(11)
                    .write(credential)
                    .write(poolKeyHash)
                    .write(coin)

            case Certificate.VoteRegDelegCert(credential, drep, coin) =>
                w.writeArrayHeader(4)
                    .writeInt(12)
                    .write(credential)
                    .write(drep)
                    .write(coin)

            case Certificate.StakeVoteRegDelegCert(credential, poolKeyHash, drep, coin) =>
                w.writeArrayHeader(5)
                    .writeInt(13)
                    .write(credential)
                    .write(poolKeyHash)
                    .write(drep)
                    .write(coin)

            case Certificate.AuthCommitteeHotCert(
                  committeeColdCredential,
                  committeeHotCredential
                ) =>
                w.writeArrayHeader(3)
                    .writeInt(14)
                    .write(committeeColdCredential)
                    .write(committeeHotCredential)

            case Certificate.ResignCommitteeColdCert(committeeColdCredential, anchor) =>
                w.writeArrayHeader(3)
                    .writeInt(15)
                    .write(committeeColdCredential)
                    .write(anchor)

            case Certificate.RegDRepCert(drepCredential, coin, anchor) =>
                w.writeArrayHeader(4)
                    .writeInt(16)
                    .write(drepCredential)
                    .write(coin)
                    .write(anchor)

            case Certificate.UnregDRepCert(drepCredential, coin) =>
                w.writeArrayHeader(3)
                    .writeInt(17)
                    .write(drepCredential)
                    .write(coin)

            case Certificate.UpdateDRepCert(drepCredential, anchor) =>
                w.writeArrayHeader(3)
                    .writeInt(18)
                    .write(drepCredential)
                    .write(anchor)

    /** Helper to write a Set as CBOR */
    private def writeSet[A](w: Writer, set: Set[A])(using encoder: Encoder[A]): Writer =
        // Use indefinite array
        w.writeTag(Tag.Other(258))
        w.writeArrayHeader(set.size)
        set.foreach(encoder.write(w, _))
        w

    given Decoder[Certificate] with
        def read(r: Reader): Certificate =
            //            println(s"Certificate: ${DataItem.stringify(r.dataItem())}")
            r.readArrayHeader()
            val tag = r.readInt()
            //            println(s"Certificate tag: ${tag}")

            tag match
                case 0 => Certificate.RegCert(r.read[Credential](), None) // old StakeRegistration
                case 1 => Certificate.UnregCert(r.read[Credential](), None)
                case 2 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[PoolKeyHash]()
                    Certificate.StakeDelegation(credential, poolKeyHash)
                case 3 =>
                    val operator = r.read[AddrKeyHash]()
                    val vrfKeyHash = r.read[VrfKeyHash]()
                    val pledge = r.read[Coin]()
                    val cost = r.read[Coin]()
                    val margin = r.read[UnitInterval]()
                    val rewardAccount = r.read[RewardAccount]()
                    val poolOwners = readSet[AddrKeyHash](r)
                    val relays = r.read[IndexedSeq[Relay]]()
                    val poolMetadata = r.read[Option[PoolMetadata]]()

                    Certificate.PoolRegistration(
                      operator = operator,
                      vrfKeyHash = vrfKeyHash,
                      pledge = pledge,
                      cost = cost,
                      margin = margin,
                      rewardAccount = rewardAccount,
                      poolOwners = poolOwners,
                      relays = relays,
                      poolMetadata = poolMetadata
                    )
                case 4 =>
                    val poolKeyHash = r.read[PoolKeyHash]()
                    val epochNo = r.readLong()
                    Certificate.PoolRetirement(poolKeyHash, epochNo)
                case 7 =>
                    val credential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    Certificate.RegCert(credential, Some(coin))
                case 8 =>
                    val credential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    Certificate.UnregCert(credential, Some(coin))
                case 9 =>
                    val credential = r.read[Credential]()
                    val drep = r.read[DRep]()
                    Certificate.VoteDelegCert(credential, drep)
                case 10 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[PoolKeyHash]()
                    val drep = r.read[DRep]()
                    Certificate.StakeVoteDelegCert(credential, poolKeyHash, drep)
                case 11 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[PoolKeyHash]()
                    val coin = r.read[Coin]()
                    Certificate.StakeRegDelegCert(credential, poolKeyHash, coin)
                case 12 =>
                    val credential = r.read[Credential]()
                    val drep = r.read[DRep]()
                    val coin = r.read[Coin]()
                    Certificate.VoteRegDelegCert(credential, drep, coin)
                case 13 =>
                    val credential = r.read[Credential]()
                    val poolKeyHash = r.read[PoolKeyHash]()
                    val drep = r.read[DRep]()
                    val coin = r.read[Coin]()
                    Certificate.StakeVoteRegDelegCert(credential, poolKeyHash, drep, coin)
                case 14 =>
                    val committeeColdCredential = r.read[Credential]()
                    val committeeHotCredential = r.read[Credential]()
                    Certificate.AuthCommitteeHotCert(
                      committeeColdCredential,
                      committeeHotCredential
                    )
                case 15 =>
                    val committeeColdCredential = r.read[Credential]()
                    val anchor = r.read[Option[Anchor]]()
                    Certificate.ResignCommitteeColdCert(committeeColdCredential, anchor)
                case 16 =>
                    val drepCredential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    val anchor = r.read[Option[Anchor]]()
                    Certificate.RegDRepCert(drepCredential, coin, anchor)
                case 17 =>
                    val drepCredential = r.read[Credential]()
                    val coin = r.read[Coin]()
                    Certificate.UnregDRepCert(drepCredential, coin)
                case 18 =>
                    val drepCredential = r.read[Credential]()
                    val anchor = r.read[Option[Anchor]]()
                    Certificate.UpdateDRepCert(drepCredential, anchor)
                case _ =>
                    r.validationFailure(s"Unknown certificate type: $tag")

    /** Helper to read a Set from CBOR */
    private def readSet[A](r: Reader)(using decoder: Decoder[A]): Set[A] =
        // Check for indefinite array tag (258)
        if r.dataItem() == DataItem.Tag then
            val tag = r.readTag()
            if tag.code != 258 then
                r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            r.read[Set[A]]()
        else r.read[Set[A]]()

    import Doc.*
    import Pretty.{formatAda, inParens}

    /** Pretty prints Certificate showing the certificate type */
    given Pretty[Certificate] with
        def pretty(a: Certificate, style: Style): Doc =
            def credDoc(c: Credential) = Pretty[Credential].pretty(c, style)
            def drepDoc(d: DRep) = Pretty[DRep].pretty(d, style)
            def anchorDoc(a: Anchor) = Pretty[Anchor].pretty(a, style)
            def depositDoc(c: Coin) = text(s", deposit=${formatAda(c.value)}")
            def refundDoc(c: Coin) = text(s", refund=${formatAda(c.value)}")
            def poolDoc(p: PoolKeyHash) = text(", pool=") + text(p.toHex)
            def optAnchor(opt: Option[Anchor]) =
                opt.fold(Doc.empty)(a => text(", anchor=") + anchorDoc(a))

            a match
                case Certificate.StakeDelegation(cred, pool) =>
                    text("StakeDelegation") + inParens(credDoc(cred) + poolDoc(pool))
                case Certificate.PoolRegistration(op, _, pledge, cost, _, _, _, _, _) =>
                    text("PoolRegistration") + inParens(
                      text("operator=") + text(op.toHex) +
                          text(s", pledge=${formatAda(pledge.value)}") +
                          text(s", cost=${formatAda(cost.value)}")
                    )
                case Certificate.PoolRetirement(pool, epoch) =>
                    text("PoolRetirement") + inParens(
                      text("pool=") + text(pool.toHex) + text(s", epoch=$epoch")
                    )
                case Certificate.RegCert(cred, coin) =>
                    text("RegCert") + inParens(credDoc(cred) + coin.fold(Doc.empty)(depositDoc))
                case Certificate.UnregCert(cred, coin) =>
                    text("UnregCert") + inParens(credDoc(cred) + coin.fold(Doc.empty)(refundDoc))
                case Certificate.VoteDelegCert(cred, drep) =>
                    text("VoteDelegCert") + inParens(credDoc(cred) + text(", ") + drepDoc(drep))
                case Certificate.StakeVoteDelegCert(cred, pool, drep) =>
                    text("StakeVoteDelegCert") + inParens(
                      credDoc(cred) + poolDoc(pool) + text(", ") + drepDoc(drep)
                    )
                case Certificate.StakeRegDelegCert(cred, pool, coin) =>
                    text("StakeRegDelegCert") + inParens(
                      credDoc(cred) + poolDoc(pool) + depositDoc(coin)
                    )
                case Certificate.VoteRegDelegCert(cred, drep, coin) =>
                    text("VoteRegDelegCert") + inParens(
                      credDoc(cred) + text(", ") + drepDoc(drep) + depositDoc(coin)
                    )
                case Certificate.StakeVoteRegDelegCert(cred, pool, drep, coin) =>
                    text("StakeVoteRegDelegCert") + inParens(
                      credDoc(cred) + poolDoc(pool) + text(", ") + drepDoc(drep) + depositDoc(coin)
                    )
                case Certificate.AuthCommitteeHotCert(cold, hot) =>
                    text("AuthCommitteeHotCert") + inParens(
                      text("cold=") + credDoc(cold) + text(", hot=") + credDoc(hot)
                    )
                case Certificate.ResignCommitteeColdCert(cold, anchor) =>
                    text("ResignCommitteeColdCert") + inParens(credDoc(cold) + optAnchor(anchor))
                case Certificate.RegDRepCert(cred, coin, anchor) =>
                    text("RegDRepCert") + inParens(
                      credDoc(cred) + depositDoc(coin) + optAnchor(anchor)
                    )
                case Certificate.UnregDRepCert(cred, coin) =>
                    text("UnregDRepCert") + inParens(credDoc(cred) + refundDoc(coin))
                case Certificate.UpdateDRepCert(cred, anchor) =>
                    text("UpdateDRepCert") + inParens(credDoc(cred) + optAnchor(anchor))
}
