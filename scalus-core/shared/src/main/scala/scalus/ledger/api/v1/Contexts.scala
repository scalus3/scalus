package scalus.ledger.api.v1

import scalus.Compile
import scalus.builtin.{Builtins, ByteString, Data, FromData, ToData}
import scalus.builtin.Builtins.*
import scalus.prelude.{<=>, ===, Eq, List, Option, Ord, Order}
import scalus.builtin.ByteString.*
import scalus.ledger.api.v1.IntervalBoundType.Finite

type Hash = ByteString
type ValidatorHash = Hash
type Datum = Data
type DatumHash = Hash
type Redeemer = Data
type ScriptHash = Hash
type RedeemerHash = Hash
@deprecated("Use PolicyId instead", "0.12.0")
type CurrencySymbol = ByteString
type PolicyId = ByteString
type TokenName = ByteString
type PosixTime = BigInt
type PosixTimeRange = Interval

type Closure = Boolean

/** A type to represent the bounds of an interval.
  *   - `NegInf` represents negative infinity
  *   - `Finite(time)` represents a finite bound
  *   - `PosInf` represents positive infinity
  */
enum IntervalBoundType:
    case NegInf
    case Finite(time: PosixTime)
    case PosInf

@Compile
object IntervalBoundType {

    given Eq[IntervalBoundType] = (x: IntervalBoundType, y: IntervalBoundType) =>
        x match
            case NegInf =>
                y match
                    case NegInf => true
                    case _      => false
            case Finite(a) =>
                y match
                    case Finite(b) => a === b
                    case _         => false
            case PosInf =>
                y match
                    case PosInf => true
                    case _      => false

    given Ord[IntervalBoundType] = (x: IntervalBoundType, y: IntervalBoundType) =>
        x match
            case NegInf =>
                y match
                    case NegInf => Order.Equal
                    case _      => Order.Less
            case Finite(a) =>
                y match
                    case NegInf    => Order.Greater
                    case Finite(b) => a <=> b
                    case PosInf    => Order.Less
            case PosInf =>
                y match
                    case PosInf => Order.Equal
                    case _      => Order.Greater

    given ToData[IntervalBoundType] = ToData.derived

    given FromData[IntervalBoundType] = FromData.derived

}

/** An interval bound, either inclusive or exclusive.
  * @param boundType
  *   the type of the bound
  * @param isInclusive
  *   whether the bound is inclusive or not
  */
case class IntervalBound(boundType: IntervalBoundType, isInclusive: Closure)

@Compile
object IntervalBound:

    given Eq[IntervalBound] = (x: IntervalBound, y: IntervalBound) =>
        (x.boundType === y.boundType) && (x.isInclusive === y.isInclusive)

    given Ord[IntervalBound] = (x: IntervalBound, y: IntervalBound) =>
        (x.boundType <=> y.boundType) ifEqualThen (x.isInclusive <=> y.isInclusive)

    given ToData[IntervalBound] = ToData.derived

    given FromData[IntervalBound] = FromData.derived

    /** Inclusive -∞ interval bound */
    val negInf: IntervalBound = IntervalBound(IntervalBoundType.NegInf, true)

    /** Inclusive +∞ interval bound */
    val posInf: IntervalBound = IntervalBound(IntervalBoundType.PosInf, true)

    /** Create a finite inclusive interval bound */
    def finiteInclusive(time: PosixTime): IntervalBound =
        IntervalBound(IntervalBoundType.Finite(time), true)

    /** Create a finite exclusive interval bound */
    def finiteExclusive(time: PosixTime): IntervalBound =
        IntervalBound(IntervalBoundType.Finite(time), false)

    /** Returns the minimum of two interval bounds. If the bounds are equal, returns the left-hand
      * side bound.
      */
    def min(lhs: IntervalBound, rhs: IntervalBound): IntervalBound =
        lhs <=> rhs match
            case Order.Less    => lhs
            case Order.Equal   => lhs
            case Order.Greater => rhs

    /** Returns the maximum of two interval bounds. If the bounds are equal, returns the left-hand
      * side bound.
      */
    def max(lhs: IntervalBound, rhs: IntervalBound): IntervalBound =
        lhs <=> rhs match
            case Order.Less    => rhs
            case Order.Equal   => lhs
            case Order.Greater => lhs

end IntervalBound

/** A type to represent time intervals.
  * @param from
  *   the lower bound of the interval
  * @param to
  *   the upper bound of the interval
  */
case class Interval(from: IntervalBound, to: IntervalBound)

@Compile
object Interval:

    given Eq[Interval] = (x: Interval, y: Interval) => (x.from === y.from) && (x.to === y.to)

    given Ord[Interval] = (x: Interval, y: Interval) =>
        (x.from <=> y.from) ifEqualThen (x.to <=> y.to)

    given ToData[Interval] = ToData.derived

    given FromData[Interval] = FromData.derived

    /** Inclusive -∞ to +∞ interval */
    val always: Interval = Interval(IntervalBound.negInf, IntervalBound.posInf)

    /** An empty interval that contains no values, ∄ posixTime ∣ never.contains(posixTime)
      * Invariants expressed in IntervalTest should hold regardless of the implementation
      */
    val never: Interval = Interval(IntervalBound.posInf, IntervalBound.negInf)

    /** Creates an interval that includes all values greater than the given bound. i.e
      * [lower_bound,+∞)
      */
    def after(time: PosixTime): Interval =
        Interval(IntervalBound.finiteInclusive(time), IntervalBound.posInf)

    /** Creates an interval that includes all values after (and not including) the given bound. i.e
      * (lower_bound,+∞)
      */
    def entirelyAfter(time: PosixTime): Interval =
        Interval(IntervalBound.finiteExclusive(time), IntervalBound.posInf)

    /** Creates an interval that includes all values less than the given bound. i.e (-∞,upper_bound]
      */
    def before(time: PosixTime): Interval =
        Interval(IntervalBound.negInf, IntervalBound.finiteInclusive(time))

    /** Creates an interval that includes all values before (and not including) the given bound. i.e
      * (-∞,upper_bound)
      */
    def entirelyBefore(time: PosixTime): Interval =
        Interval(IntervalBound.negInf, IntervalBound.finiteExclusive(time))

    /** Create an interval that includes all values between two bounds, including the bounds. i.e
      * [lower_bound, upper_bound]
      */
    def between(from: PosixTime, to: PosixTime): Interval =
        Interval(IntervalBound.finiteInclusive(from), IntervalBound.finiteInclusive(to))

    /** Create an interval that includes all values between two bounds, excluding the bounds. i.e
      * (lower_bound, upper_bound)
      */
    def entirelyBetween(from: PosixTime, to: PosixTime): Interval =
        Interval(IntervalBound.finiteExclusive(from), IntervalBound.finiteExclusive(to))

    /** Returns the hull of two intervals, i.e., the smallest interval that contains both input
      * intervals, if any.
      */
    def hull(lhs: Interval, rhs: Interval): Interval =
        Interval(
          IntervalBound.min(lhs.from, rhs.from),
          IntervalBound.max(lhs.to, rhs.to)
        )

    /** Returns the intersection of two intervals, i.e., the largest interval that is contained in
      * both input intervals, if any.
      */
    def intersection(lhs: Interval, rhs: Interval): Interval =
        Interval(
          IntervalBound.max(lhs.from, rhs.from),
          IntervalBound.min(lhs.to, rhs.to)
        )

    extension (self: Interval)

        /** Checks if a given time is contained in the interval. Returns true if the time is within
          * the bounds of the interval, taking into account whether the bounds are inclusive or
          * exclusive.
          */
        def contains(time: PosixTime): Boolean = {
            val aboveFrom = {
                self.from.boundType match {
                    case IntervalBoundType.Finite(from) =>
                        if self.from.isInclusive then time >= from else time > from
                    case IntervalBoundType.NegInf => true
                    case IntervalBoundType.PosInf => false
                }
            }
            val belowTo = {
                self.to.boundType match {
                    case IntervalBoundType.Finite(to) =>
                        if self.to.isInclusive then time <= to else time < to
                    case IntervalBoundType.NegInf => false
                    case IntervalBoundType.PosInf => true
                }
            }

            aboveFrom && belowTo
        }

        /** Checks if an interval is entirely after a given time. Returns true if all values in the
          * interval are greater than the given time.
          */
        def isEntirelyAfter(time: PosixTime): Boolean =
            self.from.boundType match
                case IntervalBoundType.Finite(fromTime) =>
                    if self.from.isInclusive then time < fromTime
                    else time <= fromTime

                case _ => false

        /** Checks if an interval is entirely before a given time. Returns true if all values in the
          * interval are less than the given time.
          */
        def isEntirelyBefore(time: PosixTime): Boolean =
            self.to.boundType match
                case IntervalBoundType.Finite(toTime) =>
                    if self.to.isInclusive then toTime < time
                    else toTime <= time

                case _ => false

        /** Checks if an interval is entirely between two given times. Returns true if all values in
          * the interval are between the given times.
          */
        def isEntirelyBetween(after: PosixTime, before: PosixTime): Boolean =
            isEntirelyAfter(after) && isEntirelyBefore(before)

        /** Checks if the interval is never, i.e., contains no values. Invariants expressed in
          * IntervalTest should hold regardless of the implementation.
          */
        def isNever: Boolean =
            self.from <=> self.to match
                case Order.Greater => true
                case Order.Equal   => !(self.from.isInclusive && self.to.isInclusive)
                case Order.Less =>
                    val isOpenInterval = !self.from.isInclusive && !self.to.isInclusive
                    if isOpenInterval then
                        self.from.boundType match
                            case IntervalBoundType.Finite(fromTime) =>
                                self.to.boundType match
                                    case IntervalBoundType.Finite(toTime) =>
                                        fromTime + 1 === toTime
                                    case _ => false
                            case _ => false
                    else false

        /** Checks if the interval is non never, i.e., contains at least one value. Invariants
          * expressed in IntervalTest should hold regardless of the implementation.
          */
        inline def nonNever: Boolean = !self.isNever

    end extension

end Interval

enum DCert:
    case DelegRegKey(cred: StakingCredential)
    case DelegDeRegKey(cred: StakingCredential)
    case DelegDelegate(cred: StakingCredential, delegatee: PubKeyHash)
    case PoolRegister(poolId: PubKeyHash, vrf: PubKeyHash)
    case PoolRetire(poolId: PubKeyHash, epoch: BigInt)
    case Genesis
    case Mir

@Compile
object DCert {

    given Eq[DCert] = (x: DCert, y: DCert) =>
        x match
            case DCert.DelegRegKey(cred) =>
                y match
                    case DCert.DelegRegKey(cred) => cred === cred
                    case _                       => false

            case DCert.DelegDeRegKey(cred) =>
                y match
                    case DCert.DelegDeRegKey(cred) => cred === cred
                    case _                         => false
            case DCert.DelegDelegate(cred, delegatee) =>
                y match
                    case DCert.DelegDelegate(cred, delegatee) =>
                        cred === cred && delegatee === delegatee
                    case _ => false
            case DCert.PoolRegister(poolId, vrf) =>
                y match
                    case DCert.PoolRegister(poolId, vrf) => poolId === poolId && vrf === vrf
                    case _                               => false
            case DCert.PoolRetire(poolId, epoch) =>
                y match
                    case DCert.PoolRetire(poolId, epoch) => poolId === poolId && epoch === epoch
                    case _                               => false
            case DCert.Genesis =>
                y match
                    case DCert.Genesis => true
                    case _             => false
            case DCert.Mir =>
                y match
                    case DCert.Mir => true
                    case _         => false

    given Ord[DCert] = (x: DCert, y: DCert) =>
        x match
            case DCert.DelegRegKey(cred1) =>
                y match
                    case DCert.DelegRegKey(cred2) => cred1 <=> cred2
                    case _                        => Order.Less

            case DCert.DelegDeRegKey(cred1) =>
                y match
                    case DCert.DelegRegKey(_)       => Order.Greater
                    case DCert.DelegDeRegKey(cred2) => cred1 <=> cred2
                    case _                          => Order.Less

            case DCert.DelegDelegate(cred1, del1) =>
                y match
                    case DCert.DelegRegKey(_)   => Order.Greater
                    case DCert.DelegDeRegKey(_) => Order.Greater
                    case DCert.DelegDelegate(cred2, del2) =>
                        (cred1 <=> cred2) ifEqualThen (del1 <=> del2)
                    case _ => Order.Less

            case DCert.PoolRegister(id1, vrf1) =>
                y match
                    case DCert.DelegRegKey(_)      => Order.Greater
                    case DCert.DelegDeRegKey(_)    => Order.Greater
                    case DCert.DelegDelegate(_, _) => Order.Greater
                    case DCert.PoolRegister(id2, vrf2) =>
                        (id1 <=> id2) ifEqualThen (vrf1 <=> vrf2)
                    case _ => Order.Less

            case DCert.PoolRetire(id1, epoch1) =>
                y match
                    case DCert.DelegRegKey(_)      => Order.Greater
                    case DCert.DelegDeRegKey(_)    => Order.Greater
                    case DCert.DelegDelegate(_, _) => Order.Greater
                    case DCert.PoolRegister(_, _)  => Order.Greater
                    case DCert.PoolRetire(id2, epoch2) =>
                        (id1 <=> id2) ifEqualThen (epoch1 <=> epoch2)
                    case _ => Order.Less

            case DCert.Genesis =>
                y match
                    case DCert.DelegRegKey(_)      => Order.Greater
                    case DCert.DelegDeRegKey(_)    => Order.Greater
                    case DCert.DelegDelegate(_, _) => Order.Greater
                    case DCert.PoolRegister(_, _)  => Order.Greater
                    case DCert.PoolRetire(_, _)    => Order.Greater
                    case DCert.Genesis             => Order.Equal
                    case DCert.Mir                 => Order.Less

            case DCert.Mir =>
                y match
                    case DCert.Mir => Order.Equal
                    case _         => Order.Greater

    given ToData[DCert] = ToData.derived

    given FromData[DCert] = FromData.derived

}

case class TxId(hash: Hash):
    override def toString = s"txid#${hash.toHex}"

@Compile
object TxId:

    given Eq[TxId] = (a: TxId, b: TxId) => a.hash === b.hash
    given Ord[TxId] = (a: TxId, b: TxId) => a.hash <=> b.hash

    given ToData[TxId] = ToData.derived
    given FromData[TxId] = FromData.derived

    extension (sc: StringContext)
        inline def txid(args: Any*): TxId =
            TxId(sc.hex(args*))

end TxId

case class TxOutRef(id: TxId, idx: BigInt)

@Compile
object TxOutRef {

    given Eq[TxOutRef] = (a: TxOutRef, b: TxOutRef) => a.id === b.id && a.idx === b.idx

    given Ord[TxOutRef] = (a: TxOutRef, b: TxOutRef) =>
        (a.id <=> b.id) ifEqualThen (a.idx <=> b.idx)

    given ToData[TxOutRef] = ToData.derived

    given FromData[TxOutRef] = FromData.derived

}

case class PubKeyHash(hash: Hash) {
    override def toString = s"pkh#${hash.toHex}"
}

@Compile
object PubKeyHash {

    given Eq[PubKeyHash] = (a: PubKeyHash, b: PubKeyHash) => a.hash === b.hash

    given Ord[PubKeyHash] = (a: PubKeyHash, b: PubKeyHash) => a.hash <=> b.hash

    given ToData[PubKeyHash] = (a: PubKeyHash) => summon[ToData[ByteString]](a.hash)

    given FromData[PubKeyHash] = (d: Data) => PubKeyHash(unBData(d))

    extension (sc: StringContext)
        inline def pkh(args: Any*): PubKeyHash =
            PubKeyHash(sc.hex(args*))
}

enum Credential:
    case PubKeyCredential(hash: PubKeyHash)
    case ScriptCredential(hash: ValidatorHash)

@Compile
object Credential {

    given Eq[Credential] = (a: Credential, b: Credential) =>
        a match
            case Credential.PubKeyCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(hash2) => hash.hash === hash2.hash
                    case Credential.ScriptCredential(hash)  => false
            case Credential.ScriptCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(hash2) => false
                    case Credential.ScriptCredential(hash2) => hash === hash2

    given Ord[Credential] = (a: Credential, b: Credential) =>
        a match
            case Credential.PubKeyCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(hash2) => hash <=> hash2
                    case Credential.ScriptCredential(_)     => Order.Less
            case Credential.ScriptCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(_)     => Order.Greater
                    case Credential.ScriptCredential(hash2) => hash <=> hash2

    given FromData[Credential] = FromData.derived

    given ToData[Credential] = ToData.derived

    extension (self: Credential) {
        def pubKeyOption: Option[PubKeyHash] = self match
            case Credential.PubKeyCredential(hash) => Option.Some(hash)
            case _                                 => Option.None

        def scriptOption: Option[ValidatorHash] = self match
            case Credential.ScriptCredential(hash) => Option.Some(hash)
            case _                                 => Option.None
    }
}

enum StakingCredential:
    case StakingHash(cred: Credential)
    case StakingPtr(a: BigInt, b: BigInt, c: BigInt)

@Compile
object StakingCredential {
    given Eq[StakingCredential] = (lhs: StakingCredential, rhs: StakingCredential) =>
        lhs match
            case StakingCredential.StakingHash(cred) =>
                rhs match
                    case StakingCredential.StakingHash(cred2)  => cred === cred2
                    case StakingCredential.StakingPtr(a, b, c) => false
            case StakingCredential.StakingPtr(a, b, c) =>
                rhs match
                    case StakingCredential.StakingHash(cred2) => false
                    case StakingCredential.StakingPtr(a2, b2, c2) =>
                        a === a2 && b === b2 && c === c2

    given Ord[StakingCredential] = (lhs: StakingCredential, rhs: StakingCredential) =>
        lhs match
            case StakingCredential.StakingHash(cred) =>
                rhs match
                    case StakingCredential.StakingHash(cred2)  => cred <=> cred2
                    case StakingCredential.StakingPtr(_, _, _) => Order.Less
            case StakingCredential.StakingPtr(a, b, c) =>
                rhs match
                    case StakingCredential.StakingHash(_) => Order.Greater
                    case StakingCredential.StakingPtr(a2, b2, c2) =>
                        (a <=> a2) ifEqualThen (b <=> b2) ifEqualThen (c <=> c2)

    given FromData[StakingCredential] = FromData.derived

    given ToData[StakingCredential] = ToData.derived
}

case class Address(
    credential: Credential,
    stakingCredential: Option[StakingCredential]
)

@Compile
object Address {

    given Eq[Address] = (a: Address, b: Address) =>
        a.credential === b.credential && a.stakingCredential === b.stakingCredential

    given Ord[Address] = (a: Address, b: Address) =>
        (a.credential <=> b.credential) ifEqualThen (a.stakingCredential <=> b.stakingCredential)

    given ToData[Address] = ToData.derived

    given FromData[Address] = FromData.derived

    /** Smart-constructor for an [[scalus.ledger.api.v1.Address]] from a
      * [[scalus.ledger.api.v1.Credential]]. The resulting address has no delegation rights
      * whatsoever.
      */
    inline def fromCredential(credential: Credential): Address =
        Address(credential, Option.None)

    /** Smart-constructor for an [[scalus.ledger.api.v1.Address]] from a
      * [[scalus.ledger.api.v1.Credential.ScriptHash]] hash. The resulting address has no delegation
      * rights whatsoever.
      */
    inline def fromScriptHash(script: ScriptHash): Address =
        fromCredential(Credential.ScriptCredential(script))

    /** Smart-constructor for an [[scalus.ledger.api.v1.Address]] from a
      * [[scalus.ledger.api.v1.Credential.PubKeyHash]] hash. The resulting address has no delegation
      * rights whatsoever.
      */
    inline def fromPubKeyHash(pubKey: PubKeyHash): Address =
        fromCredential(Credential.PubKeyCredential(pubKey))

}

case class TxOut(address: Address, value: Value, datumHash: Option[DatumHash]) {

    override def toString: String = {
        s"""TxOut(
           |  address: $address,
           |  value: ${Value.debugToString(value)},
           |  datumHash: $datumHash
           |)""".stripMargin
    }

}

@Compile
object TxOut {
    given Eq[TxOut] = (a: TxOut, b: TxOut) =>
        a.address === b.address && a.value === b.value && a.datumHash === b.datumHash

    given Ord[TxOut] = (a: TxOut, b: TxOut) =>
        given Ord[Value] = Value.valueOrd
        (a.address <=> b.address) ifEqualThen (a.value <=> b.value) ifEqualThen (a.datumHash <=> b.datumHash)

    given ToData[TxOut] = ToData.derived

    given FromData[TxOut] = FromData.derived

}

case class TxInInfo(
    outRef: TxOutRef,
    resolved: TxOut
)

@Compile
object TxInInfo {
    given Eq[TxInInfo] = (a: TxInInfo, b: TxInInfo) =>
        a.outRef === b.outRef && a.resolved === b.resolved

    given Ord[TxInInfo] = (a: TxInInfo, b: TxInInfo) =>
        (a.outRef <=> b.outRef) ifEqualThen (a.resolved <=> b.resolved)

    given ToData[TxInInfo] = ToData.derived

    given FromData[TxInInfo] = FromData.derived

}

case class TxInfo(
    inputs: List[TxInInfo],
    outputs: List[TxOut],
    fee: Value,
    mint: Value,
    dcert: List[DCert],
    withdrawals: List[(StakingCredential, BigInt)],
    validRange: PosixTimeRange,
    signatories: List[PubKeyHash],
    data: List[(DatumHash, Datum)],
    id: TxId
) {

    override def toString: String = {
        s"""TxInfo(
           |  inputs: ${inputs.asScala.mkString("[", ", ", "]")},
           |  outputs: ${outputs.asScala.mkString("[", ", ", "]")},
           |  fee: ${Value.debugToString(fee)},
           |  mint: ${Value.debugToString(mint)},
           |  dcert: $dcert,
           |  withdrawals: $withdrawals,
           |  validRange: $validRange,
           |  signatories: $signatories,
           |  data: $data,
           |  id: $id
           |)""".stripMargin
    }

}

@Compile
object TxInfo {
    val placeholder: TxInfo = TxInfo(
      inputs = List.empty,
      outputs = List.empty,
      fee = Value.zero,
      mint = Value.zero,
      dcert = List.empty,
      withdrawals = List.empty,
      validRange = Interval.always,
      signatories = List.empty,
      data = List.empty,
      id = TxId(hex"0000000000000000000000000000000000000000000000000000000000000000")
    )

    given Eq[TxInfo] = (x: TxInfo, y: TxInfo) =>
        x.inputs === y.inputs &&
            x.outputs === y.outputs &&
            x.fee === y.fee &&
            x.mint === y.mint &&
            x.dcert === y.dcert &&
            x.withdrawals === y.withdrawals &&
            x.validRange === y.validRange &&
            x.signatories === y.signatories &&
            x.data === y.data &&
            x.id === y.id

    given Ord[TxInfo] = (x: TxInfo, y: TxInfo) =>
        given Ord[Value] = Value.valueOrd
        (x.inputs <=> y.inputs) ifEqualThen
            (x.outputs <=> y.outputs) ifEqualThen
            (x.fee <=> y.fee) ifEqualThen
            (x.mint <=> y.mint) ifEqualThen
            (x.dcert <=> y.dcert) ifEqualThen
            (x.withdrawals <=> y.withdrawals) ifEqualThen
            (x.validRange <=> y.validRange) ifEqualThen
            (x.signatories <=> y.signatories) ifEqualThen
            (x.data <=> y.data) ifEqualThen
            (x.id <=> y.id)

    given ToData[TxInfo] = ToData.derived
    given FromData[TxInfo] = FromData.derived

    extension (self: TxInfo) {

        /** Finds an input by its out reference.
          *
          * @param outRef
          *   The output reference to find.
          * @return
          *   An `Option` containing the found input, or `None` if not found.
          */
        def findOwnInput(outRef: TxOutRef): Option[TxInInfo] = {
            Utils.findInput(self.inputs, outRef)
        }

        /** Finds all outputs that match a given script hash.
          *
          * @param scriptHash
          *   The script hash to match against the outputs' addresses.
          * @return
          *   A list of outputs that match the script hash.
          */
        def findOwnScriptOutputs(scriptHash: ValidatorHash): List[TxOut] = {
            Utils.findScriptOutputs(self.outputs, scriptHash)
        }
    }
}

enum ScriptPurpose:
    case Minting(curSymbol: ByteString)
    case Spending(txOutRef: TxOutRef)
    case Rewarding(stakingCred: StakingCredential)
    case Certifying(cert: DCert)

@Compile
object ScriptPurpose {

    given Eq[ScriptPurpose] = (x: ScriptPurpose, y: ScriptPurpose) =>
        x match
            case ScriptPurpose.Minting(curSymbol) =>
                y match
                    case ScriptPurpose.Minting(curSymbol) => curSymbol === curSymbol
                    case _                                => false
            case ScriptPurpose.Spending(txOutRef) =>
                y match
                    case ScriptPurpose.Spending(txOutRef) => txOutRef === txOutRef
                    case _                                => false
            case ScriptPurpose.Rewarding(stakingCred) =>
                y match
                    case ScriptPurpose.Rewarding(stakingCred) => stakingCred === stakingCred
                    case _                                    => false
            case ScriptPurpose.Certifying(cert) =>
                y match
                    case ScriptPurpose.Certifying(cert) => cert === cert
                    case _                              => false

    given Ord[ScriptPurpose] = (x: ScriptPurpose, y: ScriptPurpose) =>
        x match
            case ScriptPurpose.Minting(sym1) =>
                y match
                    case ScriptPurpose.Minting(sym2) => sym1 <=> sym2
                    case _                           => Order.Less

            case ScriptPurpose.Spending(ref1) =>
                y match
                    case ScriptPurpose.Minting(_)     => Order.Greater
                    case ScriptPurpose.Spending(ref2) => ref1 <=> ref2
                    case _                            => Order.Less

            case ScriptPurpose.Rewarding(cred1) =>
                y match
                    case ScriptPurpose.Minting(_)       => Order.Greater
                    case ScriptPurpose.Spending(_)      => Order.Greater
                    case ScriptPurpose.Rewarding(cred2) => cred1 <=> cred2
                    case _                              => Order.Less

            case ScriptPurpose.Certifying(cert1) =>
                y match
                    case ScriptPurpose.Certifying(cert2) => cert1 <=> cert2
                    case _                               => Order.Greater

    given ToData[ScriptPurpose] = ToData.derived

    given FromData[ScriptPurpose] = FromData.derived

}

case class ScriptContext(txInfo: TxInfo, purpose: ScriptPurpose)

@Compile
object ScriptContext:
    given Eq[ScriptContext] = (x: ScriptContext, y: ScriptContext) =>
        x.txInfo === y.txInfo && x.purpose === y.purpose

    given Ord[ScriptContext] = (x: ScriptContext, y: ScriptContext) =>
        (x.txInfo <=> y.txInfo) ifEqualThen (x.purpose <=> y.purpose)

    given FromData[ScriptContext] = FromData.derived

    given ToData[ScriptContext] = ToData.derived

end ScriptContext

@Compile
object Utils {

    /** Finds an input in the list of inputs by its out reference.
      *
      * @param inputs
      *   The list of inputs to search in.
      * @param outRef
      *   The output reference to find.
      * @return
      *   An `Option` containing the found input, or `None` if not found.
      */
    def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
        inputs.find(_.outRef === outRef)
    }

    /** Finds all outputs that match a given script hash.
      *
      * @param outputs
      *   The list of outputs to search in.
      * @param scriptHash
      *   The script hash to match against the outputs' addresses.
      * @return
      *   A list of outputs that match the script hash.
      */
    def findScriptOutputs(outputs: List[TxOut], scriptHash: ValidatorHash): List[TxOut] = {
        outputs.filter { output =>
            output.address.credential match
                case Credential.ScriptCredential(hash) => hash === scriptHash
                case _                                 => false
        }
    }
}
