package scalus.cardano.ledger

import io.bullet.borer.Encoder
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.uplc.builtin
import scalus.uplc.builtin.Data.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.testing.ArbitraryDerivation.autoDerived

import scala.collection.immutable
import scala.collection.immutable.SortedMap

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends scalus.cardano.address.ArbitraryInstances {

    /** `[gen.cover.optional-empty]`, `[gen.cover.no-gen-option]`: `None` in 50 % of samples. */
    def genOptional[A](gen: Gen[A]): Gen[Option[A]] =
        Gen.frequency(1 -> Gen.const(None), 1 -> gen.map(Some(_)))

    /** `Option` for every derived type of this file, `None` in 50 % of samples.
      *
      * The import of `autoDerived` makes it a candidate for every type, so without this given an
      * `Option` field resolves to a derived `Option`: a sum type, which gives `Gen.fail` at size 0
      * (`[gen.size.derived-fallback]`). The derived `Option` also gave 50 % `None`, so the shares
      * stay; the values for a seed change.
      */
    given arbitraryOption[A](using a: Arbitrary[A]): Arbitrary[Option[A]] =
        Arbitrary(genOptional(a.arbitrary))

    def genMapOfSizeFromArbitrary[A: Arbitrary, B: Arbitrary](
        from: Int,
        to: Int
    ): Gen[immutable.Map[A, B]] = {
        for
            size <- Gen.choose(from, to)
            result <- Gen.mapOfN(
              size,
              for
                  key <- arbitrary[A]
                  value <- arbitrary[B]
              yield (key, value)
            )
        yield result
    }

    def genVectorOfSizeFromArbitrary[A: Arbitrary](
        from: Int,
        to: Int
    ): Gen[immutable.IndexedSeq[A]] = {
        for
            size <- Gen.choose(from, to)
            result <- Gen.containerOfN[Vector, A](size, arbitrary[A])
        yield result
    }

    def genSetOfSizeFromArbitrary[A: Arbitrary](
        from: Int,
        to: Int
    ): Gen[immutable.Set[A]] = {
        for
            size <- Gen.choose(from, to)
            result <- Gen.containerOfN[Set, A](size, arbitrary[A])
        yield result
    }

    given [HF: HashSize, Purpose]: Arbitrary[Hash[HF, Purpose]] = Arbitrary {
        val size = summon[HashSize[HF]].size
        genByteStringOfN(size).map(Hash[HF, Purpose].apply)
    }

    given Arbitrary[Anchor] = Arbitrary {
        for
            url <- Gen
                .choose(0, 128)
                .flatMap(Gen.listOfN(_, Gen.alphaNumChar))
                .map(_.mkString)
            dataHash <- arbitrary[DataHash]
        yield Anchor(url, dataHash)
    }

    given Arbitrary[Credential] = Arbitrary {
        Gen.oneOf(
          arbitrary[AddrKeyHash].map(Credential.KeyHash.apply),
          arbitrary[ScriptHash].map(Credential.ScriptHash.apply)
        )
    }

    /** The max supply of lovelace, 45 × 10^9 ADA. A constant of the protocol, not a protocol
      * parameter, so `[gen.size.params-source]` does not apply.
      */
    final val MaxLovelace = 45_000_000_000_000_000L

    /** The generator behind `Arbitrary[Coin]` (spec `[gen.cover.three-parts]` of
      * docs/internal/test-refactor/d5-generators.md): 50 % 1 to 100 ADA, 25 % edges, 25 % any value
      * up to the max supply with a uniform bit length.
      *
      * The edges are 0, 1, 999 999 and 1 000 000 lovelace (the ADA boundary), 2^53 (the largest
      * exact `Double`) and the max supply.
      */
    def genCoin: Gen[Coin] =
        Gen.frequency(
          2 -> Gen.choose(1_000_000L, 100_000_000L),
          1 -> Gen.oneOf(0L, 1L, 999_999L, 1_000_000L, 1L << 53, MaxLovelace),
          1 -> genMagnitude(0L, MaxLovelace)
        ).map(Coin.apply)

    given Arbitrary[Coin] = Arbitrary(genCoin)
    given Arbitrary[AssetName] = Arbitrary(
      Gen.choose(0, 32).flatMap(genByteStringOfN).map(AssetName.apply)
    )

    given Arbitrary[Language] = autoDerived
    given Arbitrary[AddressBytes] = Arbitrary(arbitrary[ByteString].map(AddressBytes.apply))

    /** The generator behind `Arbitrary[Slot]` (`[gen.cover.three-parts]`): 50 % 0 to 2 × 10^8 (a
      * mainnet slot is about 1.7 × 10^8), 25 % the edges 0, 1, 2^32 and `Long.MaxValue`, 25 % any
      * slot with a uniform bit length (`[gen.magnitude.users]`).
      */
    def genSlot: Gen[Slot] =
        Gen.frequency(
          2 -> Gen.choose(0L, 200_000_000L),
          1 -> Gen.oneOf(0L, 1L, 1L << 32, Long.MaxValue),
          1 -> genMagnitude(0L, Long.MaxValue)
        ).map(Slot.apply)

    given Arbitrary[Slot] = Arbitrary(genSlot)

    private lazy val maxTxExUnits = CardanoInfo.mainnet.protocolParams.maxTxExecutionUnits

    /** The generator behind `Arbitrary[ExUnits]` (`[gen.cover.three-parts]`): 50 % the units of a
      * real script (10^4 to 10^7 memory, 10^6 to 10^9 steps), 25 % the edges 0, 1, the
      * `maxTxExecutionUnits` of the parameter snapshot (`[gen.size.params-source]`) and one unit
      * above it, 25 % any units with a uniform bit length. Before D5 both fields were at most 1
      * 000.
      */
    def genExUnits: Gen[ExUnits] = {
        val max = maxTxExUnits
        val real =
            for
                mem <- Gen.choose(10_000L, 10_000_000L)
                steps <- Gen.choose(1_000_000L, 1_000_000_000L)
            yield ExUnits(mem, steps)
        val edges = Gen.oneOf(
          ExUnits(0, 0),
          ExUnits(1, 1),
          max,
          ExUnits(max.memory + 1, max.steps + 1)
        )
        val wide = Gen.zip(genMagnitude(0L, Long.MaxValue), genMagnitude(0L, Long.MaxValue))
        Gen.frequency(2 -> real, 1 -> edges, 1 -> wide.map(ExUnits(_, _)))
    }

    given Arbitrary[ExUnits] = Arbitrary(genExUnits)

    /** The units of one script in a transaction that a ledger rule accepts: 0 to the
      * `maxTxExecutionUnits` of the parameter snapshot (`[gen.size.params-source]`). A rule test
      * takes it instead of `Arbitrary[ExUnits]`, whose wide part reaches `Long.MaxValue`.
      */
    def genScriptExUnits: Gen[ExUnits] = {
        val max = maxTxExUnits
        Gen.zip(Gen.choose(0L, max.memory), Gen.choose(0L, max.steps)).map(ExUnits(_, _))
    }

    given Arbitrary[ExUnitPrices] = autoDerived

    given Arbitrary[CostModels] = {
        given Arbitrary[Int] = Arbitrary(Gen.choose(0, 255))

        given [A: Arbitrary, B: Arbitrary]: Arbitrary[immutable.Map[A, B]] = Arbitrary(
          genMapOfSizeFromArbitrary(0, 8)
        )

        given [A: Arbitrary]: Arbitrary[immutable.IndexedSeq[A]] = Arbitrary(
          genVectorOfSizeFromArbitrary(0, 8)
        )

        val result: Arbitrary[CostModels] = autoDerived
        result
    }

    given Arbitrary[Constitution] = autoDerived

    given [A: Arbitrary: Encoder]: Arbitrary[KeepRaw[A]] = Arbitrary {
        Arbitrary.arbitrary[A].map(a => KeepRaw(a))
    }

    given [A: Arbitrary: Encoder]: Arbitrary[Sized[A]] = Arbitrary {
        Arbitrary.arbitrary[A].map(a => Sized(a))
    }

    /** Generates a MultiAsset with a configurable number of policies and assets.
      *
      * Policies are generated with unique PolicyIds, and each policy contains a map of AssetNames
      * to values. The values are always positive.
      *
      * @param minPolicies
      *   Minimum number of policies to generate
      * @param maxPolicies
      *   Maximum number of policies to generate
      * @param minAssets
      *   Minimum number of assets per policy
      * @param maxAssets
      *   Maximum number of assets per policy
      *
      * @return
      *   A generator for MultiAsset instances with the specified constraints.
      */
    def genMultiAsset(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen[MultiAsset] = {
        genConfigurableMultiAsset(minPolicies, maxPolicies, minAssets, maxAssets)(
          Gen.choose(1L, Long.MaxValue)
        )
    }

    /** Generates a Mint with a configurable number of policies and assets.
      *
      * Policies are generated with unique PolicyIds, and each policy contains a map of AssetNames
      * to values. The values can be negative (for burning) or positive (for minting). Values never
      * equal to zero.
      *
      * @param minPolicies
      *   Minimum number of policies to generate
      * @param maxPolicies
      *   Maximum number of policies to generate
      * @param minAssets
      *   Minimum number of assets per policy
      * @param maxAssets
      *   Maximum number of assets per policy
      */
    def genMint(
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
    ): Gen[Mint] = {
        genConfigurableMultiAsset(minPolicies, maxPolicies, minAssets, maxAssets)(
          Gen.oneOf(Gen.choose(Long.MinValue, -1L), Gen.choose(1L, Long.MaxValue))
        ).map(Mint.apply)
    }

    private def genConfigurableMultiAsset(
        minPolicies: Int,
        maxPolicies: Int,
        minAssets: Int,
        maxAssets: Int
    )(valueGen: Gen[Long]): Gen[MultiAsset] = {
        given Arbitrary[Long] = Arbitrary(valueGen)
        given [A: Arbitrary, B: Arbitrary]: Arbitrary[immutable.Map[A, B]] = Arbitrary(
          genMapOfSizeFromArbitrary(minAssets, maxAssets)
        )

        for
            policies <- Gen.choose(minPolicies, maxPolicies)
            result: immutable.Map[PolicyId, SortedMap[AssetName, Long]] <- Gen
                .mapOfN(
                  policies,
                  for
                      policyId <- arbitrary[PolicyId]
                      assets <- arbitrary[immutable.Map[AssetName, Long]]
                  yield (policyId, assets.to(immutable.TreeMap))
                )
        yield MultiAsset(result.to(immutable.TreeMap))
    }

    given Arbitrary[MultiAsset] = Arbitrary {
        genMultiAsset(minPolicies = 0, maxPolicies = 4, minAssets = 1, maxAssets = 4)
    }

    given Arbitrary[Mint] = Arbitrary {
        genMint(minPolicies = 1, maxPolicies = 4, minAssets = 1, maxAssets = 4)
    }

    given Arbitrary[Value] = autoDerived

    // FIXME: autoDerived for DRep is not working correctly
    // assertion failed: position not set for io.bullet.borer.derivation.key # -1 of class dotty.tools.dotc.ast.Trees$Ident in <no file>
//    given Arbitrary[DRep] = autoDerived
    given Arbitrary[DRep] = Arbitrary {
        Gen.oneOf(
          arbitrary[AddrKeyHash].map(DRep.KeyHash.apply),
          arbitrary[ScriptHash].map(DRep.ScriptHash.apply),
          Gen.const(DRep.AlwaysAbstain),
          Gen.const(DRep.AlwaysNoConfidence)
        )
    }

    given Arbitrary[GovActionId] = Arbitrary {
        for
            txId <- arbitrary[TransactionHash]
            index <- Gen.choose(0, 65535)
        yield GovActionId(txId, index)
    }
    given Arbitrary[OperationalCert] = Arbitrary {
        for
            hotVKey <- genByteStringOfN(32)
            sequenceNumber <- Gen.posNum[Long]
            kesPeriod <- Gen.posNum[Long]
            sigma <- genByteStringOfN(64)
        yield OperationalCert(hotVKey, sequenceNumber, kesPeriod, sigma)
    }
    given Arbitrary[PoolMetadata] = Arbitrary {
        for
            len <- Gen.choose(0, 128)
            url <- Gen.stringOfN(len, Gen.alphaNumChar)
            hash <- arbitrary[MetadataHash]
        yield PoolMetadata(url, hash)
    }

    // FIXME: autoDerived for DatumOption is not working correctly
    given Arbitrary[DatumOption] = Arbitrary {
        Gen.oneOf(
          arbitrary[Data].map(DatumOption.Inline.apply),
          arbitrary[DataHash].map(DatumOption.Hash.apply)
        )
    }

    object TimelockGen {

        /** KeyHash generator - simplified implementation assuming KeyHash is a value class wrapping
          * a byte array of fixed length (28 bytes as per Cardano specs)
          */
        val genKeyHash: Gen[AddrKeyHash] = arbitrary[AddrKeyHash]

        /** SlotNo generator - simplified implementation assuming SlotNo is a value class wrapping a
          * Long
          */
        val genSlotNo: Gen[SlotNo] = Gen.choose(0L, Long.MaxValue)

        /** Main Timelock generator with controlled recursion depth Uses Gen.sized to ensure
          * termination and reasonable test data
          */
        val genTimelock: Gen[Timelock] = Gen.sized { size =>
            genTimelockWithDepth(size.min(3)) // Cap maximum depth at 3 as per requirements
        }

        /** Helper method to generate Timelock instances with controlled recursion depth
          *
          * @param depth
          *   Maximum nesting depth for recursive Timelock structures
          * @return
          *   A generator for Timelock instances
          */
        private def genTimelockWithDepth(depth: Int): Gen[Timelock] = {
            if depth <= 0 then
                // Base cases - non-recursive Timelock variants
                Gen.oneOf(
                  genKeyHash.map(Timelock.Signature.apply),
                  genSlotNo.map(Timelock.TimeStart.apply),
                  genSlotNo.map(Timelock.TimeExpire.apply)
                )
            else
                // Calculate a reduced depth for nested generators
                val reducedDepth = depth - 1

                // Generate a list of nested Timelock instances with reduced depth
                lazy val genNestedTimelocks: Gen[IndexedSeq[Timelock]] = for
                    n <- Gen.choose(1, 5) // Reasonable limit for number of nested scripts
                    scripts <- Gen.buildableOfN[IndexedSeq[Timelock], Timelock](
                      n,
                      genTimelockWithDepth(reducedDepth)
                    )
                yield scripts

                // Choose between all possible Timelock variants
                Gen.frequency(
                  // Non-recursive variants have higher probability
                  (3, genKeyHash.map(Timelock.Signature.apply)),
                  (2, genSlotNo.map(Timelock.TimeStart.apply)),
                  (2, genSlotNo.map(Timelock.TimeExpire.apply)),

                  // Recursive variants with reduced depth
                  (1, genNestedTimelocks.map(Timelock.AllOf.apply)),
                  (1, genNestedTimelocks.map(Timelock.AnyOf.apply)),
                  (
                    1,
                    for
                        scripts <- genNestedTimelocks
                        m <- Gen.choose(1, scripts.length.max(1)) // Ensure m is valid
                    yield Timelock.MOf(m, scripts)
                  )
                )
        }
    }
    // FIXME: autoDerived for Script is not working correctly
    given Arbitrary[Script] = Arbitrary {
        Gen.oneOf(
          arbitrary[Script.Native],
          arbitrary[Script.PlutusV1],
          arbitrary[Script.PlutusV2],
          arbitrary[Script.PlutusV3],
          arbitrary[Script.PlutusV4],
        )
    }
    given Arbitrary[ScriptRef] = autoDerived
    given Arbitrary[Timelock] = Arbitrary(TimelockGen.genTimelock)

    given Arbitrary[TransactionInput] = Arbitrary {
        for
            transactionId <- arbitrary[TransactionHash]
            index <- Gen.frequency(5 -> Gen.choose(0, 10), 1 -> Gen.choose(11, 65535))
        yield Input(transactionId, index)
    }

    /** The generator behind `Arbitrary[TransactionOutput]`: the Shelley and the Babbage form with
      * equal chance, as the derived instance before D5, every datum and script reference form of
      * `[gen.output.ledger-forms]`, and an address of `genPaymentAddress`, never a stake address
      * (F-35). The derived instance discarded 3 % of samples at sizes 0 to 2.
      */
    def genTransactionOutput: Gen[TransactionOutput] = {
        val shelley =
            for
                address <- genPaymentAddress
                value <- arbitrary[Value]
                datumHash <- genOptional(arbitrary[DataHash])
            yield TransactionOutput.Shelley(address, value, datumHash)
        val babbage =
            for
                address <- genPaymentAddress
                value <- arbitrary[Value]
                datum <- genOptional(arbitrary[DatumOption])
                scriptRef <- genOptional(arbitrary[ScriptRef])
            yield TransactionOutput.Babbage(address, value, datum, scriptRef)
        Gen.oneOf(shelley, babbage)
    }

    given Arbitrary[TransactionOutput] = Arbitrary(genTransactionOutput)
    given Arbitrary[ProtocolVersion] = Arbitrary {
        for
            major <- Gen.choose(1, 10)
            minor <- Gen.choose(0, Int.MaxValue)
        yield ProtocolVersion(major, minor)
    }
    given Arbitrary[RewardAccount] = autoDerived

    given Arbitrary[NonNegativeInterval] = Arbitrary {
        for
            numerator <- Gen.choose(0L, Long.MaxValue)
            denominator <- Gen.posNum[Long]
        yield NonNegativeInterval(numerator, denominator)
    }

    given Arbitrary[VrfCert] = Arbitrary {
        for
            outputSize <- Gen.choose(0, 128)
            output <- genByteStringOfN(outputSize)
            proof <- genByteStringOfN(80)
        yield VrfCert(output, proof)
    }

    given Arbitrary[BlockHeaderBody] = Arbitrary {
        for
            blockNumber <- Gen.choose(0L, Long.MaxValue)
            slot <- Gen.choose(0L, Long.MaxValue)
            prevHash <- arbitrary[Option[BlockHash]]
            issuerVkey <- genByteStringOfN(32)
            vrfVkey <- genByteStringOfN(32)
            vrfResult <- arbitrary[VrfCert]
            blockBodySize <- Gen.choose(0L, Long.MaxValue)
            blockBodyHash <- arbitrary[BlockHash]
            operationalCert <- arbitrary[OperationalCert]
            protocolVersion <- arbitrary[ProtocolVersion]
        yield BlockHeaderBody(
          blockNumber,
          slot,
          prevHash,
          issuerVkey,
          vrfVkey,
          vrfResult,
          blockBodySize,
          blockBodyHash,
          operationalCert,
          protocolVersion
        )
    }

    given Arbitrary[BlockHeader] = Arbitrary {
        for
            headerBody <- arbitrary[BlockHeaderBody]
            bodySignature <- genByteStringOfN(448)
        yield BlockHeader(headerBody, bodySignature)
    }

    given Arbitrary[Metadatum] = {
        val genMetadatumInt =
            Gen.choose(Long.MinValue, Long.MaxValue).map(Metadatum.Int.apply)

        val genMetadatumBytes =
            Gen.choose(0, 64).flatMap(genByteStringOfN).map(Metadatum.Bytes.apply)

        val genMetadatumText = Gen
            .choose(0, 64)
            .flatMap(Gen.listOfN(_, Gen.alphaNumChar))
            .map(_.mkString)
            .map(Metadatum.Text.apply)

        def genMetadatum(depth: Int): Gen[Metadatum] = {
            if depth <= 0 then
                Gen.oneOf(
                  Gen.const(
                    Metadatum.Map(
                      immutable.Map.empty[Metadatum, Metadatum]
                    )
                  ),
                  Gen.const(Metadatum.List(immutable.IndexedSeq.empty)),
                  genMetadatumInt,
                  genMetadatumBytes,
                  genMetadatumText
                )
            else
                val reducedDepth = depth - 1
                Gen.frequency(
                  2 -> (
                    for
                        size <- Gen.choose(0, 2)
                        result <- Gen.mapOfN(
                          size,
                          for
                              key <- Gen.lzy(genMetadatum(reducedDepth))
                              value <- Gen.lzy(genMetadatum(reducedDepth))
                          yield (key, value)
                        )
                    yield Metadatum.Map(result)
                  ),
                  2 -> (
                    for
                        size <- Gen.choose(0, 2)
                        result <- Gen.containerOfN[IndexedSeq, Metadatum](
                          size,
                          Gen.lzy(genMetadatum(reducedDepth))
                        )
                    yield Metadatum.List(result)
                  ),
                  1 -> genMetadatumInt,
                  1 -> genMetadatumBytes,
                  1 -> genMetadatumText
                )

        }

        Arbitrary(Gen.choose(0, 3).flatMap(genMetadatum))
    }

    given Arbitrary[Word64] = Arbitrary(Arbitrary.arbitrary[Long].map(Word64(_)))

    given Arbitrary[AuxiliaryData] = {
        given [A: Arbitrary, B: Arbitrary]: Arbitrary[immutable.Map[A, B]] = Arbitrary(
          genMapOfSizeFromArbitrary(0, 4)
        )

        given [A: Arbitrary]: Arbitrary[immutable.Seq[A]] = Arbitrary(
          genVectorOfSizeFromArbitrary(0, 4)
        )

        val result: Arbitrary[AuxiliaryData] = autoDerived
        withFallback(3, Gen.const(AuxiliaryData.Metadata(immutable.Map.empty)))(result)
    }

    given Arbitrary[VKeyWitness] = Arbitrary {
        for
            vkey <- genByteStringOfN(32)
            signature <- genByteStringOfN(64)
        yield VKeyWitness(vkey, signature)
    }

    given Arbitrary[BootstrapWitness] = Arbitrary {
        for
            publicKey <- genByteStringOfN(32)
            signature <- genByteStringOfN(64)
            chainCode <- genByteStringOfN(32)
            attributesSize <- Gen.choose(0, 128)
            attributes <- genByteStringOfN(attributesSize)
        yield BootstrapWitness(publicKey, signature, chainCode, attributes)
    }

    // A derived enum of case objects fell back to `Gen.fail` at size 0 ([gen.size.derived-fallback])
    given Arbitrary[RedeemerTag] = Arbitrary(Gen.oneOf(RedeemerTag.values.toSeq))

    given Arbitrary[Redeemer] = Arbitrary {
        for
            tag <- arbitrary[RedeemerTag]
            index <- Gen.choose(0, Int.MaxValue)
            data <- arbitrary[Data]
            exUnits <- arbitrary[ExUnits]
        yield Redeemer(tag, index, data, exUnits)
    }

    given Arbitrary[Redeemers] = Arbitrary {
        Gen.oneOf(
          genVectorOfSizeFromArbitrary[Redeemer](1, 3).map(Redeemers.Array.apply), {
              given Arbitrary[Int] = Arbitrary(Gen.choose(0, Int.MaxValue))
              genMapOfSizeFromArbitrary[(RedeemerTag, Int), (Data, ExUnits)](1, 3)
                  .map(Redeemers.Map.apply)
          }
        )
    }

    given Arbitrary[Script.Native] = Arbitrary {
        arbitrary[Timelock].map(Script.Native.apply)
    }

    given Arbitrary[Script.PlutusV1] = Arbitrary {
        for bytes <- genByteStringOfN(32)
        yield Script.PlutusV1(bytes)
    }

    given Arbitrary[Script.PlutusV2] = Arbitrary {
        for bytes <- genByteStringOfN(32)
        yield Script.PlutusV2(bytes)
    }

    given Arbitrary[Script.PlutusV3] = Arbitrary {
        for bytes <- genByteStringOfN(32)
        yield Script.PlutusV3(bytes)
    }

    given Arbitrary[Script.PlutusV4] = Arbitrary {
        for bytes <- genByteStringOfN(32)
        yield Script.PlutusV4(bytes)
    }

    given [A: Arbitrary]: Arbitrary[TaggedOrderedSet[A]] = Arbitrary(
      genSetOfSizeFromArbitrary(1, 3).map(TaggedOrderedSet.from)
    )

    given [A: Arbitrary]: Arbitrary[TaggedOrderedStrictSet[A]] = Arbitrary(
      genSetOfSizeFromArbitrary(1, 3).map(TaggedOrderedStrictSet.from)
    )

    given [A: Arbitrary: Ordering]: Arbitrary[TaggedSortedSet[A]] = Arbitrary(
      genSetOfSizeFromArbitrary(1, 3).map(TaggedSortedSet.from)
    )

    given [A: Arbitrary, K: Ordering](using
        TaggedSortedMap.KeyOf[K, A]
    ): Arbitrary[TaggedSortedMap[K, A]] = Arbitrary(
      genSetOfSizeFromArbitrary(1, 3).map(TaggedSortedMap.from[K, A])
    )

    given [A: Arbitrary, K: Ordering](using
        TaggedSortedStrictMap.KeyOf[K, A]
    ): Arbitrary[TaggedSortedStrictMap[K, A]] = Arbitrary(
      genSetOfSizeFromArbitrary(1, 3).map(TaggedSortedStrictMap.from[K, A])
    )

    given Arbitrary[TransactionWitnessSet] = autoDerived

    given Arbitrary[UnitInterval] = Arbitrary {
        for
            denominator <- Gen.posNum[Long]
            numerator <- Gen.choose(0L, denominator)
        yield UnitInterval(numerator, denominator)
    }

    given Arbitrary[ProposalProcedure] = autoDerived
    given Arbitrary[Set[ProposalProcedure]] = Arbitrary(genSetOfSizeFromArbitrary(0, 3))
    // A derived enum of case objects gives `Gen.fail` at size 0 ([gen.size.derived-fallback])
    given Arbitrary[Vote] = Arbitrary(Gen.oneOf(Vote.values.toSeq))
    // FIXME: autoDerived for Voter is not working correctly
//    given Arbitrary[Voter] = autoDerived
    given Arbitrary[Voter] = Arbitrary {
        Gen.oneOf(
          arbitrary[AddrKeyHash].map(Voter.ConstitutionalCommitteeHotKey.apply),
          arbitrary[ScriptHash].map(Voter.ConstitutionalCommitteeHotScript.apply),
          arbitrary[AddrKeyHash].map(Voter.DRepKey.apply),
          arbitrary[ScriptHash].map(Voter.DRepScript.apply),
          arbitrary[AddrKeyHash].map(Voter.StakingPoolKey.apply),
        )
    }
    // Derived, its `Option[Anchor]` resolved to a derived `Option`, which gives `Gen.fail` at size 0
    given Arbitrary[VotingProcedure] = Arbitrary {
        Gen.zip(arbitrary[Vote], genOptional(arbitrary[Anchor])).map(VotingProcedure(_, _))
    }

    given arbVotingProcedureMap: Arbitrary[SortedMap[GovActionId, VotingProcedure]] = Arbitrary {
        genMapOfSizeFromArbitrary[GovActionId, VotingProcedure](1, 4).map(map =>
            SortedMap.from(map)
        )
    }

    given arbVotingProceduresMap
        : Arbitrary[SortedMap[Voter, SortedMap[GovActionId, VotingProcedure]]] = Arbitrary {
        genMapOfSizeFromArbitrary[Voter, SortedMap[GovActionId, VotingProcedure]](1, 4).map(map =>
            SortedMap.from(map)
        )
    }

    given Arbitrary[VotingProcedures] = Arbitrary {
        arbitrary[SortedMap[Voter, SortedMap[GovActionId, VotingProcedure]]].map(
          VotingProcedures.apply
        )
    }

    given Arbitrary[PoolVotingThresholds] = autoDerived
    given Arbitrary[DRepVotingThresholds] = autoDerived

    given Arbitrary[ProtocolParamUpdate] = {
        given Arbitrary[Int] = Arbitrary(Gen.choose(0, Int.MaxValue))

        val result: Arbitrary[ProtocolParamUpdate] = autoDerived
        result
    }

    given Arbitrary[GovAction] = {
        given Arbitrary[Long] = Arbitrary(Gen.choose(0, Long.MaxValue))

        val result: Arbitrary[GovAction] = autoDerived
        // the one case without fields
        withFallback(10, Gen.const(GovAction.InfoAction))(result)
    }

    given Arbitrary[Relay] = {
        val genSingleHostAddr =
            for
                port <- Gen.option(Gen.choose(0, 65535))
                ipv4 <- Gen.option(genByteStringOfN(4))
                ipv6 <- Gen.option(genByteStringOfN(16))
            yield Relay.SingleHostAddr(port, ipv4, ipv6)

        val genSingleHostName =
            for
                port <- Gen.option(Gen.choose(0, 65535))
                dnsName <- Gen
                    .choose(5, 128)
                    .flatMap(Gen.listOfN(_, Gen.alphaNumChar))
                    .map(_.mkString)
            yield Relay.SingleHostName(port, dnsName)

        val genMultiHostName =
            for dnsName <- Gen
                    .choose(5, 128)
                    .flatMap(Gen.listOfN(_, Gen.alphaNumChar))
                    .map(_.mkString)
            yield Relay.MultiHostName(dnsName)

        val genRelay = Gen.oneOf(genSingleHostAddr, genSingleHostName, genMultiHostName)

        Arbitrary(genRelay)
    }

    /** `[gen.size.derived-fallback]`: below `minSize`, a derived sum type draws `fallback`.
      *
      * A `given ArbitraryDerivation.Fallback[T]` has no effect: `ArbitraryDerivation.split` summons
      * the fallback inside its own generic method, so it always finds the default `Gen.fail`, and
      * the derived instance discards every sample at size 0.
      */
    private def withFallback[T](minSize: Int, fallback: Gen[T])(
        derived: Arbitrary[T]
    ): Arbitrary[T] =
        Arbitrary(Gen.sized(size => if size < minSize then fallback else derived.arbitrary))

    given Arbitrary[Certificate] = {
        given Arbitrary[Long] = Arbitrary(Gen.choose(0, Long.MaxValue))

        val result: Arbitrary[Certificate] = autoDerived
        withFallback(3, arbitrary[Credential].map(Certificate.UnregCert(_, None)))(result)
    }

    given Arbitrary[Withdrawals] = Arbitrary {
        genMapOfSizeFromArbitrary[RewardAccount, Coin](1, 4).map(map =>
            Withdrawals(SortedMap.from(map))
        )
    }

    private def setOf[A: Arbitrary](count: Gen[Int]): Gen[immutable.Set[A]] =
        count.flatMap(n => genSetOfSizeFromArbitrary[A](n, n))

    private def vectorOf[A: Arbitrary](count: Gen[Int]): Gen[immutable.IndexedSeq[A]] =
        count.flatMap(n => genVectorOfSizeFromArbitrary[A](n, n))

    /** The validity start and the `ttl` of a body (`[gen.cover.cross-field]`): in 90 % of samples
      * each is set with chance 1/2, and a `ttl` is at least the start, up to 10^6 slots later; in
      * 10 % both are any slot, so that a codec sees a `ttl` before the start too.
      */
    private def genValidityInterval: Gen[(Option[Long], Option[Long])] = {
        val consistent =
            for
                start <- genOptional(Gen.choose(0L, 200_000_000L))
                ttl <- genOptional(Gen.choose(0L, 1_000_000L).map(_ + start.getOrElse(0L)))
            yield (start, ttl)
        val any = Gen.zip(
          genOptional(Gen.choose(0L, Long.MaxValue)),
          genOptional(Gen.choose(0L, Long.MaxValue))
        )
        Gen.frequency(9 -> consistent, 1 -> any)
    }

    /** The generator behind `Arbitrary[TransactionBody]`. Inputs and outputs are capped by
      * construction (`[gen.size.caps]`); every optional field and every optional collection is
      * empty in half of the samples. Before D5 certificates, collateral inputs and required signers
      * always held 1 to 4 items, and 41 % of bodies had a `ttl` before their start.
      */
    def genTransactionBody: Gen[TransactionBody] =
        for
            inputs <- setOf[TransactionInput](genCappedCount)
            outputs <- vectorOf[Sized[TransactionOutput]](genCappedCount)
            fee <- arbitrary[Coin]
            (validityStartSlot, ttl) <- genValidityInterval
            certificates <- setOf[Certificate](genOptionalCount)
            withdrawals <- genOptional(arbitrary[Withdrawals])
            auxiliaryDataHash <- genOptional(arbitrary[AuxiliaryDataHash])
            mint <- genOptional(arbitrary[Mint])
            scriptDataHash <- genOptional(arbitrary[ScriptDataHash])
            collateralInputs <- setOf[TransactionInput](genOptionalCount)
            requiredSigners <- setOf[AddrKeyHash](genOptionalCount)
            networkId <- genOptional(Gen.oneOf(0, 1))
            collateralReturnOutput <- genOptional(arbitrary[Sized[TransactionOutput]])
            totalCollateral <- genOptional(arbitrary[Coin])
            referenceInputs <- setOf[TransactionInput](genOptionalCount)
            votingProcedures <- genOptional(arbitrary[VotingProcedures])
            proposalProcedures <- vectorOf[ProposalProcedure](genOptionalCount)
            currentTreasuryValue <- genOptional(arbitrary[Coin])
            donation <-
                if currentTreasuryValue.isDefined then
                    Gen.posNum[Long].map(value => Some(Coin(value)))
                else Gen.const(None)
        yield TransactionBody(
          TaggedSortedSet.from(inputs),
          outputs,
          fee,
          ttl,
          // `from` rejects an empty collection at Conway versions; a body without certificates
          // holds the empty set, as `TransactionBody`'s default does
          if certificates.isEmpty then TaggedOrderedStrictSet.empty
          else TaggedOrderedStrictSet.from(certificates),
          withdrawals,
          auxiliaryDataHash,
          validityStartSlot,
          mint,
          scriptDataHash,
          TaggedSortedSet.from(collateralInputs),
          TaggedSortedSet.from(requiredSigners),
          networkId,
          collateralReturnOutput,
          totalCollateral,
          TaggedSortedSet.from(referenceInputs),
          votingProcedures,
          TaggedOrderedSet.from(proposalProcedures),
          currentTreasuryValue,
          donation
        )

    given Arbitrary[TransactionBody] = Arbitrary(genTransactionBody)

    given Arbitrary[Block] = Arbitrary {
        for
            header <- arbitrary[BlockHeader]
            transactionBodies <- genVectorOfSizeFromArbitrary[KeepRaw[TransactionBody]](1, 4)
            transactionBodiesSize = transactionBodies.size
            transactionWitnessSets <- genVectorOfSizeFromArbitrary[KeepRaw[TransactionWitnessSet]](
              transactionBodiesSize,
              transactionBodiesSize
            )
            auxiliaryDataSet <-
                for
                    size <- Gen.choose(0, 4)
                    result <- Gen.mapOfN(
                      size,
                      for
                          key <- Gen.choose(0, transactionBodiesSize - 1)
                          value <- arbitrary[AuxiliaryData]
                      yield (key, KeepRaw(value))
                    )
                yield result
            invalidTransactions <-
                for
                    size <- Gen.choose(0, 4)
                    result <- Gen.containerOfN[IndexedSeq, Int](
                      size,
                      Gen.choose(0, transactionBodiesSize - 1)
                    )
                yield result
        yield Block(
          header,
          transactionBodies,
          transactionWitnessSets,
          auxiliaryDataSet,
          invalidTransactions
        )
    }

    given Arbitrary[BlockFile] = autoDerived

    given Arbitrary[Transaction] = Arbitrary {
        for
            body <- arbitrary[KeepRaw[TransactionBody]]
            witnessSet <- arbitrary[TransactionWitnessSet]
            isValid <- arbitrary[Boolean]
            auxiliaryData <- genOptional(arbitrary[KeepRaw[AuxiliaryData]])
        yield Transaction(body, KeepRaw(witnessSet), isValid, auxiliaryData)
    }
}
