package scalus.cardano.ledger

import io.bullet.borer.Encoder
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin
import scalus.builtin.Data.*
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.testutil.ArbitraryDerivation.autoDerived

import scala.collection.immutable
import scala.collection.immutable.SortedMap
import scala.math.pow

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends scalus.cardano.address.ArbitraryInstances {
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

    given Arbitrary[Coin] = Arbitrary(Gen.choose(0L, pow(2L, 53).toLong).map(Coin.apply))
    given Arbitrary[AssetName] = Arbitrary(
      Gen.choose(0, 32).flatMap(genByteStringOfN).map(AssetName.apply)
    )

    given Arbitrary[Language] = autoDerived
    given Arbitrary[AddressBytes] = Arbitrary(arbitrary[ByteString].map(AddressBytes.apply))
    given Arbitrary[Slot] = Arbitrary(Gen.choose(0L, Long.MaxValue).map(Slot.apply))
    given Arbitrary[ExUnits] = Arbitrary {
        for
            mem <- Gen.choose(0L, 1000L)
            steps <- Gen.choose(0L, 1000L)
        yield ExUnits(mem, steps)
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
        minPolicies: Int = 1,
        maxPolicies: Int = 8,
        minAssets: Int = 1,
        maxAssets: Int = 8
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
        )
    }
    given Arbitrary[ScriptRef] = autoDerived
    given Arbitrary[Timelock] = Arbitrary(TimelockGen.genTimelock)

    given Arbitrary[TransactionInput] = Arbitrary {
        for
            transactionId <- arbitrary[TransactionHash]
            index <- Gen.choose(0, Int.MaxValue)
        yield TransactionInput(transactionId, index)
    }

    given Arbitrary[TransactionOutput] = autoDerived
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
        result
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

    given Arbitrary[RedeemerTag] = autoDerived

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
    given Arbitrary[Vote] = autoDerived
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
    given Arbitrary[VotingProcedure] = autoDerived

    given Arbitrary[VotingProcedures] = {
        given Arbitrary[SortedMap[GovActionId, VotingProcedure]] = Arbitrary {
            genMapOfSizeFromArbitrary[GovActionId, VotingProcedure](1, 4).map(map =>
                SortedMap.from(map)
            )
        }

        Arbitrary {
            genMapOfSizeFromArbitrary[Voter, SortedMap[GovActionId, VotingProcedure]](1, 4).map(
              map => VotingProcedures(SortedMap.from(map))
            )
        }
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
        result
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

    given Arbitrary[Certificate] = {
        given Arbitrary[Long] = Arbitrary(Gen.choose(0, Long.MaxValue))

        val result: Arbitrary[Certificate] = autoDerived
        result
    }

    given Arbitrary[Withdrawals] = Arbitrary {
        genMapOfSizeFromArbitrary[RewardAccount, Coin](1, 4).map(map =>
            Withdrawals(SortedMap.from(map))
        )
    }

    given Arbitrary[TransactionBody] = Arbitrary {
        for
            inputs <- genSetOfSizeFromArbitrary[TransactionInput](1, 4).map(TaggedSortedSet.from)
            outputs <- genVectorOfSizeFromArbitrary[Sized[TransactionOutput]](1, 4)
            fee <- arbitrary[Coin]
            ttl <- Gen.option(Gen.choose(0L, Long.MaxValue))
            certificates <- genSetOfSizeFromArbitrary[Certificate](1, 4).map(
              TaggedOrderedStrictSet.from
            )
            withdrawals <- arbitrary[Option[Withdrawals]]
            auxiliaryDataHash <- arbitrary[Option[AuxiliaryDataHash]]
            validityStartSlot <- Gen.option(Gen.choose(0L, Long.MaxValue))
            mint <- arbitrary[Option[Mint]]
            scriptDataHash <- arbitrary[Option[ScriptDataHash]]
            collateralInputs <- genSetOfSizeFromArbitrary[TransactionInput](1, 4).map(
              TaggedSortedSet.from
            )
            requiredSigners <- genSetOfSizeFromArbitrary[AddrKeyHash](1, 4).map(set =>
                TaggedSortedSet.from(set)
            )
            networkId <- Gen.option(Gen.oneOf(Gen.const(0), Gen.const(1)))
            collateralReturnOutput <- arbitrary[Option[Sized[TransactionOutput]]]
            totalCollateral <- arbitrary[Option[Coin]]
            referenceInputs <- genSetOfSizeFromArbitrary[TransactionInput](0, 4).map(
              TaggedSortedSet.from
            )
            votingProcedures <- arbitrary[Option[VotingProcedures]]
            proposalProcedures <- genVectorOfSizeFromArbitrary[ProposalProcedure](0, 4).map(
              TaggedOrderedSet.from
            )
            currentTreasuryValue <- arbitrary[Option[Coin]]
            donation <-
                if currentTreasuryValue.isDefined then
                    Gen.posNum[Long].map(value => Some(Coin(value)))
                else Gen.const(None)
        yield TransactionBody(
          inputs,
          outputs,
          fee,
          ttl,
          certificates,
          withdrawals,
          auxiliaryDataHash,
          validityStartSlot,
          mint,
          scriptDataHash,
          collateralInputs,
          requiredSigners,
          networkId,
          collateralReturnOutput,
          totalCollateral,
          referenceInputs,
          votingProcedures,
          proposalProcedures,
          currentTreasuryValue,
          donation
        )
    }

    given Arbitrary[Block] = Arbitrary {
        for
            header <- arbitrary[BlockHeader]
            transactionBodies <- genVectorOfSizeFromArbitrary[KeepRaw[TransactionBody]](1, 4)
            transactionBodiesSize = transactionBodies.size
            transactionWitnessSets <- genVectorOfSizeFromArbitrary[TransactionWitnessSet](
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

    given [A: Arbitrary: Encoder]: Arbitrary[KeepRaw[A]] = Arbitrary {
        Arbitrary.arbitrary[A].map(a => KeepRaw(a))
    }

    given [A: Arbitrary: Encoder]: Arbitrary[Sized[A]] = Arbitrary {
        Arbitrary.arbitrary[A].map(a => Sized(a))
    }

    given Arbitrary[BlockFile] = autoDerived

    given Arbitrary[Transaction] = Arbitrary {
        for
            body <- arbitrary[KeepRaw[TransactionBody]]
            witnessSet <- arbitrary[TransactionWitnessSet]
            isValid <- arbitrary[Boolean]
            auxiliaryData <- arbitrary[Option[KeepRaw[AuxiliaryData]]]
        yield Transaction(body, witnessSet, isValid, auxiliaryData)
    }
}
