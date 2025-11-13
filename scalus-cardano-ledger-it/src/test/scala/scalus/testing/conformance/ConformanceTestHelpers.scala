package scalus.testing.conformance

import scalus.cardano.ledger.*
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart, ShelleyDelegationPart, Network}

/** Helper methods for conformance tests to convert Amaru data to Scalus types */
trait ConformanceTestHelpers:
    this: ConformanceTestBase =>

    import TestDataModels.extractLovelace

    /** Build UTxO set from pool pledge amounts
      *
      * Creates simplified UTxO entries using pool pledges as values.
      * Each pool owner gets a UTxO with their share of the pledge.
      */
    protected def buildUtxosFromPoolPledges(
        pools: Map[String, TestDataModels.PoolData]
    ): Utxos =
        pools.flatMap { case (poolIdStr, poolData) =>
            poolData.owners.zipWithIndex.map { case (ownerHex, idx) =>
                val stakeCred = Credential.KeyHash(AddrKeyHash.fromHex(ownerHex))

                // Create mock address with stake credential
                val address = ShelleyAddress(
                  Network.Testnet,
                  ShelleyPaymentPart.Key(AddrKeyHash.fromHex(ownerHex)),
                  ShelleyDelegationPart.Key(StakeKeyHash.fromHex(ownerHex))
                )

                // Create transaction input
                val txHash = TransactionHash.fromHex(ownerHex.take(64).padTo(64, '0'))
                val txInput = TransactionInput(txHash, idx)

                // Create output with pledge value
                val pledgePerOwner = extractLovelace(poolData.pledge) / poolData.owners.size
                val value = Value(Coin(pledgePerOwner.toLong), MultiAsset.empty)
                val output = TransactionOutput.Shelley(address, value, None)

                txInput -> output
            }
        }.toMap

    /** Build pool delegation mappings from pool data */
    protected def buildPoolDelegations(
        pools: Map[String, TestDataModels.PoolData]
    ): Map[Credential, PoolKeyHash] =
        pools.flatMap { case (poolIdStr, poolData) =>
            val poolKeyHash = parsePoolKeyHash(poolIdStr)
            poolData.owners.map { ownerHex =>
                val credential = Credential.KeyHash(AddrKeyHash.fromHex(ownerHex))
                credential -> poolKeyHash
            }
        }

    /** Build DRep delegation mappings from DRep data */
    protected def buildDRepDelegations(
        dreps: List[TestDataModels.DRepData]
    ): Map[Credential, DRep] =
        dreps.flatMap { drepData =>
            parseDRep(drepData).toSeq.flatMap { drep =>
                drepData.delegators.map { delegator =>
                    val credential = parseCredential(delegator.credential, delegator.from)
                    credential -> drep
                }
            }
        }.toMap

    /** Parse DRep from test data */
    protected def parseDRep(drepData: TestDataModels.DRepData): Option[DRep] =
        drepData.`type` match
            case "abstain"       => Some(DRep.AlwaysAbstain)
            case "no_confidence" => Some(DRep.AlwaysNoConfidence)
            case "registered" =>
                (drepData.id, drepData.from) match
                    case (Some(id), Some("script")) =>
                        Some(DRep.ScriptHash(ScriptHash.fromHex(id)))
                    case (Some(id), Some("verificationKey")) =>
                        Some(DRep.KeyHash(AddrKeyHash.fromHex(id)))
                    case _ => None
            case _ => None

    /** Parse credential from hex ID and type */
    protected def parseCredential(hexId: String, credType: String): Credential =
        credType match
            case "script" =>
                Credential.ScriptHash(ScriptHash.fromHex(hexId))
            case "verificationKey" | _ =>
                Credential.KeyHash(AddrKeyHash.fromHex(hexId))

    /** Parse pool key hash from bech32 or hex string */
    protected def parsePoolKeyHash(poolIdStr: String): PoolKeyHash =
        if poolIdStr.startsWith("pool") then
            val decoded = Bech32.decode(poolIdStr)
            PoolKeyHash.fromArray(decoded.data)
        else
            PoolKeyHash.fromHex(poolIdStr)
