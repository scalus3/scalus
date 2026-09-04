package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class ProtocolParamsTest extends AnyFunSuite {

    private val networks = Seq(
      "mainnet" -> CardanoInfo.mainnet.protocolParams,
      "preprod" -> CardanoInfo.preprod.protocolParams,
      "preview" -> CardanoInfo.preview.protocolParams
    )

    test("Blockfrost JSON round-trips through ProtocolParams") {
        for (_, params) <- networks do
            val json = ProtocolParams.toBlockfrostJson(params)
            val back = ProtocolParams.fromBlockfrostJson(json)
            assert(back == params, ProtocolParams.diff(params, back).mkString("\n"))
    }

    /** Blockfrost's `/epochs/{n}/parameters` schema, by JSON type rather than by value.
      *
      * The round-trip test above cannot see this: the reader accepts a number and a decimal string
      * alike, so a field emitted with the wrong JSON type still round-trips perfectly. That is how
      * ten `integer` fields came to ship as strings - ujson's `JsonableLong` turns a bare `Long`
      * into a `Str` at the call site, which no field-by-field reading of the writer can reveal.
      *
      * A consumer validating against Blockfrost's schema is the one that breaks, so the schema is
      * what this pins.
      */
    private val numberFields = Set(
      "a0",
      "collateral_percent",
      "dvt_committee_no_confidence",
      "dvt_committee_normal",
      "dvt_hard_fork_initiation",
      "dvt_motion_no_confidence",
      "dvt_p_p_economic_group",
      "dvt_p_p_gov_group",
      "dvt_p_p_network_group",
      "dvt_p_p_technical_group",
      "dvt_treasury_withdrawal",
      "dvt_update_to_constitution",
      "e_max",
      "max_block_header_size",
      "max_block_size",
      "max_collateral_inputs",
      "max_tx_size",
      "min_fee_a",
      "min_fee_b",
      "min_fee_ref_script_cost_per_byte",
      "n_opt",
      "price_mem",
      "price_step",
      "protocol_major_ver",
      "protocol_minor_ver",
      "pvt_committee_no_confidence",
      "pvt_committee_normal",
      "pvt_hard_fork_initiation",
      "pvt_motion_no_confidence",
      "pvt_p_p_security_group",
      "pvtpp_security_group",
      "rho",
      "tau"
    )

    /** Fields Blockfrost renders as decimal strings, because they can exceed 2^53. */
    private val stringFields = Set(
      "coins_per_utxo_size",
      "committee_max_term_length",
      "committee_min_size",
      "drep_activity",
      "drep_deposit",
      "gov_action_deposit",
      "gov_action_lifetime",
      "key_deposit",
      "max_block_ex_mem",
      "max_block_ex_steps",
      "max_tx_ex_mem",
      "max_tx_ex_steps",
      "max_val_size",
      "min_pool_cost",
      "pool_deposit"
    )

    test("every Blockfrost field is emitted with the JSON type the schema names") {
        for (network, params) <- networks do
            val json = ujson.read(ProtocolParams.toBlockfrostJson(params))
            for (key, value) <- json.obj do
                if numberFields.contains(key) then
                    assert(
                      value.numOpt.isDefined,
                      s"$network: $key must be a JSON number, was ${value.getClass.getSimpleName} ($value)"
                    )
                else if stringFields.contains(key) then
                    assert(
                      value.strOpt.isDefined,
                      s"$network: $key must be a JSON string, was ${value.getClass.getSimpleName} ($value)"
                    )
                else
                    // cost_models_raw is the only structured field; anything else is new and
                    // needs classifying above rather than silently passing.
                    assert(
                      key == "cost_models_raw",
                      s"$network: unclassified Blockfrost field '$key'; add it to numberFields or stringFields"
                    )

            // and the reverse: every field named above is actually emitted
            val emitted = json.obj.keySet
            val missing = (numberFields ++ stringFields) -- emitted
            assert(missing.isEmpty, s"$network: declared but not emitted: ${missing.toSeq.sorted}")
    }
}
