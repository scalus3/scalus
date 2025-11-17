package scalus.testing.conformance

import scalus.testing.conformance.ConformanceTestSchema.*

/** Utilities for comparing ledger states and generating diffs
  *
  * Provides methods to compare expected vs actual ledger states and generate detailed
  * differences for debugging failed conformance tests.
  */
object StateComparator {

  /** Compare two ledger states and generate a detailed diff
    *
    * @param expected
    *   Expected ledger state
    * @param actual
    *   Actual ledger state
    * @return
    *   StateDiff with all differences found, None if states match
    */
  def compareStates(
      expected: ExpectedLedgerState,
      actual: InitialLedgerState
  ): Option[StateDiff] = {
    val utxoDiff = expected.utxos.flatMap { expectedUtxos =>
      compareUtxoSets(expectedUtxos, actual.utxos)
    }

    val treasuryDiff = (expected.treasury, actual.treasury) match {
      case (Some(exp), Some(act)) if exp != act =>
        Some(ValueDiff(expected = exp, actual = act))
      case _ => None
    }

    val reservesDiff = (expected.reserves, actual.reserves) match {
      case (Some(exp), Some(act)) if exp != act =>
        Some(ValueDiff(expected = exp, actual = act))
      case _ => None
    }

    val accountsDiff = (expected.accounts, actual.accounts) match {
      case (Some(expAccounts), Some(actAccounts)) =>
        val diffs = compareAccounts(expAccounts, actAccounts)
        if (diffs.nonEmpty) Some(diffs) else None
      case _ => None
    }

    // If all diffs are None, states match
    if (
      utxoDiff.isEmpty && treasuryDiff.isEmpty && reservesDiff.isEmpty && accountsDiff.isEmpty
    ) {
      None
    } else {
      Some(
        StateDiff(
          utxoDiff = utxoDiff,
          treasuryDiff = treasuryDiff,
          reservesDiff = reservesDiff,
          accountsDiff = accountsDiff
        )
      )
    }
  }

  /** Compare UTXO sets and generate detailed differences
    *
    * @param expected
    *   Expected UTXO entries
    * @param actual
    *   Actual UTXO entries
    * @return
    *   UtxoSetDiff with missing, unexpected, and mismatched entries
    */
  def compareUtxoSets(
      expected: List[UtxoEntry],
      actual: List[UtxoEntry]
  ): Option[UtxoSetDiff] = {
    val expectedMap = expected.map(e => (e.txHash, e.outputIndex) -> e).toMap
    val actualMap = actual.map(e => (e.txHash, e.outputIndex) -> e).toMap

    val expectedKeys = expectedMap.keySet
    val actualKeys = actualMap.keySet

    // Find missing (in expected but not in actual)
    val missing = (expectedKeys -- actualKeys).map { case (txHash, outputIndex) =>
      UtxoRef(txHash, outputIndex)
    }.toList

    // Find unexpected (in actual but not in expected)
    val unexpected = (actualKeys -- expectedKeys).map { case key =>
      actualMap(key)
    }.toList

    // Find mismatched (in both but different)
    val mismatched = expectedKeys.intersect(actualKeys).flatMap { key =>
      val exp = expectedMap(key)
      val act = actualMap(key)
      if (!utxoEntriesMatch(exp, act)) {
        Some(
          UtxoMismatch(
            ref = UtxoRef(exp.txHash, exp.outputIndex),
            expected = exp,
            actual = act
          )
        )
      } else None
    }.toList

    if (missing.isEmpty && unexpected.isEmpty && mismatched.isEmpty) {
      None
    } else {
      Some(UtxoSetDiff(missing = missing, unexpected = unexpected, mismatched = mismatched))
    }
  }

  /** Check if two UTXO entries match
    *
    * @param expected
    *   Expected UTXO entry
    * @param actual
    *   Actual UTXO entry
    * @return
    *   true if entries match
    */
  def utxoEntriesMatch(expected: UtxoEntry, actual: UtxoEntry): Boolean = {
    expected.txHash == actual.txHash &&
    expected.outputIndex == actual.outputIndex &&
    expected.address == actual.address &&
    expected.value == actual.value &&
    expected.datum == actual.datum &&
    expected.referenceScript == actual.referenceScript &&
    expected.assets == actual.assets
  }

  /** Compare account states and find differences
    *
    * @param expected
    *   Expected account states
    * @param actual
    *   Actual account states
    * @return
    *   List of account differences
    */
  def compareAccounts(
      expected: List[AccountState],
      actual: List[AccountState]
  ): List[AccountDiff] = {
    val expectedMap = expected.map(a => a.stakeAddress -> a).toMap
    val actualMap = actual.map(a => a.stakeAddress -> a).toMap

    val allAddresses = expectedMap.keySet ++ actualMap.keySet

    allAddresses.flatMap { addr =>
      (expectedMap.get(addr), actualMap.get(addr)) match {
        case (Some(exp), Some(act)) =>
          compareAccountFields(addr, exp, act)
        case (Some(exp), None) =>
          List(
            AccountDiff(
              address = addr,
              field = "existence",
              expected = "present",
              actual = "missing"
            )
          )
        case (None, Some(act)) =>
          List(
            AccountDiff(
              address = addr,
              field = "existence",
              expected = "missing",
              actual = "present"
            )
          )
        case (None, None) => List.empty
      }
    }.toList
  }

  /** Compare individual fields of account states
    *
    * @param address
    *   Stake address
    * @param expected
    *   Expected account state
    * @param actual
    *   Actual account state
    * @return
    *   List of field differences
    */
  private def compareAccountFields(
      address: String,
      expected: AccountState,
      actual: AccountState
  ): List[AccountDiff] = {
    val diffs = List.newBuilder[AccountDiff]

    if (expected.delegation != actual.delegation) {
      diffs += AccountDiff(
        address = address,
        field = "delegation",
        expected = expected.delegation.toString,
        actual = actual.delegation.toString
      )
    }

    if (expected.rewards != actual.rewards) {
      diffs += AccountDiff(
        address = address,
        field = "rewards",
        expected = expected.rewards.getOrElse("none"),
        actual = actual.rewards.getOrElse("none")
      )
    }

    if (expected.deposit != actual.deposit) {
      diffs += AccountDiff(
        address = address,
        field = "deposit",
        expected = expected.deposit.getOrElse("none"),
        actual = actual.deposit.getOrElse("none")
      )
    }

    diffs.result()
  }

  /** Apply UTXO changes to a UTXO set
    *
    * Helper method to apply expected changes and generate resulting UTXO set
    *
    * @param initial
    *   Initial UTXO set
    * @param changes
    *   Changes to apply
    * @return
    *   Resulting UTXO set after applying changes
    */
  def applyUtxoChanges(initial: List[UtxoEntry], changes: UtxoChanges): List[UtxoEntry] = {
    val removedKeys = changes.removed.map(r => (r.txHash, r.outputIndex)).toSet
    val remaining = initial.filterNot(e => removedKeys.contains((e.txHash, e.outputIndex)))
    remaining ++ changes.added
  }

  /** Generate a human-readable summary of state differences
    *
    * @param diff
    *   State diff to summarize
    * @return
    *   Human-readable summary string
    */
  def summarizeDiff(diff: StateDiff): String = {
    val sb = new StringBuilder
    sb.append("State Differences:\n")

    diff.utxoDiff.foreach { utxoDiff =>
      sb.append("\nUTXO Differences:\n")
      if (utxoDiff.missing.nonEmpty) {
        sb.append(s"  Missing ${utxoDiff.missing.size} entries:\n")
        utxoDiff.missing.take(5).foreach { ref =>
          sb.append(s"    - ${ref.txHash}#${ref.outputIndex}\n")
        }
        if (utxoDiff.missing.size > 5) {
          sb.append(s"    ... and ${utxoDiff.missing.size - 5} more\n")
        }
      }
      if (utxoDiff.unexpected.nonEmpty) {
        sb.append(s"  Unexpected ${utxoDiff.unexpected.size} entries:\n")
        utxoDiff.unexpected.take(5).foreach { entry =>
          sb.append(s"    - ${entry.txHash}#${entry.outputIndex}\n")
        }
        if (utxoDiff.unexpected.size > 5) {
          sb.append(s"    ... and ${utxoDiff.unexpected.size - 5} more\n")
        }
      }
      if (utxoDiff.mismatched.nonEmpty) {
        sb.append(s"  Mismatched ${utxoDiff.mismatched.size} entries:\n")
        utxoDiff.mismatched.take(5).foreach { mismatch =>
          sb.append(s"    - ${mismatch.ref.txHash}#${mismatch.ref.outputIndex}\n")
          if (mismatch.expected.value != mismatch.actual.value) {
            sb.append(
              s"      Value: expected ${mismatch.expected.value}, got ${mismatch.actual.value}\n"
            )
          }
          if (mismatch.expected.address != mismatch.actual.address) {
            sb.append(
              s"      Address: expected ${mismatch.expected.address}, got ${mismatch.actual.address}\n"
            )
          }
        }
        if (utxoDiff.mismatched.size > 5) {
          sb.append(s"    ... and ${utxoDiff.mismatched.size - 5} more\n")
        }
      }
    }

    diff.treasuryDiff.foreach { treasuryDiff =>
      sb.append(
        s"\nTreasury: expected ${treasuryDiff.expected}, got ${treasuryDiff.actual}\n"
      )
    }

    diff.reservesDiff.foreach { reservesDiff =>
      sb.append(
        s"Reserves: expected ${reservesDiff.expected}, got ${reservesDiff.actual}\n"
      )
    }

    diff.accountsDiff.foreach { accountsDiff =>
      sb.append(s"\nAccount Differences (${accountsDiff.size} total):\n")
      accountsDiff.take(10).foreach { accountDiff =>
        sb.append(
          s"  ${accountDiff.address}.${accountDiff.field}: expected ${accountDiff.expected}, got ${accountDiff.actual}\n"
        )
      }
      if (accountsDiff.size > 10) {
        sb.append(s"  ... and ${accountsDiff.size - 10} more\n")
      }
    }

    sb.toString
  }

  /** Validate that expected state matches actual state
    *
    * @param expected
    *   Expected ledger state
    * @param actual
    *   Actual ledger state
    * @return
    *   Right(()) if states match, Left(error message) if they don't
    */
  def validateState(
      expected: ExpectedLedgerState,
      actual: InitialLedgerState
  ): Either[String, Unit] = {
    compareStates(expected, actual) match {
      case None => Right(())
      case Some(diff) =>
        Left(summarizeDiff(diff))
    }
  }
}
