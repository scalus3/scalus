package scalus.uplc.eval

import scala.collection.immutable

/** A CEK environment as a persistent stack with jump pointers (Myers 1983, "An applicative
  * random-access stack"). `extend` is O(1) and allocates one node; `lookup` of a de Bruijn index is
  * O(log n). It is still an `immutable.Seq[(String, CekValue)]`, oldest binding first, so it
  * satisfies the public `CekValEnv` type.
  *
  * Why not the `ArraySeq` it replaces: appending to it copies the whole environment, and in the
  * plutus-vm-benchmark scripts 60% of binders are deeper than 64. Why not a plain linked list: de
  * Bruijn indices can be large too (17% of the variables in those scripts are more than 64 bindings
  * up, mostly top-level helpers), and a list walks that far on every lookup. Plutus uses a
  * skew-binary random-access list for the same reason.
  *
  * Each node is one binding. `depth` counts the bindings, so the root (the empty environment) has
  * depth 0 and the newest binding has depth `length`. Besides `parent` (depth - 1), each node has a
  * `jump` to an ancestor further up; see `extend` for the rule. The jumps let `nodeAt` reach any
  * ancestor in O(log n) steps.
  */
private[eval] final class CekEnv private (
    val name: String,
    val value: CekValue | Null,
    val parent: CekEnv | Null,
    jumpOrNull: CekEnv | Null,
    val depth: Int
) extends immutable.AbstractSeq[(String, CekValue)] {
    // The root jumps to itself, so `jump` is never null.
    private val jump: CekEnv = if jumpOrNull == null then this else jumpOrNull

    /** This environment with `name` bound to `value`, as the newest binding.
      *
      * Myers' rule for the new node's jump, with `this` as its parent: if the parent's jump and the
      * jump after it span the same number of nodes, the new node jumps over both (to `j2`);
      * otherwise it jumps to its parent. The spans are 2^k - 1 nodes long and nest like the digits
      * of a skew-binary number, so from any node a search for an ancestor at a given depth takes
      * O(log n) jumps and parent steps.
      */
    def extend(name: String, value: CekValue): CekEnv = {
        val j1 = this.jump
        val j2 = j1.jump
        val jump = if depth - j1.depth == j1.depth - j2.depth then j2 else this
        new CekEnv(name, value, this, jump, depth + 1)
    }

    /** The node at `target` depth, 1 <= target <= depth. Takes a jump when it does not go above
      * `target`, otherwise one parent step.
      */
    private def nodeAt(target: Int): CekEnv = {
        var n = this
        while n.depth != target do
            if n.jump.depth >= target then n = n.jump
            else n = n.parent.nn
        n
    }

    /** The value of de Bruijn index `index` (1 = the newest binding, at depth `depth`; index k is
      * at depth `depth - k + 1`), or null when the environment has no such binding.
      */
    def lookup(index: Int): CekValue | Null =
        if index <= 0 || index > depth then null
        else if index == 1 then value
        else nodeAt(depth - index + 1).value

    // The `Seq` view, oldest binding first, as the `ArraySeq` was: error messages and
    // `dischargeCekValue` use it. The machine itself uses only `extend` and `lookup`.
    def length: Int = depth
    def apply(i: Int): (String, CekValue) =
        if i < 0 || i >= depth then throw new IndexOutOfBoundsException(i.toString)
        val n = nodeAt(i + 1)
        (n.name, n.value.nn)
    def iterator: Iterator[(String, CekValue)] = Iterator.range(0, depth).map(apply)
}

private[eval] object CekEnv {
    val empty: CekEnv = new CekEnv("", null, null, null, 0)
}
