package scalus.uplc.transform

import scalus.uplc.Term
import scalus.uplc.Term.*

import java.util.Arrays
import scala.collection.mutable

/** Numbers terms by alpha-equivalence, so that CSE compares candidate keys as ints.
  *
  * Two terms get the same number iff they are equal up to the names of the binders inside them and
  * up to annotations; free variables compare by name. This is the equality of
  * [[CommonSubexpressionElimination.AlphaTermKey]]: `lam x. f x` and `lam y. f y` get one number,
  * `f x` and `f y` get two.
  *
  * Numbers are built bottom-up from the children's, so a term costs time proportional to its
  * children and its free variables, once per term instance. Later CSE rounds reuse the numbers of
  * the subtrees that an extraction did not rebuild.
  *
  * A term is a ''shape'' plus the list of its free names in order of first occurrence. The shape
  * stands each free variable in by its position in that list:
  *
  * {{{
  * f x f         names [f, x]   shape: Apply(Apply(slot 0, slot 1), slot 0)
  * g y g         names [g, y]   same shape, other names: another number
  * lam x. f x    names [f]      shape: Lam(x is slot 1 of the body, Apply(slot 0, slot 1))
  * lam y. f y    names [f]      same shape and names: the same number
  * }}}
  *
  * A node's shape is interned from its kind, its own data (constant, builtin, constructor tag), the
  * shapes of its children and, for each child, where the child's names go in the node's list. Not
  * thread-safe: create one instance per CSE run.
  */
private[transform] final class AlphaIds {
    import AlphaIds.*

    private val infos = new java.util.IdentityHashMap[Term, Info]()
    private val shapes = mutable.HashMap.empty[ShapeKey, Int]
    private val ids = mutable.HashMap.empty[IdKey, Int]

    /** The alpha-equivalence class number of `t`. */
    def apply(t: Term): Int = info(t).id

    private def info(t: Term): Info = {
        val cached = infos.get(t)
        if cached != null then cached
        else
            val result = t match
                case Var(name, _) => make(VarKind, null, NoInts, Array(name.name))
                case Const(c, _)  => make(ConstKind, c, NoInts, NoNames)
                case Builtin(bn, _) =>
                    make(BuiltinKind, Integer.valueOf(bn.ordinal), NoInts, NoNames)
                case Error(_) => make(ErrorKind, null, NoInts, NoNames)
                case Delay(body, _) =>
                    val b = info(body)
                    make(DelayKind, null, Array(b.shape), b.names)
                case Force(body, _) =>
                    val b = info(body)
                    make(ForceKind, null, Array(b.shape), b.names)
                case LamAbs(name, body, _) =>
                    // The body's slot of the parameter becomes bound; -1 if the body ignores it.
                    // lam x. f x: body names [f, x], so slot 1 is bound and the lambda keeps [f].
                    val b = info(body)
                    val slot = b.names.indexOf(name)
                    val names =
                        if slot < 0 then b.names
                        else b.names.patch(slot, Nil, 1)
                    make(LamKind, null, Array(b.shape, slot), names)
                case Apply(f, arg, _)       => combine(ApplyKind, null, List(f, arg))
                case Constr(tag, args, _)   => combine(ConstrKind, tag, args)
                case Case(scrutinee, bs, _) => combine(CaseKind, null, scrutinee :: bs)
            infos.put(t, result)
            result
    }

    /** Merges the children's names in order of first occurrence. For each child, records its shape
      * and the slot that each of its names got. In `Constr(0, [x, x])` the second x goes to slot 0;
      * in `Constr(0, [x, y])` the y goes to slot 1. So the two shapes differ.
      */
    private def combine(kind: Int, payload: AnyRef, children: List[Term]): Info = {
        val childInfos = children.map(info)
        val names = mutable.ArrayBuffer.empty[String]
        val slotOf = mutable.HashMap.empty[String, Int]
        val ints = mutable.ArrayBuffer.empty[Int]
        for c <- childInfos do
            ints += c.shape
            for n <- c.names do ints += slotOf.getOrElseUpdate(n, { names += n; names.size - 1 })
        make(kind, payload, ints.toArray, names.toArray)
    }

    private def make(kind: Int, payload: AnyRef, ints: Array[Int], names: Array[String]): Info = {
        // A shape fixes how many free slots its term has, so `ints` decodes unambiguously.
        val shape = shapes.getOrElseUpdate(new ShapeKey(kind, payload, ints), shapes.size)
        val id = ids.getOrElseUpdate(new IdKey(shape, names), ids.size)
        new Info(shape, names, id)
    }
}

private object AlphaIds {
    private val VarKind = 0
    private val ConstKind = 1
    private val BuiltinKind = 2
    private val ErrorKind = 3
    private val DelayKind = 4
    private val ForceKind = 5
    private val LamKind = 6
    private val ApplyKind = 7
    private val ConstrKind = 8
    private val CaseKind = 9

    private val NoInts = Array.emptyIntArray
    private val NoNames = Array.empty[String]

    private final class Info(val shape: Int, val names: Array[String], val id: Int)

    /** `payload` is the node's own data: a constant, a builtin's ordinal or a constructor tag. */
    private final class ShapeKey(val kind: Int, val payload: AnyRef, val ints: Array[Int]) {
        override def equals(that: Any): Boolean = that match
            case o: ShapeKey =>
                kind == o.kind && payload == o.payload && Arrays.equals(ints, o.ints)
            case _ => false
        override def hashCode(): Int =
            (kind * 31 + (if payload == null then 0 else payload.hashCode)) * 31 +
                Arrays.hashCode(ints)
    }

    private final class IdKey(val shape: Int, val names: Array[String]) {
        override def equals(that: Any): Boolean = that match
            case o: IdKey =>
                shape == o.shape && Arrays.equals(
                  names.asInstanceOf[Array[AnyRef]],
                  o.names.asInstanceOf[Array[AnyRef]]
                )
            case _ => false
        override def hashCode(): Int =
            shape * 31 + Arrays.hashCode(names.asInstanceOf[Array[AnyRef]])
    }
}
