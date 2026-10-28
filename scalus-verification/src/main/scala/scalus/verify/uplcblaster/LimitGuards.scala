package scalus.verify.uplcblaster

import scalus.uplc.{DefaultFun, Term}
import scalus.uplc.Term.{asTerm, lam, vr}
import scalus.verify.{DataLayout, Limit, Limits, Step}

/** The program that says whether a variable's `Data` meets its limits
  * ([[scalus.verify.Props.forAllDataLimited]]): the guard of every test in the variable's scope.
  *
  * It reads the `Data` as compiled code does, with `unConstrData`, `sndPair`, `headList` and
  * `tailList`, so that Blaster relates what it established of a list to each read of the list by
  * another program ([[DropLists]]). A list's length is checked by a walk that ends: it looks at a
  * cell more than the most the list may have, or as many as the least. Each step of a walk that
  * Lean takes for a list it does not know takes the list apart into its empty list and a cell, so a
  * test that checks the guard first runs on lists of the lengths the guard allows.
  *
  * The guard may check less than the limits say, and never more: a guard that held of fewer values
  * than the limits would prove the statement of fewer values than it claims. So it does not look
  * into the constructors of a type of several, nor at the elements of a list that has no most.
  */
private[uplcblaster] object LimitGuards {

    /** The most elements a limit may bound a collection to: the guard looks at every cell up to it.
      */
    val maxBound = 64

    /** The most variables a guard may have. The walks of nested collections multiply: a list of at
      * most 64 lists of at most 64 elements is a walk of 4096 cells.
      */
    val maxVariables = 20_000

    /** The guard of `limits` of a variable laid out as `layout`, as a program from the variable's
      * `Data` to whether it meets them, or why there is none: a bound larger than [[maxBound]], or
      * a guard larger than [[maxVariables]]. It is `true` of every value where it checks nothing.
      */
    def guard(layout: DataLayout, limits: Limits): Either[String, Term] = {
        val bounds = limits.all.flatMap {
            case Limit.MaxCollectionLength(max) => List(max)
            case Limit.Length(_, min, max)      => min :: max.toList
        }
        bounds.find(_ > maxBound) match
            case Some(bound) =>
                Left(
                  s"a limit of $bound elements is more than the $maxBound a guard walks: it looks " +
                      "at every cell up to its bound"
                )
            case None =>
                val guarding = new Guarding(limits)
                val data = guarding.fresh("data")
                val body = guarding.check(layout, vr(data), Some(Nil)).getOrElse(true.asTerm)
                if guarding.variables > maxVariables then
                    Left(
                      s"the guard of the limits has ${guarding.variables} variables, more than " +
                          s"$maxVariables: the walks of nested collections multiply"
                    )
                else Right(lam(data)(body))
    }

    /** `test` and then `rest`, where `rest` runs only where `test` is `true`. */
    private[uplcblaster] def and(test: Term, rest: Term): Term =
        ifThenElse(test, rest, false.asTerm)

    /** `whenTrue` where `condition` is `true` and `whenFalse` where it is not: only one of them
      * runs.
      */
    private[uplcblaster] def ifThenElse(condition: Term, whenTrue: Term, whenFalse: Term): Term =
        !(DropLists.forced(DefaultFun.IfThenElse, 1) $ condition $ ~whenTrue $ ~whenFalse)

    private def builtin(function: DefaultFun, forces: Int, argument: Term): Term =
        DropLists.forced(function, forces) $ argument

    private final class Guarding(limits: Limits) {

        /** The variables written so far, which number the next one. */
        var variables = 0

        def fresh(what: String): String = {
            variables += 1
            s"scalus_limit_${what}_$variables"
        }

        /** `body` with a variable bound to `value`, which is evaluated once. */
        private def let(what: String, value: Term)(body: Term => Term): Term = {
            val name = fresh(what)
            lam(name)(body(vr(name))) $ value
        }

        private val everyCollection =
            limits.all.exists(_.isInstanceOf[Limit.MaxCollectionLength])

        private val limitedPaths = limits.all.collect { case Limit.Length(path, _, _) => path }

        /** Whether a limit bounds a collection at `path` or under it. Under `None`, inside the
          * constructors of a type of several, the guard does not look.
          */
        private def limitedUnder(path: Option[List[Step]]): Boolean = path match
            case None        => false
            case Some(steps) => everyCollection || limitedPaths.exists(_.startsWith(steps))

        /** Whether `value`, laid out as `layout`, at `path`, meets the limits: a program of a
          * Boolean, or none where it checks nothing there.
          */
        def check(layout: DataLayout, value: Term, path: Option[List[Step]]): Option[Term] = {
            if !limitedUnder(path) then None
            else
                val at = path.getOrElse(Nil)
                layout match {
                    case DataLayout.Record(fields) => record(fields, value, at)
                    case DataLayout.Wrapper(name, inner) =>
                        check(inner, value, Some(at :+ Step.Field(name)))
                    case DataLayout.Collection(items) =>
                        walk(builtin(DefaultFun.UnListData, 0, value), at) { cell =>
                            val item = builtin(DefaultFun.HeadList, 1, cell)
                            check(items, item, Some(at :+ Step.Elements))
                        }
                    case DataLayout.Mapping(keys, values, _) =>
                        walk(builtin(DefaultFun.UnMapData, 0, value), at) { cell =>
                            val entry = builtin(DefaultFun.HeadList, 1, cell)
                            val key = builtin(DefaultFun.FstPair, 2, entry)
                            val item = builtin(DefaultFun.SndPair, 2, entry)
                            (check(keys, key, Some(at :+ Step.Keys)) ++
                                check(values, item, Some(at :+ Step.Values))).reduceOption(and)
                        }
                    case DataLayout.Choice(_) | DataLayout.Plain(_) => None
                }
        }

        /** The fields of a case class that limits bound: each the head of the fields' list after as
          * many tails as fields before it, as compiled code reads a field. None where none of them
          * has a check.
          */
        private def record(
            fields: List[(String, DataLayout)],
            value: Term,
            path: List[Step]
        ): Option[Term] = {
            val list = fresh("fields")
            val checks = fields.zipWithIndex.flatMap { case ((name, field), index) =>
                val cells = (0 until index).foldLeft(vr(list))((cell, _) =>
                    builtin(DefaultFun.TailList, 1, cell)
                )
                check(field, builtin(DefaultFun.HeadList, 1, cells), Some(path :+ Step.Field(name)))
            }
            checks.reduceOption(and).map { body =>
                val read =
                    builtin(DefaultFun.SndPair, 2, builtin(DefaultFun.UnConstrData, 0, value))
                lam(list)(body) $ read
            }
        }

        /** Whether the builtin list `list` of a collection at `path` has as many elements as its
          * bounds allow, and each meets `element`'s check, from a cell of the list. The walk looks
          * at a cell more than the most, or at as many as the least where the list has no most, and
          * then at no elements.
          */
        private def walk(list: Term, path: List[Step])(
            element: Term => Option[Term]
        ): Option[Term] = {
            val (least, most) = limits.bounds(Some(path))
            def cells(cell: Term, before: Int): Term = {
                val rest = most match
                    case Some(max) if before == max => false.asTerm
                    case None if before >= least    => true.asTerm
                    case _ =>
                        val next =
                            let("cell", builtin(DefaultFun.TailList, 1, cell))(cells(_, before + 1))
                        most.flatMap(_ => element(cell)).fold(next)(and(_, next))
                ifThenElse(builtin(DefaultFun.NullList, 1, cell), (before >= least).asTerm, rest)
            }
            Option.when(least > 0 || most.isDefined)(let("list", list)(cells(_, 0)))
        }
    }
}
