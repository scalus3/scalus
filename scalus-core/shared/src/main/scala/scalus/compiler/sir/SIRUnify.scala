package scalus.compiler.sir

import scalus.compiler.sir.SIR.Pattern

import java.util
import scala.annotation.tailrec

object SIRUnify {

    case class Env(
        path: List[String] = List.empty,
        filledTypes: Map[SIRType.TypeVar, SIRType] = Map.empty,
        eqTypes: Map[SIRType.TypeVar, Set[SIRType.TypeVar]] = Map.empty,
        parentTypes: Map[SIRType, Set[SIRType]] = Map.empty,
        topLevelTypes: Set[SIRType] = Set.empty,
        varNames: Map[String, String] = Map.empty,
        debug: Boolean = false,
        upcasting: Boolean = false,
        var optTypeVarGenerationContext: Option[SIRType.SetBasedTypeVarGenerationContext] = None,
        deepDeclCheck: Boolean = false
    ) {
        def withDebug: Env = this.copy(debug = true)
        def setDebug(debug: Boolean): Env =
            this.copy(debug = debug)
        def withoutUpcasting: Env = this.copy(upcasting = false)
        def withUpcasting: Env = this.copy(upcasting = true)
        def withPath(path: String): Env = this.copy(path = path :: this.path)
        def withTopleveTypes(tps: SIRType*): Env =
            this.copy(topLevelTypes = tps.toSet, optTypeVarGenerationContext = None)

    }

    object Env {
        def empty: Env = Env()
        def topLevel(tps: SIRType*): Env = empty.withTopleveTypes(tps*)
    }

    sealed trait UnificationResult[+T] {

        def isSuccess: Boolean = this match {
            case UnificationSuccess(_, _)    => true
            case UnificationFailure(_, _, _) => false
        }

        def isFailure: Boolean = !isSuccess

        def map[U](f: T => U): UnificationResult[U] = this match {
            case UnificationSuccess(env, unificator)   => UnificationSuccess(env, f(unificator))
            case UnificationFailure(path, left, right) => UnificationFailure(path, left, right)
        }

        def flatMap[U](f: T => UnificationResult[U]): UnificationResult[U] = this match {
            case UnificationSuccess(env, unificator)   => f(unificator)
            case UnificationFailure(path, left, right) => UnificationFailure(path, left, right)
        }

    }

    object UnificationResult {

        def pure[A](a: A): UnificationResult[A] = UnificationSuccess(Env.empty, a)

    }

    case class UnificationSuccess[T](env: Env, unificator: T) extends UnificationResult[T]
    case class UnificationFailure[S](
        path: List[String],
        left: S,
        right: S,
    ) extends UnificationResult[Nothing] {
        val createEx = new RuntimeException("Unification failure created at")
    }

    trait Unify[T] {
        def apply(left: T, right: T, env: Env): UnificationResult[T]
    }

    object Unify {

        given Unify[String] with {
            def apply(left: String, right: String, env: Env): UnificationResult[String] = {
                if left == right then UnificationSuccess(env, left)
                else UnificationFailure(env.path, left, right)
            }
        }

    }

    extension [T: Unify](left: T)
        def ~=~(right: T): UnificationResult[T] = summon[Unify[T]].apply(left, right, Env.empty)
        def ~~=~~(right: T): Boolean =
            summon[Unify[T]].apply(left, right, Env(debug = true)).isSuccess

    def unifySIR(left: SIR, right: SIR, env: Env): UnificationResult[SIR] = {
        (left, right) match
            case (l: AnnotatedSIR, r: AnnotatedSIR) =>
                unifySIRExpr(l, r, env)
            case (l: SIR.Decl, r: SIR.Decl) =>
                unifyDecl(l, r, env)
            case _ =>
                UnificationFailure(env.path, left, right)
    }

    /** Unify two SIR expressions. Annotations are ignored during unification and can be different.
      */
    given Unify[SIR] with {
        def apply(left: SIR, right: SIR, env: Env): UnificationResult[SIR] = {
            unifySIR(left, right, env)
        }
    }

    def unifySIRExpr(
        left: AnnotatedSIR,
        right: AnnotatedSIR,
        env: Env
    ): UnificationResult[AnnotatedSIR] = {
        (left, right) match
            case (SIR.Var(name1, tp1, anns1), SIR.Var(name2, tp2, anns2)) =>
                if env.debug then println(s"unifySIRExpr: vars: \nleft=$left\nright=$right")
                val (env1, sameNames) = env.varNames.get(name1) match
                    case Some(value1) =>
                        if value1 == name2 then (env, true)
                        else (env, false)
                    case None =>
                        (env.copy(varNames = env.varNames.updated(name1, name2)), true)
                if sameNames then
                    topLevelUnifyType(tp1, tp2, env1.copy(path = "tp" :: env1.path)) match
                        case UnificationSuccess(env2, tp) =>
                            UnificationSuccess(
                              env2.copy(path = env1.path, parentTypes = Map.empty),
                              SIR.Var(name1, tp, anns1)
                            )
                        case UnificationFailure(path, tpLeft, tpRight) =>
                            UnificationFailure(path, left, right)
                else UnificationFailure("name" :: env.path, left, right)
            case (v1: SIR.ExternalVar, v2: SIR.ExternalVar) =>
                if v1.name == v2.name && v1.moduleName == v2.moduleName then
                    topLevelUnifyType(v1.tp, v2.tp, env.copy(path = "tp" :: env.path)) match
                        case UnificationSuccess(env1, tp) =>
                            UnificationSuccess(
                              env1.copy(path = env.path, parentTypes = Map.empty),
                              SIR.ExternalVar(v1.name, v1.moduleName, tp, v1.anns)
                            )
                        case UnificationFailure(path, tpLeft, tpRight) =>
                            UnificationFailure(path, left, right)
                else UnificationFailure(env.path, left, right)
            case (v1: SIR.Let, v2: SIR.Let) =>
                unifyList(
                  v1.bindings,
                  v2.bindings,
                  env.copy(path = "binding" :: env.path)
                ) match
                    case UnificationSuccess(env1, bindings) =>
                        unifySIR(
                          v1.body,
                          v2.body,
                          env1.copy(path = "body" :: env.path)
                        ) match
                            case UnificationSuccess(env2, body) =>
                                if v1.flags != v2.flags then
                                    println("warning: let flags are different during unification")
                                val flags = v1.flags & v2.flags
                                UnificationSuccess(
                                  env2,
                                  SIR.Let(bindings, body, flags, v1.anns)
                                )
                            case UnificationFailure(path, bodyLeft, bodyRight) =>
                                UnificationFailure(path, bodyLeft, bodyRight)
                    case UnificationFailure(path, bindingsLeft, bindingsRight) =>
                        UnificationFailure(path, bindingsLeft, bindingsRight)
            case (v1: SIR.LamAbs, v2: SIR.LamAbs) =>
                // note, that this is not semantic unification, but syntactic
                //  so, different parametes means different lambdas.
                unifySIRExpr(v1.param, v2.param, env.copy()) match
                    case UnificationSuccess(env1, param) =>
                        val nParam = param.asInstanceOf[SIR.Var]
                        unifyList(
                          v1.typeParams,
                          v2.typeParams,
                          env1.copy(path = "typeParams" :: env.path)
                        )(using TypeVarRelaxingUnify) match
                            case UnificationSuccess(env2, typeParams) =>
                                unifySIR(
                                  v1.term,
                                  v2.term,
                                  env2.copy(path = "body" :: env.path)
                                ) match
                                    case UnificationSuccess(env2, term) =>
                                        UnificationSuccess(
                                          env2.copy(path = env.path),
                                          SIR.LamAbs(nParam, term, typeParams, v1.anns)
                                        )
                                    case failure: UnificationFailure[?] => failure
                            case failure: UnificationFailure[?] => failure
                    case failure: UnificationFailure[?] => failure
            case (app1: SIR.Apply, app2: SIR.Apply) =>
                unifySIRExpr(app1.f, app2.f, env.copy(path = "f" :: env.path)) match
                    case UnificationSuccess(env1, fun) =>
                        unifySIRExpr(app1.arg, app2.arg, env1.copy(path = "arg" :: env.path)) match
                            case UnificationSuccess(env2, arg) =>
                                unifyType(
                                  app1.tp,
                                  app2.tp,
                                  env2.copy(path = "tp" :: env.path)
                                ) match
                                    case UnificationSuccess(env3, tp) =>
                                        UnificationSuccess(
                                          env3.copy(path = env.path, parentTypes = Map.empty),
                                          SIR.Apply(fun, arg, tp, app1.anns)
                                        )
                                    case UnificationFailure(path, tpLeft, tpRight) =>
                                        UnificationFailure(path, left, right)
                            case failure @ UnificationFailure(path, argLeft, argRight) => failure
                    case failure @ UnificationFailure(path, funLeft, funRight) => failure
            case (s1: SIR.Select, s2: SIR.Select) =>
                unifySIR(s1.scrutinee, s2.scrutinee, env.copy(path = "scrutinee" :: env.path)) match
                    case UnificationSuccess(env1, scrutinee) =>
                        if s1.field == s2.field then
                            unifyType(s1.tp, s2.tp, env.copy(path = "tp" :: env.path)) match
                                case UnificationSuccess(env2, tp) =>
                                    UnificationSuccess(
                                      env1,
                                      SIR.Select(scrutinee, s1.field, s1.tp, s1.anns)
                                    )
                                case UnificationFailure(path, tpLeft, tpRight) =>
                                    UnificationFailure(path, left, right)
                        else UnificationFailure(env.path, left, right)
                    case failure @ UnificationFailure(path, leftLeft, leftRight) => failure
            case (c1: SIR.Const, c2: SIR.Const) =>
                if c1.uplcConst == c2.uplcConst then UnificationSuccess(env, c1)
                else UnificationFailure(env.path, left, right)
            case (v1: SIR.And, v2: SIR.And) =>
                unifySIRExpr(v1.a, v2.a, env.copy(path = "a" :: env.path)) match
                    case UnificationSuccess(env1, a) =>
                        unifySIRExpr(v1.b, v2.b, env1.copy(path = "b" :: env.path)) match
                            case UnificationSuccess(env2, b) =>
                                UnificationSuccess(env2, SIR.And(a, b, v1.anns))
                            case failure @ UnificationFailure(path, rightLeft, rightRight) =>
                                failure
                    case failure @ UnificationFailure(path, leftLeft, leftRight) => failure
            case (v1: SIR.Or, v2: SIR.Or) =>
                unifySIRExpr(v1.a, v2.a, env.copy(path = "a" :: env.path)) match
                    case UnificationSuccess(env1, a) =>
                        unifySIRExpr(v1.b, v2.b, env1.copy(path = "b" :: env.path)) match
                            case UnificationSuccess(env2, b) =>
                                UnificationSuccess(env2, SIR.Or(a, b, v1.anns))
                            case failure: UnificationFailure[?] => failure
                    case failure: UnificationFailure[?] => failure
            case (v1: SIR.Not, v2: SIR.Not) =>
                unifySIRExpr(v1.a, v2.a, env.copy(path = "a" :: env.path)) match
                    case UnificationSuccess(env1, a) =>
                        UnificationSuccess(env1.copy(path = env.path), SIR.Not(a, v1.anns))
                    case failure: UnificationFailure[?] => failure
            case (v1: SIR.IfThenElse, v2: SIR.IfThenElse) =>
                unifySIRExpr(v1.cond, v2.cond, env.copy(path = "cond" :: env.path)) match
                    case UnificationSuccess(env1, cond) =>
                        unifySIRExpr(v1.t, v2.t, env1.copy(path = "t" :: env.path)) match
                            case UnificationSuccess(env2, t) =>
                                unifySIRExpr(v1.f, v2.f, env2.copy(path = "f" :: env.path)) match
                                    case UnificationSuccess(env3, f) =>
                                        unifyType(
                                          v1.tp,
                                          v2.tp,
                                          env3.copy(path = "tp" :: env.path)
                                        ) match
                                            case UnificationSuccess(env4, tp) =>
                                                UnificationSuccess(
                                                  env4.copy(
                                                    path = env.path,
                                                    parentTypes = Map.empty
                                                  ),
                                                  SIR.IfThenElse(cond, t, f, tp, v1.anns)
                                                )
                                            case failure @ UnificationFailure(path, left, right) =>
                                                failure
                                    case failure @ UnificationFailure(
                                          path,
                                          rightLeft,
                                          rightRight
                                        ) =>
                                        failure
                            case failure @ UnificationFailure(path, rightLeft, rightRight) =>
                                failure
                    case failure @ UnificationFailure(path, leftLeft, leftRight) => failure
            case (v1: SIR.Builtin, v2: SIR.Builtin) =>
                if v1.bn == v2.bn then UnificationSuccess(env, v1)
                else UnificationFailure(env.path, left, right)
            case (e1: SIR.Error, e2: SIR.Error) =>
                unifySIRExpr(e1.msg, e2.msg, env.copy(path = "msg" :: env.path))
            case (c1: SIR.Constr, c2: SIR.Constr) =>
                if c1.name == c2.name then
                    if c1.args.length != c2.args.length then
                        UnificationFailure(env.path, left, right)
                    else
                        val nEnv = env.copy(path = "args" :: env.path)
                        unifyList(c1.args, c2.args, nEnv) match
                            case UnificationSuccess(env1, args) =>
                                unifyDataDecl(
                                  c1.data,
                                  c2.data,
                                  env1.copy(path = "data" :: env.path)
                                ) match
                                    case UnificationSuccess(env2, data) =>
                                        UnificationSuccess(
                                          env2.copy(path = env.path),
                                          SIR.Constr(c1.name, data, args, c1.tp, c1.anns)
                                        )
                                    case failure @ UnificationFailure(path, left, right) =>
                                        failure
                            case failure @ UnificationFailure(path, left, right) => failure
                else UnificationFailure(env.path, left, right)
            case (m1: SIR.Match, m2: SIR.Match) =>
                unifySIRExpr(
                  m1.scrutinee,
                  m2.scrutinee,
                  env.copy(path = "scrutinee" :: env.path)
                ) match
                    case UnificationSuccess(env1, expr) =>
                        unifyCases(m1.cases, m2.cases, env1.copy(path = "cases" :: env.path)) match
                            case UnificationSuccess(env2, cases) =>
                                unifyType(m1.tp, m2.tp, env2.copy(path = "tp" :: env.path)) match
                                    case UnificationSuccess(env3, tp) =>
                                        UnificationSuccess(
                                          env3.copy(path = env.path),
                                          SIR.Match(expr, cases, tp, m1.anns)
                                        )
                                    case failure @ UnificationFailure(path, left, right) =>
                                        failure
                            case failure: UnificationFailure[?] => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (cLeft: SIR.Cast, cRight: SIR.Cast) =>
                unifySIRExpr(cLeft.term, cRight.term, env.copy(path = "term" :: env.path)) match
                    case UnificationSuccess(env1, term) =>
                        topLevelUnifyType(
                          cLeft.tp,
                          cRight.tp,
                          env1.copy(path = "tp" :: env.path)
                        ) match
                            case UnificationSuccess(env2, tp) =>
                                UnificationSuccess(
                                  env2.copy(
                                    path = env.path,
                                    topLevelTypes = Set.empty,
                                    optTypeVarGenerationContext = None
                                  ),
                                  SIR.Cast(term, tp, cLeft.anns)
                                )
                            case failure @ UnificationFailure(path, left, right) =>
                                failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case _ =>
                UnificationFailure(env.path, left, right)
    }

    def checkEqType(env: Env, k: SIRType.TypeVar, v: SIRType): UnificationResult[SIRType] = {

        def check(
            env: Env,
            k: SIRType.TypeVar,
            processed: Set[SIRType.TypeVar],
            rest: Set[SIRType.TypeVar]
        ): UnificationResult[SIRType] = {
            if processed.contains(k) then UnificationSuccess(env, v)
            else
                env.eqTypes.get(k) match
                    case None =>
                        rest.headOption match {
                            case Some(nextK) =>
                                env.filledTypes.get(nextK) match
                                    case Some(nextV) =>
                                        val nEnv = env.copy(filledTypes =
                                            env.filledTypes.updated(nextK, v)
                                        )
                                        unifyType(v, nextV, nEnv) match
                                            case UnificationSuccess(env1, v1) =>
                                                check(env1, nextK, processed + k, rest - nextK)
                                            case failure @ UnificationFailure(path, left, right) =>
                                                failure
                                    case None =>
                                        val nEnv = env.copy(filledTypes =
                                            env.filledTypes.updated(nextK, v)
                                        )
                                        check(nEnv, nextK, processed + k, rest - nextK)
                            case None =>
                                UnificationSuccess(env, v)
                        }
                    case Some(kEqSeq) =>
                        val nEnv = env.copy(eqTypes = env.eqTypes - k)
                        check(nEnv, k, processed, kEqSeq ++ rest)

        }

        check(env, k, Set.empty, Set.empty)
    }

    given Unify[SIRType] with {
        def apply(left: SIRType, right: SIRType, env: Env): UnificationResult[SIRType] = {
            unifyType(left, right, env)
        }
    }

    def topLevelUnifyType(left: SIRType, right: SIRType, env: Env): UnificationResult[SIRType] = {
        unifyType(left, right, env.copy(topLevelTypes = Set(left, right)))
    }

    private def unifyType(left: SIRType, right: SIRType, env: Env): UnificationResult[SIRType] = {
        if env.debug then {
            println(
              s"unifyType: \nleft=${left.show}\nright=${right.show}, env.upcasting=${env.upcasting}"
            )
        }
        val retval =
            if left eq right then UnificationSuccess(env, left)
            else
                env.parentTypes.get(left) match
                    case Some(parentsRight) =>
                        if parentsRight.contains(right) then
                            // Too optimistics ?
                            //
                            UnificationSuccess(env, right)
                        else
                            val nEnv = env.copy(parentTypes =
                                env.parentTypes.updated(left, parentsRight + right)
                            )
                            unifyTypeNoRec(left, right, nEnv)
                    case None =>
                        env.parentTypes.get(right) match
                            case Some(parentsLeft) =>
                                if parentsLeft.contains(left) then UnificationSuccess(env, left)
                                else
                                    val nEnv = env.copy(
                                      parentTypes = env.parentTypes
                                          .updated(left, Set(right))
                                          .updated(right, parentsLeft + left)
                                    )
                                    unifyTypeNoRec(left, right, nEnv)
                            case None =>
                                val nEnv = env.copy(
                                  parentTypes = env.parentTypes
                                      .updated(left, Set(right))
                                      .updated(right, Set(left))
                                )
                                unifyTypeNoRec(left, right, nEnv)
        if env.debug then println(s"return unifyType:\nleft=$left\nright=$right\nretval=$retval")
        retval
    }

    private def unifyTypeNoRec(
        left: SIRType,
        right: SIRType,
        env: Env
    ): UnificationResult[SIRType] = {
        (left, right) match
            case (v1: SIRType.TypeVar, v2: SIRType.TypeVar) =>
                if env.debug then
                    println(
                      s"unifyTypeNoRec: TypeVars, v1=${v1}, v2=${v2}, v1.filled=${env.filledTypes.get(v1)}, v2.filled=${env.filledTypes.get(v2)}"
                    )
                env.filledTypes.get(v1) match
                    case Some(tp1) =>
                        env.filledTypes.get(v2) match
                            case Some(tp2) =>
                                unifyType(tp1, tp2, env)
                            case None =>
                                val nFilledTypes = env.filledTypes.updated(v2, tp1)
                                checkEqType(env.copy(filledTypes = nFilledTypes), v2, tp1)
                    case None =>
                        env.filledTypes.get(v2) match
                            case Some(tp2) =>
                                val nFilledTypes = env.filledTypes.updated(v1, tp2)
                                checkEqType(env.copy(filledTypes = nFilledTypes), v1, tp2)
                            case None =>
                                val v1EqTypes = env.eqTypes.getOrElse(v1, Set.empty)
                                val v2EqTypes = env.eqTypes.getOrElse(v2, Set.empty)
                                val nEqTypes = env.eqTypes
                                    .updated(v1, v1EqTypes + v2)
                                    .updated(v2, v2EqTypes + v1)
                                UnificationSuccess(env.copy(eqTypes = nEqTypes), v1)
            case (v: SIRType.TypeVar, _) =>
                val nEnv = env.copy(filledTypes = env.filledTypes.updated(v, right))
                checkEqType(nEnv, v, right)
            case (_, v: SIRType.TypeVar) =>
                val nEnv = env.copy(filledTypes = env.filledTypes.updated(v, left))
                checkEqType(nEnv, v, left)
            case (pLeft: SIRType.Primitive, pRight: SIRType.Primitive) =>
                if pLeft == pRight then UnificationSuccess(env, pLeft)
                else UnificationFailure(env.path, left, right)
            case (ccLeft: SIRType.CaseClass, ccRight: SIRType.CaseClass) =>
                unifyConstrDecl(
                  ccLeft.constrDecl,
                  ccRight.constrDecl,
                  env.copy(path = "constrDecl" :: env.path)
                ) match
                    case UnificationSuccess(env1, constrDecl) =>
                        unifyList(
                          ccLeft.typeArgs,
                          ccRight.typeArgs,
                          env1.copy(path = "args" :: env.path)
                        ) match
                            case UnificationSuccess(env2, typeArgs) =>
                                ccLeft.parent match
                                    case None =>
                                        ccRight.parent match
                                            case None =>
                                                UnificationSuccess(
                                                  env2.copy(path = env.path),
                                                  SIRType.CaseClass(constrDecl, typeArgs, None)
                                                )
                                            case Some(rightParent) =>
                                                UnificationFailure(
                                                  "parent" :: env.path,
                                                  ccLeft.parent,
                                                  ccRight.parent
                                                )
                                    case Some(leftParent) =>
                                        ccRight.parent match
                                            case None =>
                                                UnificationFailure(
                                                  "parent" :: env.path,
                                                  ccLeft.parent,
                                                  ccRight.parent
                                                )
                                            case Some(ccRightParent) =>
                                                unifyType(
                                                  leftParent,
                                                  ccRightParent,
                                                  env2.copy(path = "parent" :: env.path)
                                                ) match
                                                    case UnificationSuccess(env3, parent) =>
                                                        UnificationSuccess(
                                                          env3.copy(path = env.path),
                                                          SIRType.CaseClass(
                                                            constrDecl,
                                                            typeArgs,
                                                            Some(parent)
                                                          )
                                                        )
                                                    case failure @ UnificationFailure(
                                                          path,
                                                          left,
                                                          right
                                                        ) =>
                                                        failure
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) =>
                        // if we are in upcasting mode, then try to find common parent type
                        if env.upcasting then
                            ccLeft.parent match
                                case Some(leftParent) =>
                                    ccRight.parent match
                                        case Some(rightParent) =>
                                            unifyType(
                                              leftParent,
                                              rightParent,
                                              env.copy(path = "parent" :: env.path)
                                            ) match
                                                case success @ UnificationSuccess(env1, parent) =>
                                                    UnificationSuccess(
                                                      env1.copy(path = env.path),
                                                      parent
                                                    )
                                                case failure: UnificationFailure[?] =>
                                                    failure
                                        case None => failure
                                case None => failure
                        else failure
            case (ccLeft: SIRType.CaseClass, ccRight: SIRType.SumCaseClass) =>
                if env.upcasting then
                    ccRight.decl.constructors.find(_.name == ccLeft.constrDecl.name) match
                        case Some(constrDecl) =>
                            unifyConstrDecl(
                              ccLeft.constrDecl,
                              constrDecl,
                              env.copy(path = "constrDecl" :: env.path)
                            ) match
                                case UnificationSuccess(env1, constrDecl) =>
                                    // for now we have no mapping between constructor tupe argument and parent type argument,
                                    // so for now just not check.
                                    //  In future, insert check here, when we will have full reflection of scala type hierechy
                                    UnificationSuccess(
                                      env1.copy(path = env.path),
                                      ccRight
                                    )
                                case failure @ UnificationFailure(path, left, right) => failure
                        case None =>
                            ccLeft.parent match
                                case Some(parent) =>
                                    if subtypeSeq(parent, ccRight, env).nonEmpty then
                                        UnificationSuccess(env, ccRight)
                                    else UnificationFailure(env.path, left, right)
                                case None => UnificationFailure(env.path, left, right)
                else UnificationFailure(env.path, left, right)
            case (ccLeft: SIRType.SumCaseClass, ccRight: SIRType.CaseClass) =>
                if env.upcasting then {
                    if env.debug then
                        println(
                          s"unifyTypeNoRec: SumCaseClass to CaseClass, ccLeft=${ccLeft}, ccRight=${ccRight}"
                        )
                    ccLeft.decl.constructors.find(_.name == ccRight.constrDecl.name) match
                        case Some(constrDecl) =>
                            unifyConstrDecl(
                              constrDecl,
                              ccRight.constrDecl,
                              env.copy(path = "constrDecl" :: env.path)
                            ) match
                                case UnificationSuccess(env1, constrDecl) =>
                                    // for now we have no mapping between constructor tupe argument and parent type argument,
                                    // so for now just not check.
                                    //  In future, insert check here, when we will have full reflection of scala type hierechy
                                    val nEnv =
                                        env1.copy(path = env.path, parentTypes = env.parentTypes)
                                    UnificationSuccess(nEnv, ccLeft)
                                case failure @ UnificationFailure(path, left, right) => failure
                        case None =>
                            UnificationFailure(env.path, left, right)
                } else UnificationFailure(env.path, left, right)
            case (ccLeft: SIRType.SumCaseClass, ccRight: SIRType.SumCaseClass) =>
                unifyDataDecl(ccLeft.decl, ccRight.decl, env.copy(path = "decl" :: env.path)) match
                    case UnificationSuccess(env1, decl) =>
                        unifyList(
                          ccLeft.typeArgs,
                          ccRight.typeArgs,
                          env1.copy(path = "args" :: env.path)
                        ) match
                            case UnificationSuccess(env2, typeArgs) =>
                                UnificationSuccess(
                                  env2.copy(path = env.path),
                                  SIRType.SumCaseClass(decl, typeArgs)
                                )
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) =>
                        if !env.upcasting then failure
                        else if subtypeSeq(ccLeft, ccRight, env).nonEmpty then
                            UnificationSuccess(env, ccRight)
                        else if subtypeSeq(ccRight, ccLeft, env).nonEmpty then
                            UnificationSuccess(env, ccLeft)
                        else failure // UnificationFailure(env.path, left, right)
            case (SIRType.Fun(inLeft, outLef), SIRType.Fun(inRight, outRight)) =>
                unifyType(inLeft, inRight, env.copy(path = "in" :: env.path)) match
                    case UnificationSuccess(env1, in) =>
                        unifyType(outLef, outRight, env1.copy(path = "out" :: env.path)) match
                            case UnificationSuccess(env2, out) =>
                                UnificationSuccess(env2.copy(path = env.path), SIRType.Fun(in, out))
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (SIRType.TypeLambda(params, body), right) =>
                if SIRType.isTypeVarsUsedIn(params, right) || typeVarsInEnv(params, env) then
                    val varGenerationContext =
                        retrieveTypeVarGenerationContext(env)
                    val renames = params.map(p => (p, varGenerationContext.freshCopy(p))).toMap
                    val renamingContext =
                        RenamingTypeVars.makeContext(renames, varGenerationContext)
                    val renamedBody = RenamingTypeVars.inType(body, renamingContext)
                    unifyType(renamedBody, right, env)
                else unifyType(body, right, env)
            case (left, SIRType.TypeLambda(params, body)) =>
                if SIRType.isTypeVarsUsedIn(params, left) || typeVarsInEnv(params, env) then
                    val varGenerationContext: SIRType.SetBasedTypeVarGenerationContext =
                        retrieveTypeVarGenerationContext(env)
                    val renames = params.map(p => (p, varGenerationContext.freshCopy(p))).toMap
                    val renamingContext =
                        RenamingTypeVars.makeContext(renames, varGenerationContext)
                    val renamedBody = RenamingTypeVars.inType(body, renamingContext)
                    unifyType(left, renamedBody, env)
                else unifyType(left, body, env)
            case (SIRType.FreeUnificator, right) =>
                UnificationSuccess(env, right)
            case (left, SIRType.FreeUnificator) =>
                UnificationSuccess(env, left)
            case (leftProxy: SIRType.TypeProxy, right) =>
                if leftProxy.ref == null then
                    // TODO: more appropriate error class
                    throw new RuntimeException("TypeProxy should be resolved before unification")
                else
                    env.parentTypes.get(leftProxy.ref) match
                        case Some(parentRight) =>
                            // we are in the cyclic loop.
                            if parentRight eq right then UnificationSuccess(env, right)
                            else
                                // can cause infinite loop. Mb add counter for max-level ?
                                // UnificationFailure("proxy-loop"::env.path, left, right)
                                unifyType(leftProxy.ref, right, env)
                        case None =>
                            unifyType(leftProxy.ref, right, env)
            case (left, rightProxy: SIRType.TypeProxy) =>
                if rightProxy.ref == null then
                    throw new RuntimeException("TypeProxy should be resolved before unification")
                else
                    env.parentTypes.get(rightProxy.ref) match
                        case Some(parentLeft) =>
                            if parentLeft eq left then UnificationSuccess(env, left)
                            else unifyType(left, rightProxy.ref, env)
                        case None =>
                            unifyType(left, rightProxy.ref, env)
            case (m1: SIRType.TypeNonCaseModule, m2: SIRType.TypeNonCaseModule) =>
                if m1.name == m2.name then UnificationSuccess(env, m1)
                else UnificationFailure(env.path, left, right)
            case (SIRType.TypeNothing, SIRType.TypeNothing) =>
                UnificationSuccess(env, SIRType.TypeNothing)
            case (SIRType.TypeNothing, _) =>
                if env.upcasting then UnificationSuccess(env, right)
                else UnificationFailure(env.path, left, right)
            case (_, SIRType.TypeNothing) =>
                if env.upcasting then UnificationSuccess(env, left)
                else UnificationFailure(env.path, left, right)
            case _ =>
                UnificationFailure(env.path, left, right)
    }

    /** return sequence, where the first element is the child type, the next elewent is the parent
      * typeof child (if exists) with free type arguments (because we don't track
      * covariance/contravariance in SIRType, so depeendencing can be any, and this up to
      * parentCandidate.)
      *
      * (Nil, List[Int]) => List(Nil, List[FreeUnificator])
      *
      * @param childCandidate
      * @param parentCandidate
      * @param env0
      * @return
      */
    def subtypeSeq(childCandidate: SIRType, parentCandidate: SIRType, env0: Env): List[SIRType] = {

        @tailrec
        def checkEqTypeVars(
            env: Env,
            x: SIRType.TypeVar,
            y: SIRType.TypeVar,
            processed: Set[SIRType.TypeVar],
            rest: Set[SIRType.TypeVar]
        ): Boolean = {
            if processed.contains(x) then true
            else
                env.eqTypes.get(x) match
                    case Some(xEqSet) =>
                        if xEqSet.contains(y) then true
                        else
                            checkEqTypeVars(
                              env.copy(eqTypes = env.eqTypes - x),
                              x,
                              y,
                              processed,
                              xEqSet ++ rest
                            )
                    case None =>
                        rest.headOption match
                            case Some(nextR) =>
                                checkEqTypeVars(env, nextR, y, processed + x, rest - nextR)
                            case None => false
        }

        val env = env0.withUpcasting

        (childCandidate, parentCandidate) match
            case (SIRType.TypeNothing, SIRType.TypeNothing) => List(SIRType.TypeNothing)
            case (SIRType.TypeNothing, _)                   => List(childCandidate, parentCandidate)
            case (_, SIRType.TypeNothing)                   => List.empty
            case (SIRType.FreeUnificator, SIRType.FreeUnificator) => List(SIRType.FreeUnificator)
            case (_, SIRType.FreeUnificator) => List(childCandidate, parentCandidate)
            case (SIRType.FreeUnificator, _) => List.empty
            case (childVar: SIRType.TypeVar, rightVar: SIRType.TypeVar) =>
                if rightVar == childVar then List(childVar)
                else if env.filledTypes.contains(childVar) then
                    subtypeSeq(env.filledTypes(childVar), rightVar, env)
                else if env.filledTypes.contains(rightVar) then
                    subtypeSeq(childVar, env.filledTypes(rightVar), env)
                else if env.eqTypes.contains(childVar) then
                    if checkEqTypeVars(env, childVar, rightVar, Set.empty, Set.empty) then
                        List(childVar)
                    else List.empty
                else if env.eqTypes.contains(rightVar) then
                    if checkEqTypeVars(env, rightVar, childVar, Set.empty, Set.empty) then
                        List(rightVar)
                    else List.empty
                else List.empty
            case (childVar: SIRType.TypeVar, rightType: SIRType) =>
                if env.filledTypes.contains(childVar) then
                    subtypeSeq(env.filledTypes(childVar), rightType, env)
                else List.empty
            case (p1: SIRType.Primitive, p2: SIRType.Primitive) =>
                if p1 == p2 then List(p1)
                else List.empty
            case (p1: SIRType.Primitive, _) => List.empty
            case (_, p2: SIRType.Primitive) => List.empty
            case (cc1: SIRType.CaseClass, cc2: SIRType.CaseClass) =>
                unifyConstrDecl(cc1.constrDecl, cc2.constrDecl, env) match
                    case UnificationSuccess(env, cc) =>
                        // we don't want introduce of contrvairance/covariance here, so we just change typeargd
                        // to FreeUnificator
                        val freeTypeArgs = cc.typeParams.map(v => (v, SIRType.FreeUnificator)).toMap
                        val u = SIRType.substitute(cc1, freeTypeArgs, Map.empty)
                        List(u)
                    case UnificationFailure(path, l, r) =>
                        List.empty
            case (cc1: SIRType.CaseClass, cc2: SIRType.SumCaseClass) =>
                cc1.parent match
                    case None =>
                        if env.debug then
                            println(s"subtypeSeq: no parent for CaseClass ${cc1.constrDecl.name}")
                        List.empty
                    case Some(parent) =>
                        subtypeSeq(parent, cc2, env) match
                            case Nil   => List.empty
                            case other => cc1 :: other
            case (ccLeft: SIRType.SumCaseClass, tlRight: SIRType.CaseClass) =>
                // Check if this is upcasting a sum case to its parent sum type
                // e.g. Order.Less <: Order
                // CaseClass has a parent field pointing to the sum class
                tlRight.parent match
                    case Some(parent) =>
                        unifyType(ccLeft, parent, env) match
                            case UnificationSuccess(_, _) =>
                                List(tlRight)
                            case UnificationFailure(_, _, _) =>
                                List.empty
                    case None =>
                        List.empty
            case (ccLeft: SIRType.SumCaseClass, ccRight: SIRType.SumCaseClass) =>
                unifyDataDecl(ccLeft.decl, ccRight.decl, env) match
                    case UnificationSuccess(env, decl1) =>
                        val freeTypeArgs =
                            decl1.typeParams.map(v => (v, SIRType.FreeUnificator)).toMap
                        val rt = decl1.tp match
                            case sm: SIRType.SumCaseClass =>
                                sm
                            case tpl: SIRType.TypeLambda =>
                                SIRType.substitute(tpl.body, freeTypeArgs, Map.empty)
                            case other =>
                                throw IllegalStateException(
                                  s"type of decl can be only SumCaseClass or TypeLambda, we have ${other.show}"
                                )
                        List(rt)
                    case UnificationFailure(_, _, _) =>
                        val childName = SIRType.syntheticNarrowConstrDeclName(ccLeft.decl.name)
                        val decls = findChildConstrForSum(ccRight.decl, childName)
                        if decls.isEmpty then List.empty
                        else {
                            val headDecl = decls.head
                            unifyType(ccLeft, headDecl.tp, env.withoutUpcasting) match
                                case UnificationSuccess(env1, ccLeft1) =>
                                    // here is approximation,
                                    // types will be type-labda, mb substirute types with free unificator
                                    ccLeft1 :: decls.tail.map(d => d.tp)
                                case UnificationFailure(path, l, r) =>
                                    if env.debug then
                                        println(
                                          s"subtypeSeq: UnificationFailure for ${ccLeft.decl.name} and ${ccRight.decl.name}"
                                        )
                                    List.empty
                        }

                // unifyType(ccLeft, ccRight, env.withoutUpcasting) match
                //    case UnificationSuccess(_, _) =>
                //        List(ccLeft)
                //    case UnificationFailure(_, _, _) =>
                //        val childName = SIRType.syntheticNarrowConstrDeclName(ccLeft.decl.name)
                //        val decls = findChildConstrForSum(ccRight.decl, childName)
                //        if decls.isEmpty then List.empty
                //        else {
                //            val headDecl = decls.head
                //            unifyType(ccLeft, headDecl.tp, env.withoutUpcasting) match
                //                case UnificationSuccess(env, ccLeft1) =>
                //                    // here is approximation,
                //                    // TODO:  track type arguments.
                //                    ccLeft1 :: decls.tail.map(d => d.tp)
                //                case UnificationFailure(_, _, _) =>
                //                    List.empty
                //        }
            case (SIRType.Fun(inLeft, outLeft), SIRType.Fun(inRight, outRight)) =>
                // Check if function types can be related through subtyping
                // Function types are contravariant in parameters and covariant in return type
                if env.debug then
                    println(
                      s"[subtypeSeq Fun] Checking ${SIRType.Fun(inLeft, outLeft).show} <: ${SIRType.Fun(inRight, outRight).show}"
                    )
                unifyType(inLeft, inRight, env.withoutUpcasting) match
                    case UnificationSuccess(env1, in) =>
                        unifyType(outLeft, outRight, env1) match
                            case UnificationSuccess(env2, out) =>
                                if env.debug then
                                    println(
                                      s"[subtypeSeq Fun] Types unify, returning [${SIRType.Fun(in, out).show}]"
                                    )
                                List(SIRType.Fun(in, out))
                            case UnificationFailure(_, _, _) =>
                                // Try subtyping on return types (covariant position)
                                val outSeq = subtypeSeq(outLeft, outRight, env)
                                if outSeq.nonEmpty then
                                    // Return type can be upcasted, so function type can be upcasted
                                    // Return both source and target so maybeUpcast can skip the first and upcast to the rest
                                    val result = List(
                                      SIRType.Fun(inLeft, outLeft),
                                      SIRType.Fun(inRight, outRight)
                                    )
                                    if env.debug then
                                        println(
                                          s"[subtypeSeq Fun] Return type can upcast, outSeq = ${outSeq.map(_.show)}"
                                        )
                                        println(s"[subtypeSeq Fun] Returning ${result.map(_.show)}")
                                    result
                                else
                                    if env.debug then
                                        println(
                                          s"[subtypeSeq Fun] Return type cannot upcast, returning empty"
                                        )
                                    List.empty
                    case UnificationFailure(_, _, _) =>
                        if env.debug then
                            println(s"[subtypeSeq Fun] Parameters don't unify, returning empty")
                        List.empty
            case (leftProxy: SIRType.TypeProxy, right) =>
                if leftProxy.ref == null then
                    throw new RuntimeException("TypeProxy should be resolved before unification")
                else
                    // since we have no F-bounded polymorphism, we can just unroll proxies
                    unifyType(leftProxy, right, env.withoutUpcasting) match
                        case UnificationSuccess(env1, tp) =>
                            List(tp)
                        case UnificationFailure(_, _, _) =>
                            subtypeSeq(leftProxy.ref, right, env.withoutUpcasting)
            case (leftLambda: SIRType.TypeLambda, right: SIRType) =>
                // TODO: for now type parameters can be unresolved.
                val nFilledTypes = leftLambda.params.foldLeft(env.filledTypes) { case (acc, v) =>
                    acc.get(v) match {
                        case Some(tp) => acc
                        case None     => acc.updated(v, SIRType.FreeUnificator)
                    }
                }
                subtypeSeq(leftLambda.body, right, env.copy(filledTypes = nFilledTypes))
            case (left, rightLambda: SIRType.TypeLambda) =>
                val nFilledTypes = rightLambda.params.foldLeft(env.filledTypes) { case (acc, v) =>
                    acc.get(v) match {
                        case Some(tp) => acc
                        case None     => acc.updated(v, SIRType.FreeUnificator)
                    }
                }
                subtypeSeq(left, rightLambda.body, env.copy(filledTypes = nFilledTypes))
            case (left, rightProxy: SIRType.TypeProxy) =>
                if rightProxy.ref == null then
                    throw new RuntimeException("TypeProxy should be resolved before unification")
                else
                    unifyType(left, rightProxy, env.withoutUpcasting) match
                        case UnificationSuccess(env1, tp) =>
                            List(tp)
                        case UnificationFailure(_, _, _) =>
                            subtypeSeq(left, rightProxy.ref, env.withoutUpcasting)
            case (_, _) =>
                List.empty
    }

    def findChildConstrForSum(
        parentCandidateDecl: DataDecl,
        childConstrName: String
    ): List[DataDecl] = {

        def findRevIn(
            decl: DataDecl,
            track: java.util.IdentityHashMap[DataDecl, DataDecl]
        ): List[DataDecl] = {
            if track.containsKey(decl) then Nil
            else
                decl.constructors.find(_.name == childConstrName) match {
                    case None =>
                        val possibleSubtypes = decl.constructors.filter(c =>
                            SIRType.isSynteticNarrowConstrDeclName(c.name)
                        )
                        if possibleSubtypes.nonEmpty then track.put(decl, decl)
                        var s = possibleSubtypes
                        var r: List[DataDecl] = Nil
                        while r.nonEmpty && s.nonEmpty do {
                            SIRType.retrieveDataDecl(s.head.params.head.tp) match
                                case Left(msg) =>
                                case Right(cDataDecl) =>
                                    val c = findRevIn(cDataDecl, track)
                                    if c.nonEmpty then r = decl :: c
                                    else s = s.tail
                        }
                        r
                    case Some(_) => List(decl)
                }
        }

        findRevIn(parentCandidateDecl, new util.IdentityHashMap()).reverse

    }

    given Unify[Binding] with {
        def apply(left: Binding, right: Binding, env: Env): UnificationResult[Binding] = {
            unifyBinding(left, right, env)
        }
    }

    def unifyBinding(left: Binding, right: Binding, env: Env): UnificationResult[Binding] = {
        val (sameName, env1) = env.varNames.get(left.name) match {
            case Some(rightName) if rightName == right.name => (true, env)
            case Some(_)                                    => (false, env)
            case None => (true, env.copy(varNames = env.varNames.updated(left.name, right.name)))
        }
        if sameName then
            topLevelUnifyType(left.tp, right.tp, env1.copy(path = "tp" :: env1.path)) match
                case UnificationSuccess(env2, tp) =>
                    unifySIR(left.value, right.value, env2.copy(path = "value" :: env1.path)) match
                        case UnificationSuccess(env3, value) =>
                            UnificationSuccess(
                              env3.copy(path = env1.path),
                              Binding(left.name, tp, value)
                            )
                        case failure @ UnificationFailure(path, left, right) => failure
                case failure @ UnificationFailure(path, left, right) => failure
        else UnificationFailure(env.path, left, right)
    }

    given Unify[SIR.Decl] with {
        def apply(left: SIR.Decl, right: SIR.Decl, env: Env): UnificationResult[SIR.Decl] = {
            unifyDecl(left, right, env)
        }
    }

    def unifyDecl(left: SIR.Decl, right: SIR.Decl, env: Env): UnificationResult[SIR.Decl] = {
        unifyDataDecl(left.data, right.data, env.copy(path = "data" :: env.path)) match
            case UnificationSuccess(env1, data) =>
                unifySIR(left.term, right.term, env1.copy(path = "term" :: env.path)) match
                    case UnificationSuccess(env2, term) =>
                        UnificationSuccess(env2.copy(path = env.path), SIR.Decl(data, term))
                    case failure @ UnificationFailure(path, left, right) => failure
            case failure @ UnificationFailure(path, left, right) => failure
    }

    given Unify[TypeBinding] with {
        def apply(
            left: TypeBinding,
            right: TypeBinding,
            env: Env
        ): UnificationResult[TypeBinding] = {
            unifyTypeBinding(left, right, env)
        }
    }

    def unifyTypeBinding(
        left: TypeBinding,
        right: TypeBinding,
        env: Env
    ): UnificationResult[TypeBinding] = {
        if env.debug then
            println(
              s"unifyTypeBinding: left=${left.name}:${left.tp.show}, right=${right.name}:${right.tp.show}, env.upcasting=${env.upcasting}"
            )
        val retval =
            if left.name == right.name then
                topLevelUnifyType(left.tp, right.tp, env.copy(path = "tp" :: env.path)) match
                    case UnificationSuccess(env1, tp) =>
                        UnificationSuccess(env1.copy(path = env.path), TypeBinding(left.name, tp))
                    case failure @ UnificationFailure(path, left, right) => failure
            else UnificationFailure(env.path, left, right)
        if env.debug then println(s"unifyTypeBinding($left,$right) return ${retval}")
        retval
    }

    def unifyCases(
        left: List[SIR.Case],
        right: List[SIR.Case],
        env: Env
    ): UnificationResult[List[SIR.Case]] = {
        unifyList(left, right, env)
    }

    given Unify[SIR.Case] with {
        def apply(left: SIR.Case, right: SIR.Case, env: Env): UnificationResult[SIR.Case] = {
            unifyCase(left, right, env)
        }
    }

    def unifyCase(left: SIR.Case, right: SIR.Case, env: Env): UnificationResult[SIR.Case] = {
        (left.pattern, right.pattern) match
            case (
                  Pattern.Constr(leftConstr, lbindings, ltbds),
                  Pattern.Constr(rightConstr, rbindings, rtbds)
                ) =>
                unifyConstrDecl(
                  leftConstr,
                  rightConstr,
                  env.copy(path = "constrDecl" :: env.path)
                ) match
                    case UnificationSuccess(env1, constrDecl) =>
                        unifyList(
                          lbindings,
                          rbindings,
                          env1.copy(path = "bindings" :: env.path)
                        ) match
                            case UnificationSuccess(env2, bindings) =>
                                unifyList(
                                  ltbds,
                                  rtbds,
                                  env2.copy(path = "typeBindings" :: env.path)
                                ) match
                                    case UnificationSuccess(env3, typeBindings) =>
                                        unifySIR(
                                          left.body,
                                          right.body,
                                          env3.copy(path = "body" :: env.path)
                                        ) match
                                            case UnificationSuccess(env4, body) =>
                                                UnificationSuccess(
                                                  env4.copy(path = env.path),
                                                  SIR.Case(
                                                    Pattern
                                                        .Constr(constrDecl, bindings, typeBindings),
                                                    body,
                                                    left.anns
                                                  )
                                                )
                                            case failure @ UnificationFailure(path, left, right) =>
                                                failure
                                    case failure @ UnificationFailure(path, left, right) => failure
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            case (Pattern.Wildcard, Pattern.Wildcard) =>
                unifySIR(left.body, right.body, env.copy(path = "body" :: env.path)) match
                    case UnificationSuccess(env1, body) =>
                        UnificationSuccess(env, SIR.Case(Pattern.Wildcard, body, left.anns))
                    case failure @ UnificationFailure(path, left, right) => failure
            case (_, _) =>
                UnificationFailure(env.path, left, right)
    }

    given Unify[ConstrDecl] with {
        def apply(left: ConstrDecl, right: ConstrDecl, env: Env): UnificationResult[ConstrDecl] = {
            unifyConstrDecl(left, right, env)
        }
    }

    def unifyConstrDecl(
        left: ConstrDecl,
        right: ConstrDecl,
        env: Env
    ): UnificationResult[ConstrDecl] = {
        if env.debug then println(s"unifyConstrDecl: left=${left}, right=${right}")
        if left eq right then UnificationSuccess(env, left)
        else if left.name == right.name then {
            if !env.deepDeclCheck then UnificationSuccess(env, left)
            else {
                // TODO: in new env,
                unifyList(
                  left.typeParams,
                  right.typeParams,
                  env.copy(path = "typeParams" :: env.path)
                )(using
                  TypeVarRelaxingUnify
                ) match
                    case UnificationSuccess(env1, typeParams) =>
                        unifyList(
                          left.params,
                          right.params,
                          env1.copy(path = "args" :: env.path)
                        ) match
                            case UnificationSuccess(env2, params) =>
                                unifyList(
                                  left.parentTypeArgs,
                                  right.parentTypeArgs,
                                  env2.copy(path = "parentTypeArgs" :: env.path)
                                ) match
                                    case UnificationSuccess(ebv3, parentTypeArgs) =>
                                        UnificationSuccess(
                                          env2.copy(path = env.path),
                                          ConstrDecl(
                                            left.name,
                                            params,
                                            typeParams,
                                            parentTypeArgs,
                                            left.annotations
                                          )
                                        )
                                    case failure @ UnificationFailure(path, left, right) =>
                                        failure
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            }

        } else UnificationFailure(env.path, left, right)
    }

    given Unify[DataDecl] with {
        def apply(left: DataDecl, right: DataDecl, env: Env): UnificationResult[DataDecl] = {
            unifyDataDecl(left, right, env)
        }
    }

    // for now we accept to different data-decls as unrealred.
    //  things can changed in future,  when DataDecl will be able contains not only constructors
    def unifyDataDecl(left: DataDecl, right: DataDecl, env: Env): UnificationResult[DataDecl] = {
        if left eq right then UnificationSuccess(env, left)
        else if left.name == right.name then {
            if !env.deepDeclCheck then UnificationSuccess(env, left)
            else {
                // order of conctructoes is determenistic (the same as in program source)
                // if (left.typeParams.isEmpty && right.typeParams.isEmpty) then

                unifyList(
                  left.constructors,
                  right.constructors,
                  env.copy(path = "constructors" :: env.path)
                ) match
                    case UnificationSuccess(env1, constructors) =>
                        unifyList(
                          left.typeParams,
                          right.typeParams,
                          env1.copy(path = "typeParams" :: env.path)
                        )(using TypeVarRelaxingUnify) match
                            case UnificationSuccess(env2, typeParams) =>
                                UnificationSuccess(
                                  env2.copy(path = env.path),
                                  DataDecl(left.name, constructors, typeParams, left.annotations)
                                )
                            case failure @ UnificationFailure(path, left, right) => failure
                    case failure @ UnificationFailure(path, left, right) => failure
            }
        } else UnificationFailure(env.path, left, right)
    }

    def unifyList[T](left: List[T], right: List[T], env: Env)(using
        Unify[T]
    ): UnificationResult[List[T]] = {
        if left.length != right.length then UnificationFailure(env.path, left, right)
        else
            val res = left
                .zip(right)
                .foldLeft[UnificationResult[List[T]]](UnificationSuccess(env, List.empty)) {
                    case (UnificationSuccess(env, acc), (l, r)) =>
                        summon[Unify[T]].apply(l, r, env) match
                            case UnificationSuccess(env1, v) =>
                                UnificationSuccess(env1, v :: acc)
                            case failure @ UnificationFailure(path, left, right) =>
                                failure
                    case (failure @ UnificationFailure(path, left, right), _) =>
                        failure
                }
            res match
                case UnificationSuccess(env, res) =>
                    UnificationSuccess(env, res.reverse)
                case failure @ UnificationFailure(path, left, right) =>
                    failure

    }

    // should not be given,  because this contradict with using type-vars in type unification.
    //  This unification is ised when we check - are two ContrDecl or DataDecl are the same
    object TypeVarSyntaxUnify extends Unify[SIRType.TypeVar] {
        def apply(
            left: SIRType.TypeVar,
            right: SIRType.TypeVar,
            env: Env
        ): UnificationResult[SIRType.TypeVar] = {
            unifyTypeVarSyntaxically(left, right, env)
        }
    }

    def unifyTypeVarSyntaxically(
        v1: SIRType.TypeVar,
        v2: SIRType.TypeVar,
        env: Env
    ): UnificationResult[SIRType.TypeVar] = {
        if v1 == v2 then UnificationSuccess(env, v1)
        else UnificationFailure(env.path, v1, v2)
    }

    object TypeVarRelaxingUnify extends Unify[SIRType.TypeVar] {
        def apply(
            left: SIRType.TypeVar,
            right: SIRType.TypeVar,
            env: Env
        ): UnificationResult[SIRType.TypeVar] = {
            unifyTypeVarRelaxing(left, right, env)
        }
    }

    def unifyTypeVarRelaxing(
        v1: SIRType.TypeVar,
        v2: SIRType.TypeVar,
        env: Env
    ): UnificationResult[SIRType.TypeVar] = {
        val retval =
            if v1 == v2 then UnificationSuccess(env, v1)
            else
                env.filledTypes.get(v1) match {
                    case Some(tp1) =>
                        env.filledTypes.get(v2) match {
                            case Some(tp2) =>
                                unifyType(tp1, tp2, env) match {
                                    case UnificationSuccess(env1, _) =>
                                        UnificationSuccess(env1, v1)
                                    case failure @ UnificationFailure(path, left, right) =>
                                        failure
                                }
                            case None =>
                                val nFilledTypes = env.filledTypes.updated(v2, tp1)
                                checkEqType(env.copy(filledTypes = nFilledTypes), v2, tp1).map(_ =>
                                    v1
                                )
                        }
                    case None =>
                        env.filledTypes.get(v2) match {
                            case Some(tp2) =>
                                val nFilledTypes = env.filledTypes.updated(v1, tp2)
                                checkEqType(env.copy(filledTypes = nFilledTypes), v1, tp2).map(_ =>
                                    v1
                                )
                            case None =>
                                val v1EqSet = env.eqTypes.getOrElse(v1, Set.empty)
                                val v2EqSet = env.eqTypes.getOrElse(v2, Set.empty)
                                val nEqTypes =
                                    env.eqTypes.updated(v1, v1EqSet + v2).updated(v2, v2EqSet + v1)
                                UnificationSuccess(env.copy(eqTypes = nEqTypes), v1)
                        }
                }
        if env.debug then
            println(s"unityTypeVarRelaxing, left=${v1}, right=${v2}, retval=${retval}")
        retval
    }

    def typeVarsInEnv(vars: List[SIRType.TypeVar], env: Env): Boolean = {
        if vars.exists(v => env.filledTypes.contains(v) || env.eqTypes.contains(v)) then true
        else env.topLevelTypes.exists(tp => SIRType.isTypeVarsUsedIn(vars, tp))
    }

    def retrieveTypeVarGenerationContext(env: Env): SIRType.SetBasedTypeVarGenerationContext = {
        env.optTypeVarGenerationContext match {
            case Some(ctx) => ctx
            case None      =>

        }
        val existing = env.filledTypes.keys.toSet ++ env.eqTypes.keys.toSet
        if env.topLevelTypes.isEmpty then
            throw IllegalStateException(
              "top-level types should not be empty in unification context"
            )
        val maxExisting =
            if existing.isEmpty then 0L
            else existing.maxBy(_.optId.getOrElse(0L)).optId.getOrElse(0L)
        val ctx =
            SIRType.createMinimalTypeVarGenerationContext(maxExisting, env.topLevelTypes.toList)
        ctx
    }

}
