package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.compiler.sir.lowering.Lowering.tpf
import scalus.compiler.sir.*
import scalus.uplc.*
import scalus.compiler.sir.lowering.SumCaseClassRepresentation.SumDataList

import scala.collection.mutable.{Map as MutableMap, Set as MutableSet}
//import scala.util.control.NonFatal

/** SEA of nodes - like representation. E.e. each value is node, which manage dependencies.
  * LoweredValue:
  *   - represents a value in lowered SIR, which is
  *   - a generator of a block of code with generate specific SIRType in specific representation. --
  *     (representation is LoweredValueRepresentation, which can be term or data in specific
  *     encoding)
  *   - some lowered values can be named (see IdentifiableLoweredValue and maintain a set of
  *     dependencies) The idea of SEA of nodes -- than we can generate code 'by need' based on
  *     dependencies. I.e. if value not used at all - code will not be generated, if value used once
  *     -- no intermediate construction will be created and code will be generated directly in place
  *     of usage. If value used multiple times -- it will be generated near the usage (i.e. in the
  *     nearest block, which contains all depended).
  *
  * The idea of SEA of nodes described in the paper: <code> Cliff Click. 1995. Global code
  * motion/global value numbering. SIGPLAN Not. 30, 6 (June 1995), 246–257.
  * "https://doi.org/10.1145/223428.207154" </code>
  *
  * We have a quite different code (non-SSA form, only dominators), but the main idea of relaxing
  * order is the same,
  */
trait LoweredValue {

    val createdEx = new RuntimeException("Lovered value created here")
    var debugMark = "i"

    def sirType: SIRType

    def pos: SIRPosition

    /** The UPLC term that represents this value, wrapped in vars if needed.
      * @param gctx - context for term generation
      *  @return generated term wrapped in lambdas with definition of needed uplevel variables
      */
    def termWithNeededVars(gctx: TermGenerationContext): Term =
        Lowering.generateDominatedUplevelVarsAccess(this)(using gctx)

    /** Generates the term for this value.
      * @param gctx
      * @return
      */
    def termInternal(gctx: TermGenerationContext): Term

    /** The type of representation of this value.
      */
    def representation: LoweredValueRepresentation

    /** Convert this value to the giveb representation,
      */
    def toRepresentation(representation: LoweredValueRepresentation, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        summon[LoweringContext].typeGenerator(sirType).toRepresentation(this, representation, pos)
    }

    def upcastOne(targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue =
        summon[LoweringContext].typeGenerator(sirType).upcastOne(this, targetType, pos)

    /** Upcast the value to the target type if needed.
      */
    def maybeUpcast(targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        SIRUnify.topLevelUnifyType(sirType, targetType, SIRUnify.Env.empty.withoutUpcasting) match
            case SIRUnify.UnificationSuccess(env, tp) =>
                this
            case SIRUnify.UnificationFailure(path, l, r) =>
                // if we cannot unify types, we need to upcast
                //  to the target type.
                val parentsSeq = SIRUnify.subtypeSeq(sirType, targetType, SIRUnify.Env.empty)
                if lctx.debug then
                    println(s"[maybeUpcast] from ${sirType.show} to ${targetType.show}")
                    println(s"[maybeUpcast] parentsSeq = ${parentsSeq.map(_.show)}")
                    println(
                      s"[maybeUpcast] will upcast to parentsSeq.tail = ${parentsSeq.tail.map(_.show)}"
                    )
                if parentsSeq.isEmpty then {
                    if lctx.debug then
                        lctx.log(
                          s"LoweredValue.maybeUpcast: first unify failure: path = ${path}, left = ${l}, right = ${r}"
                        )
                        val debugUnification = SIRUnify.topLevelUnifyType(
                          sirType,
                          targetType,
                          SIRUnify.Env.empty.withoutUpcasting.withDebug
                        )
                    throw LoweringException(
                      s"Cannot upcast ${this.sirType.show} to ${targetType.show}",
                      pos
                    )
                } else
                    parentsSeq.tail.foldLeft(this) { (s, e) =>
                        s.upcastOne(e, pos)
                    }
    }

    def findSelfOrSubtems(p: LoweredValue => Boolean): Option[LoweredValue]

    /** Uplevel variables, that should be generated before generation of term
      */
    def dominatingUplevelVars: Set[IdentifiableLoweredValue]

    /** Uplevel variables, that are used in this value.
      */
    def usedUplevelVars: Set[IdentifiableLoweredValue]

    /** Variables, that are directly defined in this value.
      */
    def ownVars: Set[IdentifiableLoweredValue]

    /** Internal variables, that are used inside this value.
      */
    def internalVars: Set[IdentifiableLoweredValue]

    /** Is this value effort-less to compute (i.e. constant or variable or lambda)
      */
    def isEffortLess: Boolean

    /** add identifiable variable to be updated from this variable */
    def addDependent(
        value: IdentifiableLoweredValue
    ): Unit

    def show: String = pretty.render(100)

    def shortShow: String = {
        val s = show
        val newlinePos = s.indexOf('\n')
        if newlinePos >= 0 && newlinePos < 60 then s.take(newlinePos) + "..."
        else if s.length > 60 then s.take(60) + "..."
        else s
    }

    /** Pretty print this value with all definitona
      * @param style - style of printing
      * @return
      */
    def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc

    /** Pretty print reference to value
      * @param style - style of printing
      * @return
      */
    def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc

    def pretty: Doc = {
        val ctx = new LoweredValue.PrettyPrintingContext()
        docDef(ctx)
    }

}

case class ConstantLoweredValue(
    sir: SIR.Const,
    representation: LoweredValueRepresentation
) extends LoweredValue {
    override def sirType: SIRType = sir.tp
    override def pos: SIRPosition = sir.anns.pos
    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    override def usedUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    override def ownVars: Set[IdentifiableLoweredValue] = Set.empty
    override def internalVars: Set[IdentifiableLoweredValue] = Set.empty
    override def isEffortLess: Boolean = true
    override def termInternal(gctx: TermGenerationContext): Term =
        Term.Const(sir.uplcConst)
    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        // constants are not depended on any variables
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        (PrettyPrinter.pretty(Term.Const(sir.uplcConst), ctx.style) + Doc.text(":") + Doc.text(
          sirType.show
        ) + PrettyPrinter
            .inBrackets(representation.doc)).grouped
    }

    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc =
        PrettyPrinter.pretty(Term.Const(sir.uplcConst), ctx.style)

    override def findSelfOrSubtems(p: LoweredValue => Boolean): Option[LoweredValue] = {
        Some(this).filter(p)
    }

}

case class StaticLoweredValue(
    sir: AnnotatedSIR,
    term: Term,
    representation: LoweredValueRepresentation,
    override val isEffortLess: Boolean
) extends LoweredValue {

    def sirType: SIRType = sir.tp
    def pos: SIRPosition = sir.anns.pos
    def dominatingUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    def usedUplevelVars: Set[IdentifiableLoweredValue] = Set.empty
    def ownVars: Set[IdentifiableLoweredValue] = Set.empty
    def internalVars: Set[IdentifiableLoweredValue] = Set.empty
    def termInternal(gctx: TermGenerationContext): Term = term
    def addDependent(value: IdentifiableLoweredValue): Unit = {}

    def findSelfOrSubtems(p: LoweredValue => Boolean): Option[LoweredValue] = {
        Some(this).filter(p)
    }

    def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import PrettyPrinter.*
        Doc.text("static") + inParens(
          PrettyPrinter.pretty(term, ctx.style) + Doc.text(":") + Doc.text(
            sir.tp.show
          ) + inBrackets(representation.doc)
        )
    }

    def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        PrettyPrinter.pretty(term, ctx.style)
    }

}

/** Values with id which can be represented by variables (if needed).
  *
  * Identificatin is id.
  */
sealed trait IdentifiableLoweredValue extends LoweredValue {

    def id: String
    def name: String

    def optRhs: Option[LoweredValue]

    override def hashCode(): Int = id.hashCode()

    override def equals(obj: Any): Boolean = obj match {
        case that: IdentifiableLoweredValue => this.id == that.id
        case _                              => false
    }

    /** Set of variables, which this value are directly used in. (i.e. the have this value in rhs).
      * non-transitive (i.e. if your want to find all depended set you should run transitive closure
      * on this)
      */
    def directDepended: MutableSet[IdentifiableLoweredValue]

    /** Set of variables, which this value are directly depended on.
      */
    def directDependFrom: MutableSet[IdentifiableLoweredValue]

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        directDepended.add(value)
        value.directDependFrom.add(this)
    }

    def isDependFrom(value: IdentifiableLoweredValue): Boolean = {
        directDependFrom.exists { c =>
            c.id == value.id || c.isDependFrom(value)
        }
    }

    def isDependFromOneOf(values: Set[IdentifiableLoweredValue]): Boolean = {
        directDependFrom.exists { c =>
            values.exists(v => c.id == v.id) || c.isDependFromOneOf(values)
        }
    }

    override def findSelfOrSubtems(p: LoweredValue => Boolean): Option[LoweredValue] = {
        Some(this).filter(p).orElse(optRhs.flatMap(_.findSelfOrSubtems(p)))
    }

    override def ownVars: Set[IdentifiableLoweredValue] = Set.empty

    override def internalVars: Set[IdentifiableLoweredValue] = Set.empty

    override def isEffortLess: Boolean = true

}

class VariableLoweredValue(
    val id: String, // unique id for variable, to avoid clashes with shadowed names
    val name: String,
    val sir: SIR.Var,
    val representation: LoweredValueRepresentation,
    val otherRepresentations: MutableMap[
      LoweredValueRepresentation,
      DependendVariableLoweredValue
    ] = MutableMap.empty,
    val optRhs: Option[LoweredValue] = None,
    val directDepended: MutableSet[IdentifiableLoweredValue] = MutableSet.empty,
    val directDependFrom: MutableSet[IdentifiableLoweredValue] = MutableSet.empty
) extends IdentifiableLoweredValue {

    optRhs.foreach { rhs =>
        rhs.addDependent(this)
    }

    // if id == "scalus.ledger.api.v1.Credential$.given_Eq_Credential252" then {
    //    println(
    //      s"VariableLoweredValue created with id = $id,  representation = $representation, sirType=${sir.tp.show}"
    //    )
    //    println(
    //      s"SIR=${sir} at ${sir.anns.pos.file}:${sir.anns.pos.startLine + 1}"
    //    )
    //    throw new RuntimeException(
    //      s"VariableLoweredValue created with id = $id,  representation = $representation, sirType=${sir.tp}"
    //    )
    // }

    override def sirType: SIRType = sir.tp
    override def pos: SIRPosition = sir.anns.pos

    override def toRepresentation(representation: LoweredValueRepresentation, pos: SIRPosition)(
        using lctx: LoweringContext
    ): LoweredValue = {
        if representation == this.representation then this
        else
            otherRepresentations.get(representation) match {
                case Some(depVar) =>
                    // if we have already created dependent variable for this representation, return it
                    depVar
                case None =>
                    val retval = summon[LoweringContext]
                        .typeGenerator(sirType)
                        .toRepresentation(this, representation, pos)
                    val depId = lctx.uniqueVarName(name + "r")
                    val depVar = DependendVariableLoweredValue(
                      depId,
                      depId,
                      this,
                      representation,
                      retval,
                      pos,
                    )
                    otherRepresentations.put(representation, depVar)
                    depVar
            }

    }

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        optRhs.map(rhs => rhs.dominatingUplevelVars).getOrElse(Set.empty)

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        Set(this) ++ optRhs.map(rhs => rhs.usedUplevelVars).getOrElse(Set.empty)

    override def termInternal(gctx: TermGenerationContext): Term = {
        if gctx.generatedVars.contains(id) then Term.Var(NamedDeBruijn(id))
        else
            optRhs match {
                case Some(rhs) =>
                    // if we here, it means that we was not included in any domination set,
                    //  so variable used once and we can generate rhs term instead of name.
                    rhs.termWithNeededVars(gctx)
                case None =>
                    if gctx.processUndefinedValues then {
                        if gctx.debug then {
                            println(
                              s"VariableLoweredValue: generating term for undefined variable $name with id $id"
                            )
                        }
                        Term.Var(NamedDeBruijn(id))
                    } else
                        throw new IllegalStateException(
                          s"Variable $name with id $id is not defined and has no rhs to generate term."
                        )

            }
    }

    def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import PrettyPrinter.*
        val head = (Doc.text("var") + inParens(
          Doc.text(id) + Doc.text(":") + Doc.text(sirType.show) + inBrackets(representation.doc)
        )).grouped
        ctx.printedIdentifiers += id
        val retval = optRhs match {
            case Some(rhs) =>
                val left = head + Doc.text("(=")
                val right = Doc.text(")")
                rhs.docDef(ctx).bracketBy(left, right)
            case None =>
                head
        }
        retval
    }

    def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        if ctx.printedIdentifiers.contains(id) then Doc.text(id)
        else
            optRhs match {
                case None => Doc.text(id)
                case Some(rhs) =>
                    ctx.printedIdentifiers += id
                    val left = Doc.text(id) + Doc.text("(=")
                    val right = Doc.text(")")
                    rhs.docDef(ctx).bracketBy(left, right)
            }
    }

}

/** Represent a variable which is dependent on another variable. i.e. var x1 =
  * (toOtherReperesentation(y))
  */
case class DependendVariableLoweredValue(
    id: String,
    name: String,
    parent: VariableLoweredValue,
    representation: LoweredValueRepresentation,
    rhs: LoweredValue,
    inPos: SIRPosition,
    directDepended: MutableSet[IdentifiableLoweredValue] = MutableSet.empty,
    directDependFrom: MutableSet[IdentifiableLoweredValue] = MutableSet.empty
) extends IdentifiableLoweredValue {

    rhs.addDependent(this)

    override def sirType: SIRType = parent.sirType
    override def pos: SIRPosition = inPos

    override def toRepresentation(representation: LoweredValueRepresentation, pos: SIRPosition)(
        using LoweringContext
    ): LoweredValue = {
        if representation == this.representation then this
        else if representation == parent.representation then parent
        else {
            parent.otherRepresentations.get(representation) match
                case Some(depVar) =>
                    depVar
                case None =>
                    val newRepr = summon[LoweringContext]
                        .typeGenerator(sirType)
                        .toRepresentation(this, representation, pos)
                    val newId = summon[LoweringContext].uniqueVarName(name + "r")
                    val newDepVar = DependendVariableLoweredValue(
                      newId,
                      newId,
                      parent,
                      representation,
                      newRepr,
                      pos
                    )
                    parent.otherRepresentations.put(representation, newDepVar)
                    newDepVar
        }
    }

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        rhs.dominatingUplevelVars

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        Set(this) ++ rhs.usedUplevelVars

    override def optRhs: Option[LoweredValue] = Some(rhs)

    override def termInternal(gctx: TermGenerationContext): Term = {
        if gctx.generatedVars.contains(id) then Term.Var(NamedDeBruijn(id))
        else rhs.termWithNeededVars(gctx)

    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import PrettyPrinter.*
        ctx.printedIdentifiers += id
        (Doc.text("depvar") + inParens(
          Doc.text(id) + Doc.text(":") + Doc.text(sirType.show) + inBrackets(representation.doc)
        )).grouped + Doc.text("=")
            + rhs.docDef(ctx)
    }

    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        if ctx.printedIdentifiers.contains(id) then Doc.text(id)
        else
            val left = Doc.text(id) + Doc.text("(=")
            val right = Doc.text(")")
            ctx.printedIdentifiers += id
            rhs.docDef(ctx).bracketBy(left, right)
    }

}

trait ProxyLoweredValue(val origin: LoweredValue) extends LoweredValue {

    override def sirType: SIRType = origin.sirType

    override def pos: SIRPosition = origin.pos

    override def representation: LoweredValueRepresentation = origin.representation

    override def dominatingUplevelVars: Set[IdentifiableLoweredValue] =
        origin.dominatingUplevelVars

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        origin.usedUplevelVars

    override def ownVars: Set[IdentifiableLoweredValue] =
        origin.ownVars

    override def internalVars: Set[IdentifiableLoweredValue] =
        origin.internalVars

    override def addDependent(value: IdentifiableLoweredValue): Unit =
        origin.addDependent(value)

    override def isEffortLess: Boolean = origin.isEffortLess

    /*
    override def docDef(style: PrettyPrinter.Style = PrettyPrinter.Style.Normal): Doc = {
        (Doc.text("proxy") + PrettyPrinter.inParens(
          origin.docDef(style) + Doc.text(":") + Doc.text(sirType.show) + PrettyPrinter.inBrackets(
            representation.doc
          )
        )).grouped
    }

     */

    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc =
        this.docDef(ctx)

    override def findSelfOrSubtems(p: LoweredValue => Boolean): Option[LoweredValue] =
        origin.findSelfOrSubtems(p)

}

case class DelayLoweredValue(input: LoweredValue, override val pos: SIRPosition)
    extends ProxyLoweredValue(input) {

    override def termInternal(gctx: TermGenerationContext): Term = {
        Term.Delay(input.termWithNeededVars(gctx))
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("delay") + Doc.text("(")
        val right = Doc.text(")")
        input.docRef(ctx).bracketBy(left, right).grouped
    }

}

case class ForceLoweredValue(input: LoweredValue, override val pos: SIRPosition)
    extends ProxyLoweredValue(input) {

    override def termInternal(gctx: TermGenerationContext): Term = {
        Term.Force(input.termWithNeededVars(gctx))
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("force") + Doc.text("(")
        val right = Doc.text(")")
        input.docRef(ctx).bracketBy(left, right).grouped
    }

}

/** ComplexLoweredValue is a base trait for lowered values that consist of multiple subvalues.
  * ownVars - var that used in subvalues but defined in this value. (i.e. not propagated up in
  * dominatedUplevelVars)
  */
trait ComplexLoweredValue(
    override val ownVars: Set[IdentifiableLoweredValue],
    subvalues: LoweredValue*
) extends LoweredValue {

    // TODO: calculate dependency set in one pass
    val usedVarsCount = Lowering.filterAndCountVars(
      v => !ownVars.contains(v) && !ownVars.exists(o => v.isDependFrom(o)),
      subvalues*
    )

    lazy val cUsedVars = usedVarsCount.keySet

    val dominatingUplevelVars: Set[IdentifiableLoweredValue] = {
        usedVarsCount.filter { case (v, c) =>
            c > 1 && v.directDepended.size > 1
        }.keySet
    }

    override def usedUplevelVars: Set[IdentifiableLoweredValue] =
        cUsedVars

    lazy val cInternalVars = ownVars ++ subvalues.flatMap(_.internalVars).toSet

    override def internalVars: Set[IdentifiableLoweredValue] = cInternalVars

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        subvalues.foreach(_.addDependent(value))
    }

    // Complex values require evaluation, so they are not effortless by default
    // Specific subclasses (like LambdaLoweredValue) can override this if needed
    override def isEffortLess: Boolean = false

    def retrieveSubvalues: Seq[LoweredValue] = subvalues

    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        this.docDef(ctx)
    }

    override def findSelfOrSubtems(p: LoweredValue => Boolean): Option[LoweredValue] = {
        Some(this).filter(p).orElse {
            var found: Option[LoweredValue] = None
            subvalues.find { sub =>
                found = sub.findSelfOrSubtems(p)
                found.isDefined
            }
            found
        }
    }

}

case class LambdaLoweredValue(newVar: VariableLoweredValue, body: LoweredValue, inPos: SIRPosition)
    extends ComplexLoweredValue(Set(newVar), body) {

    override def sirType: SIRType = SIRType.Fun(newVar.sirType, body.sirType)

    override def representation: LoweredValueRepresentation = {
        LambdaRepresentation(
          sirType,
          InOutRepresentationPair(newVar.representation, body.representation)
        )
    }

    override def pos: SIRPosition = inPos

    override def isEffortLess: Boolean = {
        // we need to check depedencies, because body can depend on other variables
        // and we don;t want to expand lambda in place if body is not effort-less.
        //  So, we should run effort-elss on dependencies of body.
        body.usedUplevelVars.forall(_.isEffortLess)
    }

    // Lambda barrier: mark all non-effortless used vars as dominating
    // This ensures they are wrapped OUTSIDE the lambda by the parent's generateDominatedUplevelVarsAccess
    override val dominatingUplevelVars: Set[IdentifiableLoweredValue] = {
        // Helper to check if a variable has a non-effortless RHS (direct check only)
        // Effortless variables that reference other variables will be inlined,
        // so we only need to prevent the actual computation from entering the lambda
        def hasNonEffortlessRhs(v: IdentifiableLoweredValue): Boolean = {
            v match
                case vv: VariableLoweredValue =>
                    vv.optRhs match
                        case Some(rhs) => !rhs.isEffortLess
                        case None      => false // No RHS means it's just a parameter/reference
                case dv: DependendVariableLoweredValue =>
                    !dv.rhs.isEffortLess
        }

        // Get the normally dominating vars (multi-use, multi-dependent)
        val normallyDominating = usedVarsCount.filter { case (v, c) =>
            c > 1 && v.directDepended.size > 1
        }.keySet

        // Lambda barrier: prevent non-effortless computations from crossing lambda boundary
        // A variable should stay inside if it depends on ANY lambda parameter:
        // - The current lambda parameter (newVar), OR
        // - Any enclosing lambda parameter from outer lambdas
        val nonEffortlessExternalVars = cUsedVars.filter { v =>
            hasNonEffortlessRhs(v) && !v.isDependFrom(newVar) && !internalVars.contains(v)
        }

        val result = normallyDominating ++ nonEffortlessExternalVars

        result
    }

    override def termInternal(gctx: TermGenerationContext): Term = {
        // Lambda barrier: non-effortless vars are already in gctx.generatedVars (marked as dominating)
        // So body.termWithNeededVars will only wrap effortless vars inside the lambda
        // The parent will wrap non-effortless vars outside the lambda
        Term.LamAbs(newVar.id, body.termWithNeededVars(gctx.addGeneratedVar(newVar.id)))
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import PrettyPrinter.*
        val left = Doc.text("(") +
            Doc.text("lam") & Doc.text(newVar.id) + Doc.text(":") + Doc.text(
              sirType.show
            ) + inBrackets(
              representation.doc
            ) + Doc.text(".")
        val right = Doc.text(")")
        body.docRef(ctx).bracketBy(left, right)
    }

    override def findSelfOrSubtems(p: LoweredValue => Boolean): Option[LoweredValue] = {
        Some(this).filter(p).orElse {
            if p(newVar) then Some(newVar)
            else body.findSelfOrSubtems(p)
        }
    }

}

/** Lowered value which exists to create a scope for the set of variables. Need to prevent pulling
  * out variables outsiode scope from lambda-s, while we have not yet full tracking of control-flow
  * dependencies. (i.e. variables defined inside scope should not be pulled outside of it
  * automatically by lambda barrier logic)
  */
case class ScopeBracketsLoweredValue(
    vars: Set[IdentifiableLoweredValue],
    body: LoweredValue
) extends ComplexLoweredValue(vars, body) {

    override def sirType: SIRType = body.sirType

    override def representation: LoweredValueRepresentation = body.representation

    override def pos: SIRPosition = body.pos

    override def termInternal(gctx: TermGenerationContext): Term = {
        body.termWithNeededVars(gctx)
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("scope") + Doc.text("{")
        val right = Doc.text("}")
        body.docRef(ctx).bracketBy(left, right).grouped
    }

}

abstract class BuiltinApplyLoweredValue(
    val fun: SIR.Builtin,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition,
    val args: LoweredValue*
) extends ComplexLoweredValue(Set.empty, args*) {

    override def sirType: SIRType = tp

    override def representation: LoweredValueRepresentation = repr

    override def pos: SIRPosition = inPos

}

case class BuilinApply1LoweredVale(
    override val fun: SIR.Builtin,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition,
    arg: LoweredValue
) extends BuiltinApplyLoweredValue(fun, tp, repr, inPos, arg) {

    override def termInternal(gctx: TermGenerationContext): Term =
        Term.Apply(
          fun.bn.tpf,
          arg.termWithNeededVars(gctx)
        )

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text(fun.bn.name) + Doc.text("(")
        val right = Doc.text(")")
        arg.docRef(ctx).bracketBy(left, right).grouped
    }

}

case class BuilinApply2LoweredVale(
    override val fun: SIR.Builtin,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition,
    arg1: LoweredValue,
    arg2: LoweredValue
) extends BuiltinApplyLoweredValue(fun, tp, repr, inPos, arg1, arg2) {

    override def termInternal(gctx: TermGenerationContext): Term =
        Term.Apply(
          Term.Apply(
            Lowering.forcedBuiltin(fun.bn),
            arg1.termWithNeededVars(gctx)
          ),
          arg2.termWithNeededVars(gctx)
        )

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import Doc.*
        import PrettyPrinter.*
        text(fun.bn.name) + inParens(
          intercalate(lineOrSpace, List(arg1.docRef(ctx), arg2.docRef(ctx)))
        )
    }

}

case class ApplyLoweredValue(
    f: LoweredValue,
    arg: LoweredValue,
    sirType: SIRType,
    repr: LoweredValueRepresentation,
    pos: SIRPosition
) extends ComplexLoweredValue(Set.empty, f, arg) {

    override def representation: LoweredValueRepresentation = repr

    override def termInternal(gctx: TermGenerationContext): Term = {
        Term.Apply(
          f.termWithNeededVars(gctx),
          arg.termWithNeededVars(gctx)
        )
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import Doc.*
        import PrettyPrinter.*
        ((text("App") + inParens(
          f.docRef(ctx).nested(2) + lineOrSpace + arg.docRef(ctx)
        )) + lineOrSpace +
            text(":") + text(sirType.show) + inBrackets(
              representation.doc.nested(2)
            ) + lineOrSpace).grouped.aligned
    }

    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        (Doc.text("App") + PrettyPrinter.inParens(
          f.docRef(ctx).nested(2) + Doc.lineOrSpace + arg.docRef(ctx).nested(2)
        )).grouped
    }

}

case class LetNonRecLoweredValue(
    bindings: Seq[(IdentifiableLoweredValue, LoweredValue)],
    body: LoweredValue,
    inPos: SIRPosition
) extends ComplexLoweredValue(bindings.map(_._1).toSet, (bindings.map(_._2) ++ Seq(body))*) {

    override def sirType: SIRType = body.sirType

    override def representation: LoweredValueRepresentation =
        body.representation

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term = {
        val bodyGctx = gctx.copy(
          generatedVars = gctx.generatedVars ++ bindings.map(_._1.id)
        )
        val bodyTerm = body.termWithNeededVars(bodyGctx)
        bindings.foldRight(bodyTerm) { case ((varVal, rhs), term) =>
            Term.Apply(
              Term.LamAbs(varVal.id, term),
              rhs.termWithNeededVars(
                gctx.copy(
                  generatedVars = gctx.generatedVars + varVal.id
                )
              )
            )
        }
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val bindingsDoc = bindings.map { case (v, rhs) =>
            (v.docRef(ctx) + Doc.text("=") + rhs.docRef(ctx).nested(2)).grouped
        }
        val left = Doc.text("let") & Doc.intercalate(
          Doc.lineOrSpace,
          bindingsDoc
        ) + Doc.text("in") + Doc.lineOrSpace
        val right = Doc.empty
        body.docRef(ctx).bracketBy(left, right)
    }

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        super.addDependent(value)
        bindings.foreach { case (v, rhs) =>
            v.addDependent(value)
        }
    }

}

case class LetRecLoweredValue(
    newVar: IdentifiableLoweredValue,
    rhs: LoweredValue,
    body: LoweredValue,
    inPos: SIRPosition
) extends ComplexLoweredValue(Set(newVar), rhs, body) {

    override def sirType: SIRType = body.sirType

    override def pos: SIRPosition = inPos

    /*  let rec f  = x => f (x + 1)
        in f 0
        (\f -> f 0) (Z (\f. \x. f (x + 1)))
     */
    override def termInternal(gctx: TermGenerationContext): Term = {
        val nGctx = gctx.copy(
          generatedVars = gctx.generatedVars + newVar.id
        )
        val fixed =
            Term.Apply(
              Term.Var(NamedDeBruijn("__z_combinator__")),
              Term.LamAbs(
                newVar.id,
                rhs.termWithNeededVars(nGctx)
              )
            )
        Term.Apply(
          Term.LamAbs(newVar.id, body.termWithNeededVars(nGctx)),
          fixed
        )
    }

    override def representation: LoweredValueRepresentation =
        body.representation

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("(") + Doc.text("letrec") &
            (newVar.docRef(ctx) + Doc.text("=") + rhs.docRef(ctx).nested(2)).grouped & Doc.text(
              "in"
            )
        val right = Doc.text(")")
        body.docRef(ctx).bracketBy(left, right)
    }

    override def addDependent(value: IdentifiableLoweredValue): Unit = {
        super.addDependent(value)
        newVar.addDependent(value)
    }

}

case class IfThenElseLoweredValue(
    cond: LoweredValue,
    thenBranch: LoweredValue,
    elseBranch: LoweredValue,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition
) extends ComplexLoweredValue(Set.empty, cond, thenBranch, elseBranch) {

    override def sirType: SIRType = tp

    override def representation: LoweredValueRepresentation = repr

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term = {
        !(DefaultFun.IfThenElse.tpf $
            cond.termWithNeededVars(gctx) $
            ~thenBranch.termWithNeededVars(gctx) $
            ~elseBranch.termWithNeededVars(gctx))
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import Doc.*
        ((text("if") + (lineOrSpace + cond.docRef(ctx)).nested(2)).grouped
            + (line + text("then") + (Doc.lineOrSpace + thenBranch.docRef(ctx)).nested(2)).grouped
            + (line + text("else") + (Doc.lineOrSpace + elseBranch.docRef(ctx)).nested(
              2
            )).grouped).aligned
    }

}

/** LoweredValue for case on boolean builtins (PlutusV4 feature).
  *
  * In PlutusV4, `Case` can be used directly on boolean constants: Case(bool, [falseBranch,
  * trueBranch]) where False = index 0 and True = index 1.
  */
case class CaseBooleanLoweredValue(
    scrutinee: LoweredValue,
    falseBranch: LoweredValue,
    trueBranch: LoweredValue,
    tp: SIRType,
    repr: LoweredValueRepresentation,
    inPos: SIRPosition
) extends ComplexLoweredValue(Set.empty, scrutinee, falseBranch, trueBranch) {

    override def sirType: SIRType = tp

    override def representation: LoweredValueRepresentation = repr

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term = {
        // Case(scrutinee, [falseBranch, trueBranch])
        // False = 0, True = 1
        Term.Case(
          scrutinee.termWithNeededVars(gctx),
          List(
            falseBranch.termWithNeededVars(gctx),
            trueBranch.termWithNeededVars(gctx)
          )
        )
    }

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        import Doc.*
        ((text("case") + space + scrutinee.docRef(ctx) + space + text("of"))
            + (line + text("False ->") + (lineOrSpace + falseBranch.docRef(ctx)).nested(2)).grouped
            + (line + text("True ->") + (lineOrSpace + trueBranch.docRef(ctx)).nested(
              2
            )).grouped).aligned
    }

}

object LoweredValue {

    class PrettyPrintingContext(
        val style: PrettyPrinter.Style = PrettyPrinter.Style.Normal,
        var printedIdentifiers: Set[String] = Set.empty
    )

    /** Builder for LoweredValue, to avoid boilerplate code. Import this object to make available
      */
    object Builder {

        def lvIfThenElse(
            cond: LoweredValue,
            thenBranch: LoweredValue,
            elseBranch: LoweredValue,
            inPos: SIRPosition,
            optTargetType: Option[SIRType] = None
        )(using lctx: LoweringContext): LoweredValue = {

            // val resType = SIRType.leastUpperBound(thenBranch.sirType, elseBranch.sirType)

            val resType = optTargetType.getOrElse(
              SIRUnify.topLevelUnifyType(
                thenBranch.sirType,
                elseBranch.sirType,
                SIRUnify.Env.empty.withUpcasting
              ) match {
                  case SIRUnify.UnificationSuccess(_, tp) =>
                      if tp == SIRType.FreeUnificator then
                          throw LoweringException(
                            s"if-then-else branches return unrelated types: ${thenBranch.sirType.show} and ${elseBranch.sirType.show}",
                            inPos
                          )
                      tp
                  case failure @ SIRUnify.UnificationFailure(path, l, r) =>
                      // if we cannot unify types, we need to upcast
                      //  to the target type.
                      lctx.warn("Unification failure: " + failure, inPos)
                      SIRType.FreeUnificator
              }
            )

            if lctx.debug then {
                lctx.log(
                  s"lvIfThenElse: cond = ${cond.pretty.render(100)}, \n thenBranch = ${thenBranch.pretty.render(100)}, \n elseBranch = ${elseBranch.pretty.render(
                        100
                      )}, resType = ${resType.show}"
                )
                lctx.log(
                  s"lvIfThenElse: thenBranch.sirType = ${thenBranch.sirType.show}, thenBranch.representation = ${thenBranch.representation}, elseBranch.sirType = ${elseBranch.sirType.show}, elseBranch.representation = ${elseBranch.representation}"
                )
            }

            val thenBranchUpcasted = thenBranch.maybeUpcast(resType, inPos)
            val elseBranchUpcasted = elseBranch.maybeUpcast(resType, inPos)

            val targetRepresentation = chooseCommonRepresentation(
              Seq(thenBranchUpcasted, elseBranchUpcasted),
              resType,
              inPos
            )

            val thenBranchR = thenBranchUpcasted.toRepresentation(targetRepresentation, inPos)

            val elseBranchR = elseBranchUpcasted.toRepresentation(targetRepresentation, inPos)

            val condR = cond.toRepresentation(PrimitiveRepresentation.Constant, inPos)

            IfThenElseLoweredValue(
              condR,
              thenBranchR,
              elseBranchR,
              resType,
              targetRepresentation,
              inPos
            )

        }

        /** Generate a case expression on a boolean value (PlutusV4 feature).
          *
          * In PlutusV4, `Case` can be used directly on boolean constants: Case(bool, [falseBranch,
          * trueBranch]) where False = index 0 and True = index 1.
          */
        def lvCaseBoolean(
            scrutinee: LoweredValue,
            falseBranch: LoweredValue,
            trueBranch: LoweredValue,
            inPos: SIRPosition,
            optTargetType: Option[SIRType] = None
        )(using lctx: LoweringContext): LoweredValue = {

            val resType = optTargetType.getOrElse(
              SIRUnify.topLevelUnifyType(
                trueBranch.sirType,
                falseBranch.sirType,
                SIRUnify.Env.empty.withUpcasting
              ) match {
                  case SIRUnify.UnificationSuccess(_, tp) =>
                      if tp == SIRType.FreeUnificator then
                          throw LoweringException(
                            s"case branches return unrelated types: ${trueBranch.sirType.show} and ${falseBranch.sirType.show}",
                            inPos
                          )
                      tp
                  case failure @ SIRUnify.UnificationFailure(path, l, r) =>
                      lctx.warn("Unification failure: " + failure, inPos)
                      SIRType.FreeUnificator
              }
            )

            val trueBranchUpcasted = trueBranch.maybeUpcast(resType, inPos)
            val falseBranchUpcasted = falseBranch.maybeUpcast(resType, inPos)

            val targetRepresentation = chooseCommonRepresentation(
              Seq(trueBranchUpcasted, falseBranchUpcasted),
              resType,
              inPos
            )

            val trueBranchR = trueBranchUpcasted.toRepresentation(targetRepresentation, inPos)
            val falseBranchR = falseBranchUpcasted.toRepresentation(targetRepresentation, inPos)
            val scrutineeR = scrutinee.toRepresentation(PrimitiveRepresentation.Constant, inPos)

            CaseBooleanLoweredValue(
              scrutineeR,
              falseBranchR,
              trueBranchR,
              resType,
              targetRepresentation,
              inPos
            )
        }

        def lvApply(
            f: LoweredValue,
            arg: LoweredValue,
            inPos: SIRPosition,
            resTp: Option[SIRType] = None,
            resRepr: Option[LoweredValueRepresentation] = None
        )(using
            lctx: LoweringContext
        ): LoweredValue = {
            // if lctx.debug then {
            //    Thread.dumpStack()
            //    throw new RuntimeException(
            //      s"lvApply called with f = ${f.pretty.render(100)}, arg = ${arg.pretty.render(100)}"
            //    )
            // }

            val prevDebug = lctx.debug

            def argType(tp: SIRType): SIRType = tp match {
                case SIRType.Fun(argTp, _) => argTp
                case SIRType.TypeLambda(params, body) =>
                    SIRType.TypeLambda(params, argType(body))
                case SIRType.TypeProxy(ref) =>
                    if ref != null then argType(ref.asInstanceOf[SIRType])
                    else
                        throw LoweringException(
                          s"Empty type proxy in function application",
                          inPos
                        )
                case SIRType.FreeUnificator =>
                    SIRType.FreeUnificator
                case _ =>
                    throw LoweringException(
                      s"Exprected function type, but have: ${tp.show}",
                      inPos
                    )
            }

            val targetArgType = argType(f.sirType) match {
                case tv @ SIRType.TypeVar(name, optId, isBuiltin) =>
                    val resolvedFArgType = lctx.resolveTypeVarIfNeeded(tv)
                    resolvedFArgType
                case SIRType.FreeUnificator =>
                    arg.sirType
                case fArgTp => fArgTp
            }

            if lctx.debug then {
                lctx.log(
                  s"lvApply: argType(f.sirType) = ${argType(f.sirType).show}, f.sirType = ${f.sirType.show}, arg.sirType = ${arg.sirType.show}"
                )
                lctx.log(
                  s"lvApply: f = ${f.pretty.render(100)}"
                )
                lctx.log(
                  s"lvApply: f.representation = ${f.representation.doc.render(100)}, arg.representation = ${arg.representation.doc.render(100)}"
                )
                lctx.log(
                  s"lvApply: arg.sirType = ${arg.sirType.show}, arg.representation = ${arg.representation.doc.render(100)}"
                )
                lctx.log(
                  s"lvApply: arg = ${arg.pretty.render(100)}"
                )
                lctx.log(
                  s"lvApply: resTp=${resTp.map(_.show)} resRepresentation = ${resRepr.getOrElse("None")}"
                )
            }

            val (argTypevarResolved, typeAligned) = arg.sirType match {
                case tv @ SIRType.TypeVar(name, optId, isBuiltin) =>
                    val resolvedArgType = lctx.resolveTypeVarIfNeeded(arg.sirType)
                    resolvedArgType match
                        case tv1 @ SIRType.TypeVar(name, optId, isBuiltin) =>
                            val targetArgGen = lctx.typeGenerator(targetArgType)
                            val targetArgRepr =
                                if isBuiltin then targetArgGen.defaultRepresentation(targetArgType)
                                else targetArgGen.defaultTypeVarReperesentation(targetArgType)
                            val argTyped = new TypeRepresentationProxyLoweredValue(
                              arg,
                              targetArgType,
                              targetArgRepr,
                              inPos
                            )
                            (argTyped, true)
                        case other =>
                            val gen = lctx.typeGenerator(other)
                            val targetArgRepr =
                                if isBuiltin then gen.defaultRepresentation(other)
                                else gen.defaultTypeVarReperesentation(other)
                            val argTyped = new TypeRepresentationProxyLoweredValue(
                              arg,
                              other,
                              targetArgRepr,
                              inPos
                            )
                            (argTyped, false)
                case _ => (arg, false)
            }

            val argUpcasted = if !typeAligned && SIRType.isSum(targetArgType) then {
                if lctx.debug then lctx.log("targetArgType is sum, trying to upcast arg")
                argTypevarResolved
                    .maybeUpcast(targetArgType, inPos)
            } else {
                argTypevarResolved
            }

            val fRepresentationPair = f.representation match {
                case lr: LambdaRepresentation =>
                    lr.reprFun(argUpcasted.sirType, inPos)
                case TypeVarRepresentation(isBuiltin) =>
                    InOutRepresentationPair(
                      TypeVarRepresentation(isBuiltin),
                      TypeVarRepresentation(isBuiltin)
                    )
                case _ =>
                    throw LoweringException(
                      s"Function representation is not a lambda: ${f.representation.doc.render(100)}",
                      inPos
                    )
            }

            val targetArgRepresentation = fRepresentationPair.inRepr

            val argInTargetRepresentation =
                if SIRType.isPolyFunOrFun(argUpcasted.sirType) then {
                    // if we have a function, we need check, are we need to upcast their arguments
                    //  because it can be a polymorphic function with covariant argumets.
                    //  like Eq[Credential],
                    alignTypeArgumentsAndRepresentations(
                      argUpcasted,
                      targetArgType,
                      targetArgRepresentation,
                      inPos
                    )
                } else {
                    argUpcasted.toRepresentation(targetArgRepresentation, inPos)
                }

            val calculatedResType = SIRType.calculateApplyType(
              f.sirType,
              argInTargetRepresentation.sirType,
              Map.empty,
              debug = lctx.debug
            )
            if lctx.debug then lctx.log(s"lvApply1: calculatedResType = ${calculatedResType.show}")

            val resType = resTp match {
                case Some(tp) =>
                    // Unroll TypeProxy before using the type
                    val unrolledTp = SIRType.unrollTypeProxy(tp)
                    val tvGen = new SIRType.SetBasedTypeVarGenerationContext(Set.empty, 0L)
                    val ctOverlapp = tvGen.importSetFromType(calculatedResType)
                    val (resTps, resBody) = unrolledTp match {
                        case tl @ SIRType.TypeLambda(resTps, resBody) =>
                            val resOverlapp = tvGen.importSetFromType(tp)
                            if resOverlapp then {
                                val nResTps = resTps.map(x => tvGen.freshCopy(x))
                                val renames = resTps.zip(nResTps).toMap
                                val renamingContext = RenamingTypeVars.makeContext(renames, tvGen)
                                val nResBody = RenamingTypeVars.inType(resBody, renamingContext)
                                (nResTps, nResBody)
                            } else (resTps, resBody)
                        case SIRType.Fun(in, out) =>
                            // Unroll TypeProxy in the function argument position
                            val unrolledIn = SIRType.unrollTypeProxy(in)
                            if lctx.debug then {
                                lctx.log(
                                  s"lvApply: Unrolling Fun type: in=${in.show} -> unrolledIn=${unrolledIn.show}"
                                )
                            }
                            (Seq.empty, SIRType.Fun(unrolledIn, out))
                        case other =>
                            (Seq.empty, other)
                    }
                    val (ctRes, ctBody) = calculatedResType match {
                        case SIRType.TypeLambda(tps, body) => (tps, body)
                        case other                         => (List.empty, other)
                    }
                    if lctx.debug then {
                        lctx.log(s"lvApply: calulatedResType = ${calculatedResType.show}")
                        if resTp.nonEmpty then
                            lctx.log(
                              s"lvApply: resType = ${resTps.map(_.show).mkString(", ")} =>> ${resBody.show}))"
                            )
                        else lctx.log(s"lvApply: resType =${resBody.show}")
                        lctx.log(s"lvApply: About to unify:")
                        lctx.log(s"  ctBody  = ${ctBody.show}")
                        lctx.log(s"  resBody = ${resBody.show}")
                    }
                    SIRUnify.topLevelUnifyType(
                      ctBody,
                      resBody,
                      SIRUnify.Env.empty.withUpcasting // .setDebug(lctx.debug)
                    ) match {
                        case SIRUnify.UnificationSuccess(env, tp) =>
                            // tp is ok, now try get typevars only from resultedSize

                            val ctResFree = ctRes.filterNot(env.filledTypes.contains)
                            val resTpsFree = resTps.filterNot(tv =>
                                env.filledTypes.contains(tv) || env.eqTypes.contains(tv)
                            )
                            val resEqSubst = resTps
                                .flatMap(tv =>
                                    env.eqTypes
                                        .get(tv)
                                        .flatMap { eqSet =>
                                            eqSet
                                                .find(v => ctResFree.contains(v))
                                                .orElse(eqSet.find(v => !resTps.contains(v)))
                                                .map(x => (tv, x))
                                        }
                                )
                                .toMap
                            if lctx.debug then {
                                lctx.log(
                                  s"lvApply: resEqSubst = ${resEqSubst.map { case (k, v) => s"${k.show} -> ${v.show}" }.mkString(", ")}"
                                )
                            }

                            val renamingContext =
                                RenamingTypeVars.makeContext(resEqSubst, tvGen)
                            val ntp = RenamingTypeVars.inType(tp, renamingContext)
                            val (grounded, ungrounded) =
                                SIRType.partitionGround(ctResFree ++ resTpsFree, ntp)
                            if grounded.nonEmpty then SIRType.TypeLambda(grounded, ntp) else ntp
                        case failure @ SIRUnify.UnificationFailure(path, l, r) =>
                            val lString = l match
                                case tp: SIRType =>
                                    tp.show
                                case _ => l.toString
                            val rString = r match
                                case tp: SIRType =>
                                    tp.show
                                case _ => r.toString
                            throw LoweringException(
                              s"Cannot unify result type of apply: \n${tp.show}\n and\n${calculatedResType.show}.\n" +
                                  s"ctBody = ${ctBody.show}, resBody = ${resBody.show}\n" +
                                  s"Unification failure: path=${path}, left=${lString}, right=${rString}",
                              inPos
                            )
                    }
                case None =>

                    calculatedResType
            }

            val calculatedResRepr = fRepresentationPair.outRepr

            if lctx.debug then {
                lctx.log(
                  s"lvApply: before applied calculatedResType = ${calculatedResType.show}, calculatedResRep=${calculatedResRepr.show} resType = ${resType.show}"
                )
            }

            lctx.debug = prevDebug

            val applied = ApplyLoweredValue(
              f,
              argInTargetRepresentation,
              resType,
              calculatedResRepr,
              inPos
            )

            if lctx.debug then {
                lctx.log(
                  s"lvApply: applied = ${applied.pretty.render(100)}"
                )
            }

            val retval = {
                if SIRType.isPolyFunOrFun(resType) then
                    if lctx.debug then {
                        lctx.log(
                          s"lvApply: aligning type arguments and representations for resType = ${resType.show} representation = ${resRepr.getOrElse("None")}"
                        )
                        lctx.log(
                          s"resTp=${resTp.map(_.show)}, calculatedResType = ${calculatedResType.show}"
                        )
                        lctx.log(
                          s"lvApply: applied type = ${applied.sirType.show}, representation = ${applied.representation.doc.render(100)}"
                        )
                        lctx.log(
                          s"lvApply: applied = ${applied.pretty.render(100)}"
                        )

                    }
                    val resRepresentation = resRepr.getOrElse(
                      lctx.typeGenerator(resType).defaultRepresentation(resType)
                    )
                    val retval = alignTypeArgumentsAndRepresentations(
                      applied,
                      resType,
                      resRepresentation,
                      inPos
                    )
                    if lctx.debug then {
                        lctx.log(
                          s"lvApply: retval type after aligning = ${retval.sirType.show}"
                        )
                    }
                    retval
                else if SIRType.isSum(resType) then
                    val upcasted = applied.maybeUpcast(resType, inPos)
                    resRepr match
                        case Some(repr) =>
                            upcasted.toRepresentation(repr, inPos)
                        case None => upcasted
                else
                    resRepr match
                        case Some(repr) =>
                            applied.toRepresentation(repr, inPos)
                        case None =>
                            applied
            }

            if lctx.debug then
                lctx.log(
                  s"lvApply: retval = ${retval.pretty.render(100)}"
                )
                lctx.log(
                  s"lvApply: retval dominatedUplevelVars = ${retval.dominatingUplevelVars.map(_.id).mkString(", ")}"
                )
                lctx.log(
                  s"lvApply: retval usedUplevelVars = ${retval.usedUplevelVars.map(_.id).mkString(", ")}"
                )

            retval

        }

        def lvEqualsInteger(x: LoweredValue, y: LoweredValue, inPos: SIRPosition)(using
            lctx: LoweringContext
        ): LoweredValue = {
            val xc = x.toRepresentation(PrimitiveRepresentation.Constant, inPos)
            val yc = y.toRepresentation(PrimitiveRepresentation.Constant, inPos)
            BuilinApply2LoweredVale(
              SIRBuiltins.equalsInteger,
              SIRType.Boolean,
              PrimitiveRepresentation.Constant,
              inPos,
              xc,
              yc
            )
        }

        def lvLamAbs(
            name: String,
            tp: SIRType,
            inputRepresentation: LoweredValueRepresentation,
            f: IdentifiableLoweredValue => LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            lvLamAbs(
              SIR.Var(name, tp, AnnotationsDecl(inPos)),
              inputRepresentation,
              f,
              inPos
            )
        }

        def lvLamAbs(
            sirVar: SIR.Var,
            inputRepresentation: LoweredValueRepresentation,
            f: IdentifiableLoweredValue => LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            val newVar = new VariableLoweredValue(
              id = lctx.uniqueVarName(sirVar.name),
              name = sirVar.name,
              sir = sirVar,
              representation = inputRepresentation
            )

            // Make inner lambda parameter depend on the immediate outer lambda parameter
            // Since isDependFrom is transitive, this creates a dependency chain
            // Update BOTH directions: forward (dependFrom) and reverse (depended)
            lctx.enclosingLambdaParams.headOption.foreach { outerParam =>
                newVar.directDependFrom.add(outerParam)
                outerParam.directDepended.add(newVar)
            }

            val prevScope = lctx.scope
            val prevEnclosingParams = lctx.enclosingLambdaParams
            lctx.scope = lctx.scope.add(newVar)
            lctx.enclosingLambdaParams = newVar :: lctx.enclosingLambdaParams
            val body = f(newVar)(using lctx)
            lctx.scope = prevScope
            lctx.enclosingLambdaParams = prevEnclosingParams
            LambdaLoweredValue(newVar, body, inPos)
        }

        /** create let and add it to the current scope.
          */
        def lvNewLazyIdVar(
            id: String,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            rhs: LoweringContext ?=> LoweredValue,
            inPos: SIRPosition,
        )(using lctx: LoweringContext): IdentifiableLoweredValue = {
            val newVar: VariableLoweredValue = new VariableLoweredValue(
              id = id,
              name = id,
              sir = SIR.Var(id, tp, AnnotationsDecl(inPos)),
              representation = lvr,
              optRhs = Some(rhs(using lctx)),
            )
            lctx.scope = lctx.scope.add(newVar)
            newVar
        }

        def lvNewLazyNamedVar(
            name: String,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            rhs: LoweringContext ?=> LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): IdentifiableLoweredValue = {
            val newVar: VariableLoweredValue = new VariableLoweredValue(
              id = lctx.uniqueVarName(name),
              name = name,
              sir = SIR.Var(name, tp, AnnotationsDecl(inPos)),
              representation = lvr,
              optRhs = Some(rhs(using lctx)),
            )
            lctx.scope = lctx.scope.add(newVar)
            newVar
        }

        def lvBuiltinApply(
            fun: SIR.Builtin,
            arg: LoweredValue,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            inPos: SIRPosition
        )(using
            lctx: LoweringContext
        ): LoweredValue = {
            BuilinApply1LoweredVale(fun, tp, lvr, inPos, arg)
        }

        def lvIntConstant(
            value: Int,
            pos: SIRPosition
        )(using lctx: LoweringContext): ConstantLoweredValue = {
            ConstantLoweredValue(
              SIR.Const(Constant.Integer(value), SIRType.Integer, AnnotationsDecl(pos)),
              PrimitiveRepresentation.Constant
            )
        }

        def lvBoolConstant(value: Boolean, pos: SIRPosition)(using
            lctx: LoweringContext
        ): ConstantLoweredValue = {
            ConstantLoweredValue(
              SIR.Const(Constant.Bool(value), SIRType.Boolean, AnnotationsDecl(pos)),
              PrimitiveRepresentation.Constant
            )
        }

        def lvStringConstant(value: String, pos: SIRPosition)(using
            lctx: LoweringContext
        ): ConstantLoweredValue = {
            ConstantLoweredValue(
              SIR.Const(Constant.String(value), SIRType.String, AnnotationsDecl(pos)),
              PrimitiveRepresentation.Constant
            )
        }

        def lvBuiltinApply0(
            fun: SIR.Builtin,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            inPos: SIRPosition
        )(using
            lctx: LoweringContext
        ): LoweredValue = {
            StaticLoweredValue(
              SIR.Builtin(fun.bn, tp, AnnotationsDecl(inPos)),
              Lowering.forcedBuiltin(fun.bn) $ Term.Const(Constant.Unit),
              lvr,
              false
            )
        }

        def lvDataNil(inPos: SIRPosition)(using
            lctx: LoweringContext
        ): LoweredValue = {
            lvBuiltinApply0(
              SIRBuiltins.mkNilData,
              SIRType.List(SIRType.Data),
              SumDataList,
              inPos
            )
        }

        def lvBuiltinApply2(
            fun: SIR.Builtin,
            arg1: LoweredValue,
            arg2: LoweredValue,
            tp: SIRType,
            lvr: LoweredValueRepresentation,
            inPos: SIRPosition
        )(using
            lctx: LoweringContext
        ): LoweredValue = {
            BuilinApply2LoweredVale(
              fun,
              tp,
              lvr,
              inPos,
              arg1,
              arg2
            )
        }

        def lvCast(expr: LoweredValue, targetType: SIRType, inPos: SIRPosition)(using
            lctx: LoweringContext
        ): LoweredValue = {
            if lctx.debug then {
                lctx.log(
                  s"lvCast: expr.sirType = ${expr.sirType.show}, targetType = ${targetType.show}"
                )
            }

            def castedValue(
                value: LoweredValue = expr,
                changeRepresentation: Boolean = true,
                printWarning: Boolean = true
            ): LoweredValue = {
                if printWarning then {
                    lctx.warn(
                      s"casting unrelated types ${value.sirType.show} and ${targetType.show}",
                      inPos
                    )
                }
                val valueGen = lctx.typeGenerator(value.sirType)
                val tvRepr =
                    if changeRepresentation then
                        value.toRepresentation(
                          valueGen.defaultTypeVarReperesentation(value.sirType),
                          inPos
                        )
                    else value
                TypeRepresentationProxyLoweredValue(
                  tvRepr,
                  targetType,
                  tvRepr.representation,
                  inPos
                )
            }

            val unifyResult = SIRUnify.topLevelUnifyType(
              expr.sirType,
              targetType,
              SIRUnify.Env.empty.withoutUpcasting
            )

            unifyResult match {
                case SIRUnify.UnificationSuccess(_, tp) =>
                    // if we can unify types, we can return the expression as is
                    castedValue(changeRepresentation = false, printWarning = false)
                case SIRUnify.UnificationFailure(_, _, _) =>
                    if SIRType.isSum(targetType) then
                        val parentsSeq =
                            SIRUnify.subtypeSeq(expr.sirType, targetType, SIRUnify.Env.empty)
                        if parentsSeq.isEmpty then castedValue(printWarning = true)
                        else {
                            val upcasted = parentsSeq.tail.foldLeft(expr) { (s, e) =>
                                val result = s.upcastOne(e, inPos)
                                result
                            }
                            castedValue(
                              upcasted,
                              changeRepresentation = false,
                              printWarning = false
                            )
                        }
                    else
                        val printWarning =
                            targetType != SIRType.FreeUnificator && expr.sirType != SIRType.TypeNothing
                        castedValue(printWarning = printWarning)
            }

        }

        def lvLetRec(
            name: String,
            tp: SIRType,
            repr: LoweredValueRepresentation,
            rhs: LoweringContext ?=> IdentifiableLoweredValue => LoweredValue,
            body: LoweringContext ?=> IdentifiableLoweredValue => LoweredValue,
            inPos: SIRPosition
        )(using
            lctx: LoweringContext
        ): LetRecLoweredValue = {
            val id = lctx.uniqueVarName(name)
            val newVar = new VariableLoweredValue(
              id = id,
              name = name,
              sir = SIR.Var(name, tp, AnnotationsDecl(inPos)),
              representation = repr
            )
            LetRecLoweredValue(
              newVar,
              rhs(newVar),
              body(newVar),
              inPos
            )
        }

        def lvDelay(
            value: LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            DelayLoweredValue(value, inPos)
        }

        def lvForce(
            value: LoweredValue,
            inPos: SIRPosition
        )(using lctx: LoweringContext): LoweredValue = {
            ForceLoweredValue(value, inPos)
        }

        def lvTrace(message: String, value: LoweredValue)(using LoweringContext): LoweredValue = {
            lvBuiltinApply2(
              SIRBuiltins.trace,
              lvStringConstant(message, AnnotationsDecl.empty.pos),
              value,
              value.sirType,
              value.representation,
              AnnotationsDecl.empty.pos
            )
        }

    }

    private def alignTypeArgumentsAndRepresentations(
        arg: LoweredValue,
        targetType: SIRType,
        targetRepresentation: LoweredValueRepresentation,
        inPos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // arg

        if lctx.debug then {
            lctx.log(
              s"alignTypeArgumentsAndRepresentations: arg.sirType = ${arg.sirType.show}, targetType = ${targetType.show}, arg.representation = ${arg.representation.show}  targetRepresentation = ${targetRepresentation.show}"
            )
        }

        def alignWithChange(
            arg: LoweredValue,
            targetType: SIRType,
            targetRepresentation: LoweredValueRepresentation
        ): (LoweredValue, Boolean) = {

            val alignEnv =
                SIRUnify.topLevelUnifyType(
                  arg.sirType,
                  targetType,
                  SIRUnify.Env.empty.withUpcasting
                ) match
                    case SIRUnify.UnificationSuccess(env, _) => env
                    case SIRUnify.UnificationFailure(_, _, _) =>
                        throw LoweringException(
                          s"Cannot unify types ${arg.sirType.show} and ${targetType.show} at $inPos",
                          inPos
                        )

            def resolvedInAlign(tp: SIRType): SIRType = {
                if alignEnv.filledTypes.isEmpty then tp
                else SIRType.substitute(tp, alignEnv.filledTypes, Map.empty)
            }

            SIRType.collectPolyOrFun(arg.sirType) match {
                case Some((argParams, argIn, argOut)) =>
                    SIRType.collectPolyOrFun(targetType) match {
                        case Some((targetParams, targetIn, targetOut)) =>
                            val resolvedTargetIn = resolvedInAlign(targetIn)
                            val resolvedArgIn = resolvedInAlign(argIn)
                            val (targetInRepr, targetOutRepr) = targetRepresentation match {
                                case targetLambdaRepr: LambdaRepresentation =>
                                    val pair = targetLambdaRepr.reprFun(resolvedTargetIn, inPos)
                                    (
                                      pair.inRepr,
                                      pair.outRepr
                                    )
                                case TypeVarRepresentation(isBuiltin) =>
                                    (
                                      TypeVarRepresentation(isBuiltin),
                                      TypeVarRepresentation(isBuiltin)
                                    )
                                case _ =>
                                    throw LoweringException(
                                      s"Expected lambda representation, but got $targetRepresentation",
                                      inPos
                                    )
                            }
                            val (argInRepr, argOutRepr) =
                                arg.representation match {
                                    case argLambdaRepr: LambdaRepresentation =>
                                        val pair = argLambdaRepr.reprFun(resolvedArgIn, inPos)
                                        (
                                          pair.inRepr,
                                          pair.outRepr
                                        )
                                    case TypeVarRepresentation(isBuiltin) =>
                                        (
                                          TypeVarRepresentation(isBuiltin),
                                          TypeVarRepresentation(isBuiltin)
                                        )
                                    case _ =>
                                        throw LoweringException(
                                          s"Expected lambda representation, but got ${arg.representation} for arg\n${arg.show}",
                                          inPos
                                        )
                                }

                            // if both are functions, we need to align their type arguments
                            val runUpcast = SIRType.isSum(argIn) && SIRUnify
                                .topLevelUnifyType(
                                  resolvedTargetIn,
                                  resolvedArgIn,
                                  lctx.typeUnifyEnv.withoutUpcasting
                                )
                                .isFailure
                            val changeRepresentation =
                                !argInRepr.isCompatibleOn(argIn, targetInRepr, inPos)
                            val xId = lctx.uniqueVarName("xIn")
                            val xIn = VariableLoweredValue(
                              xId,
                              xId,
                              SIR.Var(xId, resolvedTargetIn, AnnotationsDecl(inPos)),
                              targetInRepr
                            )
                            var changed = false
                            val prevScope = lctx.scope
                            lctx.scope = lctx.scope.add(xIn)
                            val xInUpcased =
                                if runUpcast then xIn.maybeUpcast(resolvedArgIn, inPos) else xIn
                            if !(xInUpcased eq xIn) then changed = true
                            val xInAligned =
                                if SIRType.isPolyFunOrFun(resolvedTargetIn) then
                                    // arg = \\ lambda xIn: targetIn => (xIn'): argIn
                                    val (xInAligned, changed) =
                                        alignWithChange(xInUpcased, resolvedArgIn, argInRepr)
                                    xInAligned
                                else xInUpcased
                            if !(xInAligned eq xInUpcased) then changed = true
                            val yIn = xInAligned.toRepresentation(argInRepr, inPos)
                            if !(yIn eq xInAligned) then changed = true
                            val resolvedArgOut = resolvedInAlign(argOut)
                            val xOut =
                                ApplyLoweredValue(arg, yIn, resolvedArgOut, argOutRepr, inPos)
                            val resolvedTargetOut = resolvedInAlign(targetOut)
                            val (resOut, outChanged) =
                                alignWithChange(xOut, resolvedTargetOut, targetOutRepr)
                            if outChanged then changed = true
                            lctx.scope = prevScope
                            if !changed then (arg, false)
                            else {
                                /*
                                val nextArg = new ComplexLoweredValue(Set(xIn), resOut) {
                                    override def sirType: SIRType = targetType
                                    override def pos: SIRPosition = inPos
                                    override def termInternal(gctx: TermGenerationContext): Term = {
                                        val ngctx = gctx.addGeneratedVar(xIn.id)
                                        Term.LamAbs(xIn.id, resOut.termWithNeededVars(ngctx))
                                    }

                                    override def representation: LoweredValueRepresentation =
                                        targetRepresentation
                                    override def print: String = {
                                        s"Lam(${resOut.print},${xIn.id})"
                                    }
                                }
                                
                                 */
                                val nextArg = LambdaLoweredValue(xIn, resOut, inPos)
                                (nextArg, true)
                            }
                        case None =>
                            // arg is function but targetis npt. Passing function in type params is not supported yet
                            throw LoweringException(
                              s"Cannot apply function with type ${arg.sirType.show} to type ${targetType.show} at $inPos",
                              inPos
                            )
                    }
                case None =>
                    val retval = arg
                        .maybeUpcast(resolvedInAlign(targetType), arg.pos)
                        .toRepresentation(targetRepresentation, arg.pos)
                    val chanded = retval != arg
                    (retval, chanded)
            }

        }

        val (alignedArg, changed) = alignWithChange(arg, targetType, targetRepresentation)

        if lctx.debug then {
            lctx.log(
              s"alignTypeArgumentsAndRepresentations: alignedArg.type = ${alignedArg.sirType.show}, changed = $changed"
            )
        }
        if changed then alignedArg
        else arg

    }

    /** Common representation prerequisite: all values have the same type: targetType
      */
    def chooseCommonRepresentation(
        values: Seq[LoweredValue],
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValueRepresentation = {
        if values.isEmpty then
            throw LoweringException(
              "Cannot choose common type and representation for empty sequence of values",
              pos
            )
        val nonNothingValues = values.filter(_.sirType != SIRType.TypeNothing)
        val retval =
            if nonNothingValues.isEmpty then values.head.representation
            else
                val byRepresentation =
                    nonNothingValues.groupBy(_.representation).map((k, v) => (k, v.length)).toMap
                val nonErrored = byRepresentation.removed(ErrorRepresentation)
                if nonErrored.isEmpty then byRepresentation.head._1
                else {
                    val compatibleOn =
                        nonErrored.filter((lw, c) => lw.isCompatibleWithType(targetType))
                    if compatibleOn.isEmpty
                    then lctx.typeGenerator(targetType).defaultRepresentation(targetType)
                    else {
                        val candidates = compatibleOn.toSeq.sortBy(-_._2)
                        candidates
                            .find((r, c) => values.forall(v => r.isCompatibleWithType(v.sirType)))
                            .map(_._1)
                            .getOrElse(
                              lctx.typeGenerator(targetType).defaultRepresentation(targetType)
                            )
                    }
                }
        retval
    }

}
