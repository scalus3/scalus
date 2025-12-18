package scalus.compiler.sir

import scalus.uplc.{Constant, DefaultFun}

object SirDSL:

    def extractAnnotated(sir: SIR): AnnotatedSIR =
        sir match
            case ansir: AnnotatedSIR => ansir
            case SIR.Decl(data, term) =>
                extractAnnotated(term)

    def applyToList(app: SIR): (SIR, List[SIR]) =
        app match
            case SIR.Apply(f, arg, tp, _) =>
                val (f1, args) = applyToList(f)
                (f1, args :+ arg)
            case f => (f, Nil)

    // flatten LamAbs into a list of names and the body
    def lamAbsToList(lam: SIR): (List[String], SIR) =
        lam match
            case SIR.LamAbs(svar, body, _, _) =>
                val (names, body1) = lamAbsToList(body)
                (svar.name :: names, body1)
            case body => (Nil, body)

    def λ(names: String*)(term: SIR): SIR = lam(names*)(term)
    def lam(names: String*)(term: SIR): SIR =
        names.foldRight(term)((e, s) =>
            SIR.LamAbs(
              SIR.Var(e, s.tp, AnnotationsDecl.empty),
              s,
              List.empty,
              AnnotationsDecl.empty
            )
        )
    extension (term: SIR)
        infix def $(rhs: SIR): AnnotatedSIR =
            SIR.Apply(
              extractAnnotated(term),
              extractAnnotated(rhs),
              SIRType.calculateApplyType(term.tp, rhs.tp, Map.empty),
              AnnotationsDecl.empty
            )

    given Conversion[DefaultFun, SIR] with
        def apply(bn: DefaultFun): SIR = SIRBuiltins.fromUplc(bn)

    given constantAsTerm[A: Constant.LiftValue]: Conversion[A, SIR] with
        def apply(c: A): SIR = {
            val lifted = summon[Constant.LiftValue[A]].lift(c)
            SIR.Const(lifted, SIRType.fromDefaultUni(lifted.tpe), AnnotationsDecl.empty)
        }

    given Conversion[Constant, SIR] with
        def apply(c: Constant): SIR =
            c match
                case Constant.Integer(value) => SIR.Const(c, SIRType.Integer, AnnotationsDecl.empty)
                case Constant.ByteString(value) =>
                    SIR.Const(c, SIRType.ByteString, AnnotationsDecl.empty)
                case Constant.String(value) => SIR.Const(c, SIRType.String, AnnotationsDecl.empty)
                case Constant.Unit          => SIR.Const(c, SIRType.Unit, AnnotationsDecl.empty)
                case Constant.Bool(value)   => SIR.Const(c, SIRType.Boolean, AnnotationsDecl.empty)
                case Constant.Data(value)   => SIR.Const(c, SIRType.Data.tp, AnnotationsDecl.empty)
                case Constant.List(elemType, value) =>
                    SIR.Const(
                      c,
                      SIRType.BuiltinList(SIRType.fromDefaultUni(elemType)),
                      AnnotationsDecl.empty
                    )
                case Constant.Array(elemType, value) =>
                    SIR.Const(
                      c,
                      SIRType.BuiltinArray(SIRType.fromDefaultUni(elemType)),
                      AnnotationsDecl.empty
                    )
                case Constant.Pair(a, b) =>
                    SIR.Const(
                      c,
                      SIRType.BuiltinPair(
                        SIRType.fromDefaultUni(a.tpe),
                        SIRType.fromDefaultUni(b.tpe)
                      ),
                      AnnotationsDecl.empty
                    )
                case Constant.BLS12_381_G1_Element(value) =>
                    SIR.Const(c, SIRType.BLS12_381_G1_Element, AnnotationsDecl.empty)
                case Constant.BLS12_381_G2_Element(value) =>
                    SIR.Const(c, SIRType.BLS12_381_G2_Element, AnnotationsDecl.empty)
                case Constant.BLS12_381_MlResult(value) =>
                    SIR.Const(c, SIRType.BLS12_381_MlResult, AnnotationsDecl.empty)
                case Constant.BuiltinValue(value) =>
                    SIR.Const(c, SIRType.BuiltinValue, AnnotationsDecl.empty)
