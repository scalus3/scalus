package scalus.verify

import scalus.compiler.sir.{ConstrDecl, SIR, SIRType}

/** What the verifier reads of SIR types: their declarations, arguments and fields. */
private[verify] object SirTypes {

    /** A type without the wrappers that leave it what it is: annotations and type proxies. */
    def unwrap(tp: SIRType): SIRType = tp match
        case SIRType.Annotated(inner, _)           => unwrap(inner)
        case SIRType.TypeProxy(ref) if ref != null => unwrap(ref)
        case other                                 => other

    /** `Data` itself, and not one of its constructors. */
    def isWholeData(tp: SIRType): Boolean = tp match
        case SIRType.SumCaseClass(decl, _) => decl.name == SIRType.Data.name
        case _                             => false

    /** The name of a type's declaration, or none for a type that has no declaration. */
    def declarationName(tp: SIRType): String = tp match
        case SIRType.SumCaseClass(declaration, _)    => declaration.name
        case SIRType.CaseClass(constructor, _, None) => constructor.name
        case _                                       => ""

    /** The type arguments of a type, as written: `List[A]` has `A`. */
    def typeArgumentsOf(tp: SIRType): List[SIRType] = unwrap(tp) match
        case SIRType.SumCaseClass(_, arguments) => arguments
        case SIRType.CaseClass(_, arguments, _) => arguments
        case _                                  => Nil

    /** The type arguments of a constructor of a type with `typeArguments`. A constructor passes its
      * own type parameters on to the type's: there, each is the type argument it is passed as. None
      * where a parameter of the constructor is passed on otherwise.
      */
    def ownArguments(
        constructor: ConstrDecl,
        typeArguments: List[SIRType]
    ): Option[List[SIRType]] = {
        val passed = constructor.parentTypeArgs
            .zip(typeArguments)
            .collect { case (parameter: SIRType.TypeVar, argument) => parameter -> argument }
            .toMap
        val own = constructor.typeParams.map(passed.get)
        Option.when(!own.contains(None))(own.flatten)
    }

    /** The types of the fields of a constructor with its own `typeArguments`. */
    def fieldTypes(constructor: ConstrDecl, typeArguments: List[SIRType]): List[SIRType] = {
        val arguments = constructor.typeParams.zip(typeArguments).toMap
        constructor.params.map(field => SIRType.substitute(field.tp, arguments, Map.empty))
    }

    /** `tp` as a key of a cache: the names of what it is made of, its arguments, and the values of
      * its annotations. A type itself does not do: each decode of a SIR has proxies of its own for
      * a type that contains itself, which compare by identity, and a type shown does not tell the
      * values of its annotations apart.
      */
    def typeKey(tp: SIRType): String = {
        def annotation(value: SIR): String = value match
            case SIR.Const(constant, _, _) => constant.toString
            case other                     => other.##.toString
        def key(tp: SIRType, depth: Int): String =
            if depth > maxNesting then "…"
            else
                def arguments(types: List[SIRType]) = types.map(key(_, depth + 1)).mkString(",")
                tp match
                    case SIRType.Annotated(inner, annotations) =>
                        val values = annotations.data.toList.sortBy(_._1).map { (name, value) =>
                            s"$name=${annotation(value)}"
                        }
                        s"@[${values.mkString(",")}] ${key(inner, depth + 1)}"
                    case SIRType.TypeProxy(ref) if ref != null => key(ref, depth + 1)
                    case SIRType.CaseClass(constructor, types, _) =>
                        s"${constructor.name}[${arguments(types)}]"
                    case SIRType.SumCaseClass(declaration, types) =>
                        s"${declaration.name}[${arguments(types)}]"
                    case SIRType.Fun(in, out) =>
                        s"(${key(in, depth + 1)} => ${key(out, depth + 1)})"
                    case SIRType.TypeLambda(params, body) =>
                        s"[${params.map(_.name).mkString(",")}] ${key(body, depth + 1)}"
                    case other => other.show
        key(tp, 0)
    }

    /** How deep the types of a variable's fields may nest: deep enough for any type of the ledger,
      * and an end to a type that contains itself under other arguments each time.
      */
    val maxNesting = 32
}
