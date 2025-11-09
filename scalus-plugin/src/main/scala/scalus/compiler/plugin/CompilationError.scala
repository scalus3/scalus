package scalus.compiler.plugin

import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.util.SrcPos
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.ast.tpd.ValDef

import scalus.compiler.sir.SIRType
import scalus.compiler.sir.SIRPosition

sealed trait CompilationError {
    def message: String
    def srcPos: SrcPos
}

case class SymbolNotFound(
    name: String,
    module: String,
    srcPos: SrcPos,
    refPos: SIRPosition,
    namesInModule: Set[String]
) extends CompilationError {
    def message: String =
        s"""Symbol not found: $name
           |Possible reasons and solutions:
           |  Make sure you added @Compile annotation to the object that contains '$name' in module ${module} 
           |  referenced from ${refPos.file}:${refPos.startLine}.
           |
           |  Maybe '$name' is not intended to be used in Scalus scripts.
           |
           |  Maybe you used $name by accident?
           |
           |  It can be a bug in Scalus. Please report it or contact us via Discord.
           |  
           |  Names listed in module:
           |  ${namesInModule}
           |""".stripMargin
}

case class LiteralTypeNotSupported(const: Constant, srcPos: SrcPos) extends CompilationError {
    def message: String = const.tag match
        case ByteTag => s"""Literals of type Byte can't be used in Scalus scripts.
                           |Use BigInt(${const.byteValue}) instead""".stripMargin
        case ShortTag =>
            s"""Literals of type Short can't be used in Scalus scripts.
               |Use BigInt(${const.shortValue}) instead""".stripMargin
        case CharTag =>
            s"""Literals of type Char can't be used in Scalus scripts.
               |Either use '${const.charValue}' numeric value as BigInt(${const.charValue.toInt})
               |or make a string literal "${const.charValue}".
               |""".stripMargin
        case IntTag =>
            s"""Literals of type Int can't be used in Scalus scripts.
               |Use BigInt(${const.intValue}) instead""".stripMargin
        case LongTag =>
            s"""Literals of type Long can't be used in Scalus scripts.
               |Use BigInt(${const.longValue}) instead""".stripMargin
        case FloatTag =>
            s"""Literals of type Float can't be used in Scalus scripts.
               |Try representing a float literal as BigInt with fixed decimal point""".stripMargin
        case DoubleTag =>
            s"""Literals of type Double can't be used in Scalus scripts.
               |Try representing a double literal as BigInt with fixed decimal point""".stripMargin
        case NullTag =>
            s"""Literal null can't be used in Scalus scripts.
               |Maybe use Option""".stripMargin
        case ClazzTag => s"""Class literals can't be used in Scalus scripts."""
        case _        => ""
}

case class UnsupportedPairFunction(fun: String, srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""Unsupported builtin Pair method '$fun'.
           |It's likely a Scalus bug. Please, report it via GitHub Issues or Discord""".stripMargin
}

case class PairConstructionError(
    a: Tree,
    tpe1: Tree,
    aIsLiteral: Boolean,
    aIsData: Boolean,
    b: Tree,
    tpe2: Tree,
    bIsLiteral: Boolean,
    bIsData: Boolean,
    srcPos: SrcPos
)(using
    Context
) extends CompilationError {

    def message: String =
        def showType(tpe: Tree, lit: Boolean, data: Boolean) = if lit then "is a literal"
        else if data then "is a Data"
        else s"is not a literal or a Data, but has type ${tpe.show}"
        s"""Plutus Pair can only be created either by 2 literals or 2 Data expressions, like
           |  Pair(true, false) or Pair(datum1, datum2)
           |
           |  In this case:
           |    - '${a.show}' ${showType(tpe1, aIsLiteral, aIsData)}
           |    - '${b.show}' ${showType(tpe2, bIsLiteral, bIsData)}
           |
           |  Try converting the arguments to Data or use Tuple expression like '(${a.show}, ${b.show})'
           |""".stripMargin
}

case class UnsupportedListFunction(fun: String, srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""Unsupported builtin List method '$fun'.
           |It's likely a Scalus bug. Please, report it via GitHub Issues or Discord""".stripMargin
}

case class UnsupportedListApplyInvocation(tree: Tree, tpe: Tree, srcPos: SrcPos)
    extends CompilationError {
    def message: String =
        s"""Unsupported builtin List.apply method invocation.
           |  It should be like List(a, b) or List[BigInt](1, 2, 3)""".stripMargin
}

case class NotBuiltinTypeInBuiltinListConstruction(tpe: Type, list: Tree)(using Context)
    extends CompilationError {
    val srcPos: SrcPos = list.srcPos
    def message: String =
        s"""Builtin List can only be used with builtin types, but
           |type ${tpe.show} is not a builtin type.
           |
           |Builtin types are: Unit, Boolean, String, ByteString, BigInt, Data, List, and Pair
           |
           |Try converting your values to Data or use 'scalus.prelude.List'
           |""".stripMargin
}

case class ReturnNotSupported(tree: Tree, srcPos: SrcPos)(using Context) extends CompilationError {
    def message: String =
        s"""return expressions can't be used in Scalus scripts
           |If this is the last expression in the script, you can just remove the 'return' keyword:
           |
           |  '${tree.showIndented(2)}'
           |""".stripMargin
}

case class ExpressionNotSupported(exprType: String, srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""$exprType is not supported in Scalus scripts
           |Try rewriting your program without using it""".stripMargin
}

case class UnsupportedBigIntOp(op: String, srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""BigInt operation '$op' is not supported in Scalus scripts.
           |Only +, -, *, /, %, <, <=, >, >=, ==, != are supported
           |Try rewriting your program without using it""".stripMargin
}

case class MissingConstructors(
    adtInfo: AdtTypeInfo,
    missingConstructors: Set[Symbol],
    srcPos: SrcPos
)(using Context)
    extends CompilationError {
    def message: String =
        val missing = missingConstructors.map(s => s"'${s.name}'").toBuffer.sorted.mkString(", ")
        s"""Missing cases for constructors ${missing} of type '${adtInfo.dataTypeSymbol.showFullName}'
           |
           |In Scalus script all constructors must be addressed
           |Please, add missing cases to the pattern-matching expression
           |Or add a wildcard pattern, like 'case _ => ...'""".stripMargin
}

case class GuardsNotSupported(srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""Pattern-matching guards can't be used in Scalus scripts
           |Try rewriting your program without using them
           |by moving the guard condition to the right-hand side of the pattern-matching expression
           |""".stripMargin
}

case class UnsupportedTopLevelBind(name: String, srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""You can't bind names in top-level pattern-matching expression in Scalus scripts.
           |Remove the '$name @' binding""".stripMargin
}

case class LiteralPattern(srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""Literal patterns can't be used in Scalus scripts.
           |Use a named binding instead""".stripMargin
}

case class UnsupportedBinding(name: String, srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""This type of binding of name '$name' is not supported in Scalus scripts.
           |You can only bind names to constructor parameters, like 'case MyType(a, b@Inner(c)) => ...'.
           |Remove the '$name @' binding""".stripMargin
}

case class UnsupportedMatchExpression(tree: Tree, srcPos: SrcPos) extends CompilationError {
    def message: String =
        s"""Unsupported pattern-matching expression.
           |Try rewriting your program without using it""".stripMargin
}

case class VarNotSupported(vd: ValDef, srcPos: SrcPos)(using Context) extends CompilationError {
    def message: String =
        s"""'var' keyword can't be used in Scalus scripts.
           |Try 'val ${vd.symbol.name} = ...' instead""".stripMargin
}

case class LazyValNotSupported(vd: ValDef, srcPos: SrcPos)(using Context) extends CompilationError {
    def message: String =
        s"""lazy vals can't be used in Scalus scripts.
           |Try 'val ${vd.symbol.name} = ...' instead""".stripMargin
}

case class TypeMismatch(name: String, expected: SIRType, actual: SIRType, srcPos: SrcPos)
    extends CompilationError {
    def message: String =
        s"""Type mismatch.
           |symbol: $name
           |Expected: ${expected.show}
           |Actual: ${actual.show}""".stripMargin
}

case class ExpectedTypeLambda(name: String, actual: SIRType, srcPos: SrcPos)
    extends CompilationError {
    def message: String =
        s"""Type of expression should be type-lambda, but it's not.
           |symbol: $name
           |Actual: ${actual.show}""".stripMargin
}

case class UnsupportedType(tp: Type, srcPos: SrcPos, msg: String = "")(using Context)
    extends CompilationError {
    def message: String =
        s"""Unsupported type ${tp.show} ${msg}
           |Try rewriting your program without using it""".stripMargin
}

case class GenericError(message: String, srcPos: SrcPos) extends CompilationError
