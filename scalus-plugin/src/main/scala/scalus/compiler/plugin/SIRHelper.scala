package scalus.compiler.plugin

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition, Spans, SrcPos}
import scalus.serialization.flat.{DecoderState, HSRIdentityHashMap, HashConsed, HashConsedDecoderState, HashConsedEncoderState, HashConsedReprRefFlat}
import scalus.sir.{AnnotationsDecl, SIRPosition}

import java.nio.charset.StandardCharsets
import scala.annotation.nowarn

/** Convert SIR to a [[Tree]] that represents that SIR by encoding it to a string and generating a
  * code that decodes it back.
  *
  * So a call to `compile` or `compileDebug` will be replaced with a string literal that contains
  * the encoded SIR and a call to `decodeStringLatin1` that will decode it back.
  *
  * {{{
  *   val sir = compile(true)
  * }}}
  * becomes
  * {{{
  *  val sir = decodeStringLatin1("...encoded SIR...")
  *  // that decodes to SIR.Const(true)
  * }}}
  * This is a bit of a hack. Otherwise, we need to convert every SIR node to a [[Tree]] manually by
  * something like
  * {{{
  *   val sirVar = requiredModule("scalus.sir.SIR.Var").requiredMethod("apply")
  *   val varTree = ref(sirVar).appliedTo(arg)
  *   ...
  * }}}
  * which is a lot of boilerplate. And we have [[Flat]] encoding for SIR, so we can use it.
  */
private def convertFlatToTree[T <: AnyRef](
    data: T,
    reprRefFlat: HashConsedReprRefFlat[T],
    toExprFlatSymbol: Symbol,
    span: Spans.Span,
    debug: Boolean,
)(using Context): Tree = {
    val bitSize = reprRefFlat.bitSizeHC(data, HashConsed.State.empty)
    val byteSize = ((bitSize + 1 /* for filler */ ) / 8) + 1
    /* minimum size */
    val encodedState = HashConsedEncoderState.withSize(byteSize)
    reprRefFlat.encodeHC(data, encodedState)
    encodedState.encode.filler()
    val bytes = encodedState.encode.result
    if debug then
        // try to decode the SIR back
        val decodeState = HashConsedDecoderState(DecoderState(bytes), HashConsed.State.empty)
        val decodedRef = reprRefFlat.decodeHC(decodeState)
        decodeState.runFinCallbacks()
        @nowarn val sirRer: T =
            decodedRef.finValue(decodeState.hashConsed, 0, new HSRIdentityHashMap)
        report.echo("ScalusPhace.convertSIRToTree: sir was decoded back successfully")

    /*
        We could generate Array[Byte] constant from bytes directly, like this:

        val bytesLiterals = bytes.map(b => Literal(Constant(b))).toList
        JavaSeqLiteral(bytesLiterals, TypeTree(defn.ByteType))

        But Scala 3.3.4 generates the array literal inside a method.
        That sometimes produces "Method too large" error. JVM has a limit of 64KB for a method.
        But for String's it appears to generate a `LDC` opcode loading the String from a constant pool.

        So we convert the bytes to a String in ISO_8859_1 encoding to get a one byte per character.
        It was Base64 encoded before, but it's 33% larger than the original bytes.
        We could fit two bytes in one character, but then it's not a valid UTF-16 string.

        We split the bytes into chunks of 65000 bytes, because the maximum size of a String literal is 65535 bytes.
        https://stackoverflow.com/questions/816142/strings-maximum-length-in-java-calling-length-method
        https://asm.ow2.io/javadoc/org/objectweb/asm/ByteVector.html#putUTF8(java.lang.String)

        But for some reason, it's not possible to create a string literal with 65535 bytes.
        45000 is a safe value that works.
     */
    val strings = (
      for bytes <- bytes.grouped(45000)
      yield
          val str = new String(bytes, StandardCharsets.ISO_8859_1)
          Literal(Constant(str)).withSpan(span): Tree
    ).toList
    // Concatenate all the strings: "str1" + "str2" + ...
    val concatenatedStrings =
        strings.reduce((lhs, rhs) => lhs.select(nme.Plus).appliedTo(rhs).withSpan(span))
    if debug then
        // save the SIR to a file for debugging purposes
        val groupedBytes = bytes.grouped(45000).toList
        val strings = groupedBytes.map { b =>
            Literal(Constant(new String(b, StandardCharsets.ISO_8859_1))).withSpan(span)
        }
        val parts = strings.map {
            case Literal(Constant(str: String)) => str
            case _ => throw new RuntimeException("Expected a string literal")
        }
        val codedStr = parts.mkString
        report.echo(s"string.length: ${codedStr.length},  nParts: ${parts.size}")
        val path = ctx.settings.outputDir.value.file.toPath
            .resolve(s"${ctx.compilationUnit.source.file.name}_${span.start}.sir")
        import java.nio.file.Files
        Files.createDirectories(path.getParent)
        Files.write(path, codedStr.getBytes(StandardCharsets.ISO_8859_1))
        report.echo(s"Scalus: saved SIR to ${path}")
        report.echo(s"Scalus: SIR size: ${codedStr.length} characters, ${bitSize} bits")
    // // Generate scalus.sir.ToExprHSSIRFlat.decodeStringLatin1(str1 + str2 + ...)
    // val toExprFlat = requiredModule("scalus.sir.ToExprHSSIRFlat")
    val decodeLatin1SIR = toExprFlatSymbol.requiredMethod("decodeStringLatin1")
    ref(toExprFlatSymbol).select(decodeLatin1SIR).appliedTo(concatenatedStrings).withSpan(span)
}

extension (singleton: SIRPosition.type)
    def fromSrcPos(pos: SrcPos)(using Context): SIRPosition =
        if pos.span.exists then
            SIRPosition(
              pos.startPos.source.path,
              pos.startPos.startLine,
              pos.startPos.startColumn,
              pos.endPos.endLine,
              pos.endPos.endColumn
            )
        else SIRPosition.empty

    def fromSourcePosition(
        pos: SourcePosition
    )(using @annotation.unused ctx: Context): SIRPosition =
        if pos.span.exists then
            SIRPosition(
              pos.source.path,
              pos.startLine,
              pos.startColumn,
              pos.endLine,
              pos.endColumn
            )
        else SIRPosition.empty

extension (singleton: AnnotationsDecl.type)
    def fromSrcPos(pos: SrcPos)(using Context): AnnotationsDecl =
        AnnotationsDecl(
          SIRPosition.fromSrcPos(pos)
        )

    def fromSourcePosition(pos: SourcePosition)(using Context): AnnotationsDecl =
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(pos)
        )

    def fromSym(sym: Symbol)(using Context): AnnotationsDecl =
        val optComment = sym.defTree match
            case memberDef: MemberDef => memberDef.rawComment.map(_.raw)
            case _                    => None
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(sym.sourcePos),
          optComment
        )

    def fromSymIn(sym: Symbol, inPos: SourcePosition)(using Context): AnnotationsDecl =
        val optComment = sym.defTree match
            case memberDef: MemberDef => memberDef.rawComment.map(_.raw)
            case _                    => None
        val pos =
            if sym.sourcePos == NoSourcePosition then inPos
            else if inPos == NoSourcePosition then sym.sourcePos
            else inPos
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(pos),
          optComment
        )

extension (pos: SourcePosition)
    infix def union(other: SourcePosition): SourcePosition =
        if pos == NoSourcePosition then other
        else if other == NoSourcePosition then pos
        else SourcePosition(pos.source, pos.span.union(other.span), NoSourcePosition)

def createSIRPositionTree(pos: SIRPosition, span: Span)(using Context): Tree = {
    val posModule = Symbols.requiredModule("scalus.sir.SIRPosition")
    val posTree = ref(posModule).select(posModule.requiredMethod("apply"))
    posTree
        .appliedTo(
          Literal(Constant(pos.file)),
          Literal(Constant(pos.startLine)),
          Literal(Constant(pos.startColumn)),
          Literal(Constant(pos.endLine)),
          Literal(Constant(pos.endColumn))
        )
        .withSpan(span)
}
