package scalus.compiler.sir

import scala.quoted.*

object SIRMacro {

    inline def emptyAnnotationsDecl: AnnotationsDecl = ${ emptyAnnotationsDeclImpl }

    def emptyAnnotationsDeclImpl(using qctx: Quotes): Expr[AnnotationsDecl] = {
        val scalaPosition = qctx.reflect.Position.ofMacroExpansion
        '{
            AnnotationsDecl(
              SIRPosition(
                file = ${ Expr(scalaPosition.sourceFile.path) },
                startLine = ${ Expr(scalaPosition.startLine) },
                startColumn = ${ Expr(scalaPosition.startColumn) },
                endLine = ${ Expr(scalaPosition.endLine) },
                endColumn = ${ Expr(scalaPosition.endColumn) },
                inlinedFrom = Nil
              ),
              comment = None,
              data = Map.empty
            )
        }
    }

}
