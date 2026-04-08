package scalus.testing.regression.binocular20260408

import org.scalatest.funsuite.AnyFunSuite

/** Regression test: inline extension method on opaque type with type alias return type in a @Compile
  * object.
  *
  * Root cause: When an opaque type's inline extension method has a return type that is a type alias
  * (e.g., `type CompactBits = ByteString`), the Scala compiler creates a scope proxy binding during
  * inline expansion. The Scalus plugin's proxy filter only handles `$package` proxies from opaque
  * type scope resolution, but not the additional proxy binding introduced by the type alias in the
  * return type.
  *
  * Minimal reproduction (in BitcoinValidator.scala): opaque type BlockHeader = ByteString type
  * CompactBits = ByteString extension (self: BlockHeader) inline def bits: CompactBits =
  * self.slice(72, 4) // fails: type alias return inline def bits: ByteString = self.slice(72, 4) //
  * works: concrete return type
  *
  * Error pattern: Module <Validator>._$headerBits, referenced from var <Validator>$._$_$$proxy ...
  * is not found
  */
class OpaqueInlineExtensionTest extends AnyFunSuite {

    test("inline extension on opaque type with type alias return type in @Compile object") {
        // If BitcoinValidator.scala compiles without plugin error, the regression is fixed
        succeed
    }
}
