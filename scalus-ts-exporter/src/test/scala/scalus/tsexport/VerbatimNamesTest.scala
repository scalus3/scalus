package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

class VerbatimNamesTest extends AnyFunSuite {
    import VerbatimNames.referencedNames

    test("generic arguments and union members are references") {
        assert(
          referencedNames("Partial<ProtocolParamsLike>") == List("Partial", "ProtocolParamsLike")
        )
        assert(
          referencedNames("string | Uint8Array | Utxo") == List("string", "Uint8Array", "Utxo")
        )
    }

    test("string literal types are not references") {
        assert(referencedNames("\"Spend\" | \"Mint\" | 'Cert' | `Vote`") == Nil)
        assert(referencedNames("\"a \\\" Quoted\" | Real") == List("Real"))
    }

    test("property keys and parameter names are not references") {
        assert(
          referencedNames("{ readonly type: \"key\" | \"script\"; readonly hash: string }") ==
              List("readonly", "string")
        )
        assert(
          referencedNames("{ params: Uint8Array; poolId?: undefined }") ==
              List("Uint8Array", "undefined")
        )
        assert(referencedNames("(a: string, b?: Thing) => void") == List("string", "Thing", "void"))
        assert(referencedNames("{ [key: string]: Value }") == List("string", "Value"))
    }

    test("member names after a dot and names the text binds are not references") {
        assert(referencedNames("Ns.Member") == List("Ns"))
        assert(referencedNames("{ [K in keyof Shape]: Shape[K] }") == List("in", "keyof", "Shape"))
        assert(
          referencedNames("T extends Promise<infer U> ? U : never") ==
              List("T", "extends", "Promise", "infer", "never")
        )
    }

    test("number literal types are not references") {
        assert(referencedNames("0 | 1 | 2e3") == Nil)
    }

    private def property(tpe: TsType) =
        TsMember.Property("p", tpe, readonly = true, optional = false, static = false, doc = None)

    private def iface(name: String, members: List[TsMember], tps: List[TsTypeParam] = Nil) =
        TsDecl.Iface(name, tps, members, doc = None, inputOnly = false)

    test("an undeclared name is an error that names the member and the type") {
        val module = TsModule(
          List(iface("Opts", List(property(TsType.Verbatim("Partial<Missing>")))))
        )
        val errors = VerbatimNames.check(module)
        assert(errors.map(_.member) == List("Opts.p"))
        assert(errors.head.message.contains("'Missing'"))
    }

    test("declared names, aliases, builtins and type parameters in scope are accepted") {
        val module = TsModule(
          List(
            iface("Shape", Nil),
            TsDecl.Cls("Box", Nil, None, Nil, None, deprecatedAliases = List("OldBox")),
            iface(
              "Holder",
              List(
                property(
                  TsType.Union(
                    List(
                      TsType.Verbatim("readonly (Shape | OldBox | A)[]"),
                      TsType.Named("undefined")
                    )
                  )
                ),
                TsMember.Method(
                  "get",
                  List(
                    TsOverload(
                      List(TsTypeParam("B", None)),
                      Nil,
                      TsType.Verbatim("Promise<Record<string, B>>"),
                      None
                    )
                  ),
                  static = false
                )
              ),
              tps = List(TsTypeParam("A", None))
            )
          )
        )
        assert(VerbatimNames.check(module) == Nil)
    }

    test("a method type parameter is not in scope of another member") {
        val module = TsModule(
          List(
            TsDecl.Fun(
              "f",
              List(TsOverload(Nil, Nil, TsType.Verbatim("Array<B>"), None)),
              deprecatedAliases = Nil
            )
          )
        )
        assert(VerbatimNames.check(module).map(_.member) == List("f"))
    }
}
