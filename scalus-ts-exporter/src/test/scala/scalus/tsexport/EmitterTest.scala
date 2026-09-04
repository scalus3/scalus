package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

import TsType.*

class EmitterTest extends AnyFunSuite {
    private val num = Named("number")
    private val str = Named("string")

    test("input positions render readonly arrays, output positions do not") {
        // a caller holding a ReadonlyArray must be able to pass it; what we hand BACK stays
        // mutable, because consumers do getAllUtxos().map(...).push(...)
        val iface = TsDecl.Iface(
          "Cfg",
          Nil,
          List(
            TsMember.Property(
              "xs",
              Arr(Named("X")),
              readonly = true,
              optional = true,
              static = false,
              None
            ),
            TsMember.Property(
              "grid",
              Arr(Arr(num)),
              readonly = true,
              optional = false,
              static = false,
              None
            )
          ),
          None,
          inputOnly = true
        )
        val out = Emitter.emit(TsModule(List(iface)))
        assert(out.contains("readonly xs?: readonly X[];"))
        assert(out.contains("readonly grid: readonly (readonly number[])[];"))

        val cls = TsDecl.Cls(
          "C",
          Nil,
          None,
          List(
            TsMember.Ctor(List(List(TsParam("cfg", Arr(Named("X")), false))), None),
            TsMember.Method(
              "all",
              List(TsOverload(Nil, Nil, Arr(Named("Uint8Array")), None)),
              static = false
            ),
            TsMember
                .Property("logs", Arr(str), readonly = true, optional = false, static = false, None)
          ),
          None,
          Nil
        )
        val outC = Emitter.emit(TsModule(List(cls)))
        assert(outC.contains("constructor(cfg: readonly X[]);"))
        assert(outC.contains("all(): Uint8Array[];"))
        assert(outC.contains("readonly logs: string[];"))
    }

    test("renders types") {
        assert(Emitter.render(Union(List(str, Named("undefined")))) == "string | undefined")
        assert(Emitter.render(Arr(Union(List(num, Named("null"))))) == "(number | null)[]")
        assert(Emitter.render(Index(str)) == "{ [key: string]: string }")
        assert(
          Emitter.render(Func(List(TsParam("x", num, false)), str)) == "(x: number) => string"
        )
        assert(Emitter.render(Generic("Promise", List(str))) == "Promise<string>")
        assert(Emitter.render(Intersect(List(Named("A"), Named("B")))) == "A & B")
        // & binds tighter than |, so only the union member needs parentheses
        assert(
          Emitter.render(Intersect(List(Named("A"), Union(List(num, str))))) ==
              "A & (number | string)"
        )
        assert(
          Emitter.render(Union(List(Intersect(List(Named("A"), Named("B"))), num))) ==
              "A & B | number"
        )
        assert(Emitter.render(Arr(Intersect(List(Named("A"), Named("B"))))) == "(A & B)[]")
        assert(Emitter.render(Verbatim("\"key\" | \"script\"")) == "\"key\" | \"script\"")
    }

    test("emits a class with ctor, method overloads, static and readonly members") {
        val cls = TsDecl.Cls(
          "Emu",
          Nil,
          None,
          List(
            TsMember.Ctor(List(List(TsParam("a", num, false), TsParam("b", str, true))), None),
            TsMember.Method(
              "submit",
              List(
                TsOverload(
                  Nil,
                  List(TsParam("tx", Named("Uint8Array"), false)),
                  Named("SubmitResult"),
                  Some(TsDoc(List("Submits a transaction.")))
                ),
                TsOverload(
                  Nil,
                  List(
                    TsParam("tx", Named("Uint8Array"), false),
                    TsParam("dbg", Index(str), false)
                  ),
                  Named("SubmitResult"),
                  Some(TsDoc(List("Submits a transaction with debug scripts.")))
                )
              ),
              static = false
            ),
            TsMember.Property("slot", num, readonly = true, optional = false, static = false, None),
            TsMember.Property(
              "mainnet",
              Named("Emu"),
              readonly = true,
              optional = false,
              static = true,
              None
            )
          ),
          None,
          deprecatedAliases = Nil
        )
        val out = Emitter.emit(TsModule(List(cls)))
        assert(out.contains("export class Emu {"))
        assert(out.contains("constructor(a: number, b?: string);"))
        assert(out.contains("submit(tx: Uint8Array): SubmitResult;"))
        assert(
          out.contains("submit(tx: Uint8Array, dbg: { [key: string]: string }): SubmitResult;")
        )
        // one doc block per overload signature, not just for the first
        assert(out.contains("  /** Submits a transaction. */\n  submit(tx: Uint8Array)"))
        assert(out.contains("  /** Submits a transaction with debug scripts. */\n  submit(tx"))
        assert(out.contains("readonly slot: number;"))
        assert(out.contains("static readonly mainnet: Emu;"))
    }

    test("emits interface, function overloads, const object, docs, aliases; sorted; header") {
        val doc = TsDoc(List("Adds.", "@param x the x", "@returns sum"))
        val decls = List(
          TsDecl.Fun(
            "zeta",
            List(TsOverload(Nil, List(TsParam("x", num, false)), num, Some(doc))),
            deprecatedAliases = List("oldZeta")
          ),
          TsDecl.Iface(
            "Alpha",
            Nil,
            List(
              TsMember.Property("v", num, readonly = false, optional = true, static = false, None)
            ),
            None,
            inputOnly = false
          ),
          TsDecl.ConstObj(
            "Scalus",
            List(TsMember.Method("run", List(TsOverload(Nil, Nil, num, None)), static = false)),
            Some(TsDoc(List("@deprecated Use top-level functions.")))
          ),
          TsDecl.Cls("Beta", Nil, None, Nil, None, deprecatedAliases = List("OldBeta"))
        )
        val out = Emitter.emit(TsModule(decls))
        // header
        assert(out.startsWith("// Generated by scalus-ts-exporter. DO NOT EDIT."))
        // alphabetical order of declarations: Alpha, Beta, Scalus, zeta
        val order = List("interface Alpha", "class Beta", "const Scalus", "function zeta")
            .map(s => out.indexOf(s))
        assert(order == order.sorted && order.forall(_ >= 0), s"order was $order")
        // pieces
        assert(out.contains("export interface Alpha {"))
        assert(out.contains("v?: number;"))
        assert(out.contains("export function zeta(x: number): number;"))
        assert(out.contains("/**\n * Adds.\n * @param x the x\n * @returns sum\n */"))
        assert(out.contains("export const Scalus: {"))
        assert(out.contains("run(): number;"))
        // deprecated alias exported after everything else
        assert(out.contains("/** @deprecated Use Beta instead. */"))
        assert(out.contains("export { Beta as OldBeta };"))
        // functions get deprecated aliases too
        assert(out.contains("/** @deprecated Use zeta instead. */"))
        assert(out.contains("export { zeta as oldZeta };"))
        // generic class type params
        val gen = TsDecl.Cls("Box", List(TsTypeParam("A", None)), None, Nil, None, Nil)
        assert(Emitter.emit(TsModule(List(gen))).contains("export class Box<A> {"))
        // a native JavaScript base is named, so the consumer sees what it inherits
        val err = TsDecl.Cls("Boom", Nil, Some("Error"), Nil, None, Nil)
        assert(Emitter.emit(TsModule(List(err))).contains("export class Boom extends Error {"))
        val genErr =
            TsDecl.Cls("BoxErr", List(TsTypeParam("A", None)), Some("Error"), Nil, None, Nil)
        assert(
          Emitter.emit(TsModule(List(genErr))).contains("export class BoxErr<A> extends Error {")
        )
        // generic interface, generic method, bounded type parameter
        val iface = TsDecl.Iface(
          "Pair",
          List(TsTypeParam("A", None), TsTypeParam("B", Some(Named("object")))),
          List(
            TsMember.Method(
              "pick",
              List(
                TsOverload(
                  List(TsTypeParam("T", Some(Named("object")))),
                  List(TsParam("t", Named("T"), false)),
                  Generic("Box", List(Named("T"))),
                  None
                )
              ),
              static = false
            )
          ),
          None,
          inputOnly = false
        )
        val ifaceOut = Emitter.emit(TsModule(List(iface)))
        assert(ifaceOut.contains("export interface Pair<A, B extends object> {"))
        assert(ifaceOut.contains("pick<T extends object>(t: T): Box<T>;"))
        // no trailing whitespace anywhere, deterministic double-run
        assert(!out.linesIterator.exists(_.endsWith(" ")))
        assert(out == Emitter.emit(TsModule(decls)))
    }
}
