package scalus.tsexport

import org.scalatest.funsuite.AnyFunSuite

class ExportCollectorTest extends AnyFunSuite {
    private lazy val result = ExportCollector.collect(
      List(InspectorFixture.fixtureClasses),
      InspectorFixture.fixtureClasspath,
      InspectorFixture.sourceRoot
    )

    private def decl(name: String): TsDecl =
        result.module.decls
            .find(_.name == name)
            .getOrElse(fail(s"no declaration named $name; have ${result.module.decls.map(_.name)}"))

    test("js.Object class exports all public members; ctor from primary constructor") {
        val p = decl("Point").asInstanceOf[TsDecl.Cls]
        assert(p.members.exists {
            case TsMember.Ctor(List(ps), _) => ps.map(_.name) == List("x", "y")
            case _                          => false
        })
        assert(p.members.exists {
            case m: TsMember.Method => m.name == "dist"
            case _                  => false
        })
        assert(p.doc.get.lines.head == "A point.")
    }

    test("non-js.Object class exports only @JSExport members") {
        val p = decl("Partial").asInstanceOf[TsDecl.Cls]
        assert(p.members.collect { case m: TsMember.Method => m.name } == List("visible"))
        assert(!p.members.exists {
            case p: TsMember.Property => true
            case _                    => false
        })
    }

    test("statics from companion @JSExportStatic; default param optional") {
        val s = decl("Statics").asInstanceOf[TsDecl.Cls]
        val make = s.members.collectFirst { case m: TsMember.Method if m.name == "make" => m }.get
        assert(make.static)
        assert(make.overloads.head.params.last.optional) // tag: String = "x"
        assert(s.members.exists {
            case p: TsMember.Property => p.name == "mainnet" && p.static && p.readonly
            case _                    => false
        })
    }

    test("multiple JSExportTopLevel: first canonical, rest deprecated aliases") {
        val r = decl("NewName").asInstanceOf[TsDecl.Cls]
        assert(r.deprecatedAliases == List("OldName"))
        assert(!result.module.decls.exists(_.name == "OldName"))
    }

    test("generics; getters readonly; var mutable; trailing UndefOr optional; overloads") {
        assert(decl("Box").asInstanceOf[TsDecl.Cls].typeParams == List(TsTypeParam("A", None)))
        val k = decl("Kitchen").asInstanceOf[TsDecl.Cls]
        assert(k.members.exists {
            case p: TsMember.Property => p.name == "getter" && p.readonly
            case _                    => false
        })
        assert(k.members.exists {
            case p: TsMember.Property => p.name == "rw" && !p.readonly
            case _                    => false
        })
        assert(k.members.exists {
            case p: TsMember.Property => p.name == "ro" && p.readonly
            case _                    => false
        })
        val opt = k.members.collectFirst { case m: TsMember.Method if m.name == "opt" => m }.get
        assert(opt.overloads.head.params.map(_.optional) == List(false, true, true))
        // optional-by-UndefOr params drop the "| undefined" from their type
        assert(opt.overloads.head.params(1).tpe == TsType.Named("string"))
        val overl =
            k.members.collectFirst { case m: TsMember.Method if m.name == "overloaded" => m }.get
        assert(overl.overloads.size == 2)
        val dflt = k.members.collectFirst { case m: TsMember.Method if m.name == "dflt" => m }.get
        assert(dflt.overloads.head.params.map(_.optional) == List(false, true))
    }

    test("applied type arguments are rendered, not dropped") {
        val g = decl("Generics").asInstanceOf[TsDecl.Cls]
        def m(n: String): TsMember.Method =
            g.members.collectFirst { case m: TsMember.Method if m.name == n => m }.get
        val str = TsType.Named("string")
        val num = TsType.Named("number")
        assert(m("getBox").overloads.head.ret == TsType.Generic("Box", List(str)))
        assert(
          m("boxes").overloads.head.params.head.tpe ==
              TsType.Arr(TsType.Generic("Box", List(str)))
        )
        assert(
          m("boxes").overloads.head.ret ==
              TsType.Arr(TsType.Generic("Box", List(TsType.Arr(num))))
        )
        val pair = m("pair").overloads.head
        assert(pair.params.head.tpe == TsType.Generic("GenPair", List(str, num)))
        assert(
          pair.ret == TsType.Generic("GenPair", List(num, TsType.Generic("Box", List(str))))
        )
    }

    test("method type parameters are collected, with bounds") {
        val g = decl("Generics").asInstanceOf[TsDecl.Cls]
        def m(n: String): TsMember.Method =
            g.members.collectFirst { case m: TsMember.Method if m.name == n => m }.get
        assert(m("pick").overloads.head.typeParams == List(TsTypeParam("A", None)))
        assert(m("pick").overloads.head.ret == TsType.Named("A"))
        assert(
          m("widen").overloads.head.typeParams ==
              List(TsTypeParam("A", Some(TsType.Named("object"))))
        )
    }

    test("generic chased traits and classes declare their type parameters") {
        val pair = decl("GenPair").asInstanceOf[TsDecl.Iface]
        assert(pair.typeParams == List(TsTypeParam("A", None), TsTypeParam("B", None)))
        assert(pair.members.exists {
            case p: TsMember.Property => p.name == "first" && p.tpe == TsType.Named("A")
            case _                    => false
        })
        assert(decl("Box").asInstanceOf[TsDecl.Cls].typeParams == List(TsTypeParam("A", None)))
        assert(
          decl("BoundedBox").asInstanceOf[TsDecl.Cls].typeParams ==
              List(TsTypeParam("A", Some(TsType.Named("object"))))
        )
    }

    test("@JSExport rename arguments name and group members") {
        val r = decl("Renames").asInstanceOf[TsDecl.Cls]
        val methods = r.members.collect { case m: TsMember.Method => m.name }
        // the exported name wins over the Scala name
        assert(methods.contains("evaluate"))
        assert(!methods.contains("evaluateScript"))
        // overloads group under the exported name, across differing Scala names
        val evaluate =
            r.members.collectFirst { case m: TsMember.Method if m.name == "evaluate" => m }.get
        assert(evaluate.overloads.size == 2)
        val run = r.members.collectFirst { case m: TsMember.Method if m.name == "run" => m }.get
        assert(run.overloads.size == 2)
        assert(!methods.contains("runNumber") && !methods.contains("runString"))
        // a member carrying both a bare and a named @JSExport is emitted under both names
        assert(methods.contains("both") && methods.contains("aliased"))
        // fields honour the rename too
        assert(r.members.exists {
            case p: TsMember.Property => p.name == "ver"
            case _                    => false
        })
        assert(!r.members.exists {
            case p: TsMember.Property => p.name == "version"
            case _                    => false
        })
        // unannotated members of a non-js.Object class stay unexported
        assert(!methods.contains("internal"))
        // under @JSExportAll semantics the alias is additional, not a replacement
        val all = decl("RenamesAll").asInstanceOf[TsDecl.Cls]
        assert(
          all.members.collect { case m: TsMember.Method => m.name }.sorted ==
              List("extra", "plain")
        )
    }

    test("every overload keeps its own doc") {
        val r = decl("Renames").asInstanceOf[TsDecl.Cls]
        def m(n: String): TsMember.Method =
            r.members.collectFirst { case m: TsMember.Method if m.name == n => m }.get
        assert(
          m("evaluate").overloads.map(_.doc.map(_.lines.head)) ==
              List(Some("Evaluates a script."), Some("Evaluates a script against a budget."))
        )
        assert(
          m("run").overloads.map(_.doc.map(_.lines.head)) ==
              List(
                Some("Two differently named Scala methods share one exported name."),
                Some("The string arm of `run`.")
              )
        )
    }

    test("constructor-parameter val docs survive; @constructor becomes the ctor doc") {
        val r = decl("Rect").asInstanceOf[TsDecl.Cls]
        assert(r.members.exists {
            case p: TsMember.Property =>
                p.name == "width" && p.doc.map(_.lines) == Some(
                  List("The width, measured in pixels.")
                )
            case _ => false
        })
        // the undocumented sibling must not inherit its neighbour's comment
        assert(r.members.exists {
            case p: TsMember.Property => p.name == "height" && p.doc.isEmpty
            case _                    => false
        })
        val ctor = r.members.collectFirst { case c: TsMember.Ctor => c }.get
        assert(
          ctor.doc.map(_.lines) == Some(List("Creates a rectangle from its width and height."))
        )
        // @constructor is moved off the class doc, the rest of it stays
        assert(r.doc.get.lines == List("A rectangle.", "", "@param width the width in pixels"))
    }

    test("a preceding one-line annotated definition does not donate its doc") {
        val o = decl("OneLiners").asInstanceOf[TsDecl.Cls]
        def m(n: String): TsMember.Method =
            o.members.collectFirst { case m: TsMember.Method if m.name == n => m }.get
        assert(m("a").overloads.head.doc.map(_.lines) == Some(List("Doc for a.")))
        assert(m("b").overloads.head.doc.isEmpty)
    }

    test("@JSExportStatic rename argument is honoured") {
        val s = decl("Statics").asInstanceOf[TsDecl.Cls]
        val statics = s.members.collect { case m: TsMember.Method if m.static => m.name }
        assert(statics.contains("of"))
        assert(!statics.contains("create"))
    }

    test("TsType override wins; TsName renames chased interfaces; transitive chase") {
        val k = decl("Kitchen").asInstanceOf[TsDecl.Cls]
        val ct = k.members.collectFirst { case m: TsMember.Method if m.name == "credType" => m }.get
        assert(ct.overloads.head.ret == TsType.Verbatim("\"key\" | \"script\""))
        val conf = decl("Config").asInstanceOf[TsDecl.Iface]
        assert(conf.members.exists {
            case p: TsMember.Property =>
                p.name == "nested" && p.optional && p.tpe == TsType.Arr(TsType.Named("Inner"))
            case _ => false
        })
        assert(decl("Inner").isInstanceOf[TsDecl.Iface]) // chased transitively
    }

    test("object with JSExport members becomes ConstObj; JSExportTopLevel def becomes Fun") {
        val tools = decl("Tools").asInstanceOf[TsDecl.ConstObj]
        assert(
          tools.members.collect { case m: TsMember.Method => m.name }.sorted ==
              List("concat", "twice")
        )
        val twice = decl("twice").asInstanceOf[TsDecl.Fun]
        assert(twice.overloads.head.doc.get.lines.head == "Doubles.")
        // a second @JSExportTopLevel name becomes a deprecated alias, it is not dropped
        assert(twice.deprecatedAliases == List("double"))
        assert(!result.module.decls.exists(_.name == "double"))
    }

    test("exported objects carry their fields and their js.Object members") {
        val tools = decl("Tools").asInstanceOf[TsDecl.ConstObj]
        assert(tools.members.exists {
            case p: TsMember.Property =>
                p.name == "version" && p.readonly && p.doc.map(_.lines) == Some(
                  List("The library version.")
                )
            case _ => false
        })
        // an exported js.Object singleton needs no @JSExport on each member
        val consts = decl("Consts").asInstanceOf[TsDecl.ConstObj]
        assert(consts.members.collect { case p: TsMember.Property => p.name } == List("answer"))
        assert(consts.members.collect { case m: TsMember.Method => m.name } == List("negate"))
    }

    test("inherited members are part of the subclass's exported API") {
        val c = decl("Circle").asInstanceOf[TsDecl.Cls]
        val names = c.members.collect {
            case p: TsMember.Property => p.name
            case m: TsMember.Method   => m.name
        }
        // own members first, then the base's, and no java.lang.Object / js.Object noise
        assert(names == List("radius", "sides", "kind", "describe"))
        val describe = c.members.collectFirst {
            case m: TsMember.Method if m.name == "describe" => m
        }.get
        assert(
          describe.overloads.head.doc.map(_.lines) == Some(List("A human-readable description."))
        )
        // an override is not emitted twice
        assert(names.count(_ == "sides") == 1)
    }

    test("a generic base contributes no members rather than unbound type names") {
        val b = decl("StringBox").asInstanceOf[TsDecl.Cls]
        // emitting Box's `value: A` here would reference an unbound A and break the whole file
        assert(b.typeParams.isEmpty)
        assert(!b.members.exists {
            case p: TsMember.Property => p.tpe == TsType.Named("A")
            case _                    => false
        })
    }

    test("TsIgnore members are omitted, no error") {
        val k = decl("Kitchen").asInstanceOf[TsDecl.Cls]
        assert(!k.members.exists {
            case m: TsMember.Method => m.name == "scalaOnly"
            case _                  => false
        })
        assert(!result.errors.exists(_.member.contains("scalaOnly")))
    }

    test("errors accumulate for Bad* fixtures with member context") {
        val msgs = result.errors.map(_.render)
        assert(msgs.exists(m => m.contains("BadLong") && m.contains("Long")))
        assert(msgs.exists(m => m.contains("BadOption") && m.contains("js.UndefOr")))
        assert(msgs.exists(m => m.contains("BadColl")))
        assert(msgs.exists(m => m.contains("BadOpaque")))
    }

    test("ownsPrecedingDoc accepts only blank lines, annotations and the definition head") {
        import ExportCollector.ownsPrecedingDoc
        assert(ownsPrecedingDoc(""))
        assert(ownsPrecedingDoc("\n    def "))
        assert(ownsPrecedingDoc("\n    val "))
        assert(ownsPrecedingDoc("\n@JSExportTopLevel(\"Point\")\nclass "))
        assert(ownsPrecedingDoc("\n@JSExport\n@JSExportTopLevel(\"twice\")\n    def "))
        assert(ownsPrecedingDoc("\n@JSExport def "))
        assert(ownsPrecedingDoc("\n    private def "))
        // a complete definition in between owns the comment instead
        assert(!ownsPrecedingDoc("\n@JSExport def a(): Unit = ()\n@JSExport def "))
        // meta-annotations: @(TsType @field)(...) is how a constructor-val's PROPERTY type is
        // narrowed, and it must not cost the member its doc comment
        assert(ownsPrecedingDoc("\n@(TsType @field)(\"\\\"a\\\" | \\\"b\\\"\")\n    val "))
        assert(ownsPrecedingDoc("\n@(TsType @field)(\"x\")\n@JSExport\n    def "))
        assert(ownsPrecedingDoc("\n@scalus.interop.TsIgnore\n    def "))

        assert(!ownsPrecedingDoc("\n    val width: Double,\n    val "))
        assert(!ownsPrecedingDoc("\nclass Probe")) // the class's own doc is not the ctor's
        assert(!ownsPrecedingDoc("\n  someCall()\n  def "))
    }

    test("duplicate top-level export names are reported") {
        assert(
          result.errors.exists(e =>
              e.member == "Duplicated" && e.message.contains("duplicate top-level")
          )
        )
    }

    test("empty or missing tasty root is an error, not an empty module") {
        val empty = ExportCollector.collect(
          List(InspectorFixture.sourceRoot + "/no-such-dir"),
          InspectorFixture.fixtureClasspath,
          InspectorFixture.sourceRoot
        )
        assert(empty.module.decls.isEmpty)
        assert(
          empty.errors.exists(e =>
              e.member == "<tasty-inspector>" && e.message.contains("no .tasty files found")
          )
        )
    }

    test("an excluded type is not chased into an interface") {
        val filtered = ExportCollector.collect(
          List(InspectorFixture.fixtureClasses),
          InspectorFixture.fixtureClasspath,
          InspectorFixture.sourceRoot,
          excludes = List("tsfixtures.Bad", "tsfixtures.Conf")
        )
        assert(!filtered.module.decls.exists(_.name == "Config"))
        // and the member that referenced it is reported, not silently pointed at a missing name
        assert(
          filtered.errors.exists(e =>
              e.member == "tsfixtures.Kitchen.config" && e.message.contains("excluded")
          )
        )
    }

    test("excludes drop declarations and errors") {
        val filtered = ExportCollector.collect(
          List(InspectorFixture.fixtureClasses),
          InspectorFixture.fixtureClasspath,
          InspectorFixture.sourceRoot,
          excludes = List("tsfixtures.Bad")
        )
        assert(filtered.errors.isEmpty)
        assert(!filtered.module.decls.exists(_.name.startsWith("Bad")))
        assert(filtered.module.decls.exists(_.name == "Point"))
    }
}
