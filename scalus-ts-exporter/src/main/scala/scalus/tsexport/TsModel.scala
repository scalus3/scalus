package scalus.tsexport

/** TypeScript type expressions, rendered by [[Emitter.render]]. */
enum TsType {
    case Named(name: String) // "number", "Uint8Array", "EvaluationResult"
    case Union(members: List[TsType])
    case Intersect(members: List[TsType])
    case Arr(elem: TsType)
    case Func(params: List[TsParam], ret: TsType)
    case Index(value: TsType) // { [key: string]: V }
    case Generic(name: String, args: List[TsType]) // Promise<string>
    case Verbatim(text: String) // @TsType override, emitted as-is
}

case class TsParam(name: String, tpe: TsType, optional: Boolean)

/** A declaration's type parameter; `bound` renders as `A extends <bound>`. */
case class TsTypeParam(name: String, bound: Option[TsType])

/** TSDoc body lines, already converted, without the comment frame. */
case class TsDoc(lines: List[String])

/** One call signature of a method or function, with the doc block that precedes it.
  *
  * TypeScript allows a doc comment per overload signature, so each carries its own.
  */
case class TsOverload(
    typeParams: List[TsTypeParam],
    params: List[TsParam],
    ret: TsType,
    doc: Option[TsDoc]
)

enum TsMember {
    case Ctor(overloads: List[List[TsParam]], doc: Option[TsDoc])
    case Method(name: String, overloads: List[TsOverload], static: Boolean)
    case Property(
        name: String,
        tpe: TsType,
        readonly: Boolean,
        optional: Boolean,
        static: Boolean,
        doc: Option[TsDoc]
    )
}

enum TsDecl {

    /** deprecatedAliases: extra top-level export names, emitted as deprecated alias exports. */
    case Cls(
        name: String,
        typeParams: List[TsTypeParam],
        members: List[TsMember],
        doc: Option[TsDoc],
        deprecatedAliases: List[String]
    )

    /** @param inputOnly
      *   this interface is only ever reached from parameter positions, so its array properties
      *   render as `readonly T[]` - callers may hand us a ReadonlyArray
      */
    case Iface(
        name: String,
        typeParams: List[TsTypeParam],
        members: List[TsMember],
        doc: Option[TsDoc],
        inputOnly: Boolean
    )
    case Fun(name: String, overloads: List[TsOverload], deprecatedAliases: List[String])
    case ConstObj(name: String, members: List[TsMember], doc: Option[TsDoc])
}

object TsDecl {
    extension (d: TsDecl)
        def name: String = d match
            case c: Cls      => c.name
            case i: Iface    => i.name
            case f: Fun      => f.name
            case o: ConstObj => o.name
}

case class TsModule(decls: List[TsDecl])

object TsModule {

    /** Applies `f` to every doc block in the module, wherever one can appear. */
    def mapDocs(module: TsModule, f: TsDoc => TsDoc): TsModule = {
        def doc(d: Option[TsDoc]): Option[TsDoc] = d.map(f)
        def overload(o: TsOverload): TsOverload = o.copy(doc = doc(o.doc))
        def member(m: TsMember): TsMember = m match
            case c: TsMember.Ctor     => c.copy(doc = doc(c.doc))
            case m: TsMember.Method   => m.copy(overloads = m.overloads.map(overload))
            case p: TsMember.Property => p.copy(doc = doc(p.doc))
        TsModule(module.decls.map {
            case d: TsDecl.Cls      => d.copy(members = d.members.map(member), doc = doc(d.doc))
            case d: TsDecl.Iface    => d.copy(members = d.members.map(member), doc = doc(d.doc))
            case d: TsDecl.Fun      => d.copy(overloads = d.overloads.map(overload))
            case d: TsDecl.ConstObj => d.copy(members = d.members.map(member), doc = doc(d.doc))
        })
    }
}

case class ExportError(member: String, message: String) {
    def render: String = s"$member: $message"
}
