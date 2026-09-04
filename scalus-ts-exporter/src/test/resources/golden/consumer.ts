// Type-level consumer of the golden declarations; compiled by tsc --noEmit in GoldenTest.
import {
  Point,
  NewName,
  OldName,
  Statics,
  Box,
  Kitchen,
  Tools,
  twice,
  Config,
  Inner,
  Generics,
  GenPair,
  BoundedBox,
  Renames,
  RenamesAll,
  Rect,
  OneLiners,
  Consts,
  double,
  Circle,
  StringBox,
  Documented,
  Intersections,
  Boom,
  BoomType,
} from "./fixtures";

const p: Point = new Point(1, 2);
const d: number = p.dist(p);
const n: NewName = new NewName(1);
const o: OldName = new OldName(2);
const s: Statics = Statics.make(1);
const s2: Statics = Statics.make(1, "tag");
const m: Statics = Statics.mainnet;
const b: Box<string> = new Box("x");
const k = new Kitchen();
const u: number | undefined = k.undef("a");
const u2: number | undefined = k.undef();
const big: bigint = k.big(1n);
const un: Uint8Array | null = k.union(null);
const ct: "key" | "script" = k.credType();
const over1: number = k.overloaded(1);
const over2: string = k.overloaded(1, "b");
const cfg: Config = { flag: true };
const cfg2: Config = { flag: true, nested: [{ id: "a" }] };
const t: number = twice(2);
const c: string = Tools.concat("a", undefined);
const c2: string = Tools.concat("a");

const g = new Generics();
const gb: Box<string> = g.getBox();
const gbs: Box<number[]>[] = g.boxes([new Box("x")]);
const gp: GenPair<number, Box<string>> = g.pair({ first: "a", second: 1 });
const gpick: string = g.pick("a", "b");
const gwiden: Box<Kitchen> = g.widen(k);
const bb: BoundedBox<Kitchen> = new BoundedBox(k);
// @ts-expect-error a bounded type parameter rejects a primitive argument
const bbBad: BoundedBox<number> = new BoundedBox(1);

const r = new Renames();
const rev: string = r.evaluate("00");
const rev2: string = r.evaluate("00", 1);
const rrn: number = r.run(1);
const rrs: string = r.run("a");
const rb: number = r.both();
const ra: number = r.aliased();
const rv: string = r.ver;
const rall: number = new RenamesAll().extra();
const sof: Statics = Statics.of(1);

const rect = new Rect(1, 2);
const rw: number = rect.width;
const ol: number = new OneLiners().a();

const tv: string = Tools.version;
const ca: number = Consts.answer;
const cn: number = Consts.negate(1);
const dbl: number = double(2);
const circle = new Circle(1);
const ck: string = circle.kind;
const cd: string = circle.describe();
const cs: number = circle.sides();
const sb: StringBox = new StringBox();
const dp: string = new Documented().pick("a");
const both: Config & Inner = new Intersections().both({ flag: true, id: "a" });

// a js.Error subclass inherits the platform's members through `extends Error`
const boom = new Boom("boom", "why");
const boomMessage: string = boom.message;
const boomDetail: string = boom.detail;
const boomIsError: boolean = boom instanceof Error;
const boomStack: string | undefined = boom.stack;
const boomType: TypeError = new BoomType("nope");

// keep the values "used" for --noUnusedLocals-style strictness
export {
  p, d, n, o, s, s2, m, b, u, u2, big, un, ct, over1, over2, cfg, cfg2, t, c, c2,
  g, gb, gbs, gp, gpick, gwiden, bb, bbBad,
  r, rev, rev2, rrn, rrs, rb, ra, rv, rall, sof,
  rect, rw, ol, tv, ca, cn, dbl, circle, ck, cd, cs, sb, dp, both,
  boom, boomMessage, boomDetail, boomIsError, boomStack, boomType,
};
