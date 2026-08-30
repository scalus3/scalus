#!/usr/bin/env python3
"""Regenerate scalus.uplc.eval.VanRossemNewBuiltinCosts from the vendored Plutus cost models.

Run this when builtinCostModelD.json or builtinCostModelE.json changes, which
VanRossemNewBuiltinCostsTest will tell you about by failing:

    python3 scripts/gen-vanrossem-costs.py
    sbt scalafmtAll        # the generator emits long lines; scalafmt wraps them

The second step is not optional: the committed file is the formatted output, so skipping it
leaves the tree failing scalafmtCheckAll.

The object holds the costs of the fourteen builtins that the van Rossem (PV11) semantics
variants add, which MachineParams.fromCostModels substitutes when a pre-van-Rossem cost model
is in force. They are literals rather than a parse of the JSON because BuiltinCostModel's
reader is a upickle one, and upickle is deliberately not in the Scala.js bundle. See
docs/internal/JS_BUNDLE_SIZE.md.

Do not hand-edit the generated file. The JSON "type" tags map onto the costing constructors in
non-obvious ways: with_interaction_in_x_and_y takes its arguments as c00, c10, c01, c11 while
the JSON lists them c00, c01, c10, c11, and const_above_diagonal nests a whole second model.
"""

import io
import json
import os

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RESOURCES = os.path.join(ROOT, "scalus-core/shared/src/main/resources")
D = json.load(open(os.path.join(RESOURCES, "builtinCostModelD.json")))
E = json.load(open(os.path.join(RESOURCES, "builtinCostModelE.json")))

# field -> (wrapper, arity type). wrapper None means DefaultCostingFun.
FIELDS = [
    ("expModInteger", "ExpModIntegerCostingFun", "ThreeArguments"),
    ("dropList", "DropListCostingFun", "TwoArguments"),
    ("lengthOfArray", None, "OneArgument"),
    ("listToArray", None, "OneArgument"),
    ("indexArray", None, "TwoArguments"),
    ("bls12_381_G1_multiScalarMul", None, "TwoArguments"),
    ("bls12_381_G2_multiScalarMul", None, "TwoArguments"),
    ("insertCoin", None, "FourArguments"),
    ("lookupCoin", None, "ThreeArguments"),
    ("unionValue", None, "TwoArguments"),
    ("valueContains", None, "TwoArguments"),
    ("valueData", None, "OneArgument"),
    ("unValueData", None, "OneArgument"),
    ("scaleValue", None, "TwoArguments"),
]

def ci(n):
    return f"CostingInteger({int(n)}L)"

def model(entry, arity):
    t = entry["type"]
    a = entry["arguments"]
    if t == "constant_cost":
        return f"{arity}.ConstantCost({ci(a)})"
    if t == "linear_in_x":
        return f"{arity}.LinearInX(OneVariableLinearFunction({ci(a['intercept'])}, {ci(a['slope'])}))"
    if t == "linear_in_y":
        return f"{arity}.LinearInY(OneVariableLinearFunction({ci(a['intercept'])}, {ci(a['slope'])}))"
    if t == "linear_in_z":
        return f"{arity}.LinearInZ(OneVariableLinearFunction({ci(a['intercept'])}, {ci(a['slope'])}))"
    if t == "linear_in_u":
        return f"{arity}.LinearInU(OneVariableLinearFunction({ci(a['intercept'])}, {ci(a['slope'])}))"
    if t == "added_sizes":
        return f"{arity}.AddedSizes(OneVariableLinearFunction({ci(a['intercept'])}, {ci(a['slope'])}))"
    if t == "linear_in_x_and_y":
        return (f"{arity}.LinearInXAndY(TwoVariableLinearFunction("
                f"{ci(a['intercept'])}, {ci(a['slope1'])}, {ci(a['slope2'])}))")
    if t == "quadratic_in_x":
        return (f"{arity}.QuadraticInX(OneVariableQuadraticFunction("
                f"{ci(a['c0'])}, {ci(a['c1'])}, {ci(a['c2'])}))")
    if t == "with_interaction_in_x_and_y":
        # constructor order is c00, c10, c01, c11
        return (f"{arity}.WithInteractionInXAndY(TwoVariableWithInteractionFunction("
                f"{ci(a['c00'])}, {ci(a['c10'])}, {ci(a['c01'])}, {ci(a['c11'])}))")
    if t == "const_above_diagonal":
        inner = model(a["model"], arity)
        return f"{arity}.ConstAboveDiagonal(ConstantOrTwoArguments({ci(a['constant'])}, {inner}))"
    if t == "exp_mod_cost":
        return (f"{arity}.ExpModCost(ExpModCostingFunction("
                f"{ci(a['coefficient00'])}, {ci(a['coefficient11'])}, {ci(a['coefficient12'])}))")
    raise SystemExit("unhandled costing type: " + t)

lines = []
for name, wrapper, arity in FIELDS:
    assert D[name] == E[name], f"D and E differ for {name}"
    entry = E[name]
    cpu = model(entry["cpu"], arity)
    mem = model(entry["memory"], arity)
    ctor = wrapper if wrapper else "DefaultCostingFun"
    lines.append(f"    val {name}: {ctor}{'' if wrapper else f'[{arity}]'} = {ctor}(\n"
                 f"      cpu = {cpu},\n"
                 f"      memory = {mem}\n"
                 f"    )")

body = "\n\n".join(lines)

out = f'''package scalus.uplc.eval

/** Costs for the builtins that the van Rossem (PV11) semantics variants D and E add, taken from
  * the vendored Plutus reference models `builtinCostModelD.json` and `builtinCostModelE.json`.
  *
  * A cost model set by governance before van Rossem has no entries for these builtins, so
  * reading them yields the `300_000_000` placeholder that `PlutusParams.fromSeq` fills in for
  * absent parameters, which would make the new builtins absurdly expensive.
  * [[MachineParams.fromCostModels]] therefore substitutes these values, and only these, leaving
  * every builtin that the supplied model does cover on its governance-set cost.
  *
  * The values are literals rather than a parse of the JSON resources at runtime because
  * `BuiltinCostModel.fromJsonString` is a upickle reader, and `Data.fromJson` aside it was the
  * last thing keeping upickle, ujson and upack in the published `scalus.js` bundle. Variants D
  * and E carry identical values for all fourteen builtins, so one set covers both.
  *
  * Generated by `scripts/gen-vanrossem-costs.py`; do not hand-edit, and re-run that script (then
  * `scalafmtAll`) when the vendored resources change. `VanRossemNewBuiltinCostsTest` compares
  * every field with `BuiltinCostModel.vanRossemReferenceD` and `...E`, so a resource change
  * fails the build rather than silently mispricing a builtin. See
  * `docs/internal/JS_BUNDLE_SIZE.md`.
  */
private[scalus] object VanRossemNewBuiltinCosts {{

{body}
}}
'''

path = os.path.join(ROOT, "scalus-core/shared/src/main/scala/scalus/uplc/eval/VanRossemNewBuiltinCosts.scala")
io.open(path, "w", encoding="utf-8").write(out)
print("wrote", path, f"({len(out)} bytes, {len(FIELDS)} fields)")
