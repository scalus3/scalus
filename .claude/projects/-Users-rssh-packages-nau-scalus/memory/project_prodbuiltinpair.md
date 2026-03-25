---
name: ProdBuiltinPair refactoring
description: Next planned refactoring — parameterize PairData into ProdBuiltinPair(fstRepr, sndRepr) to avoid unnecessary Data conversion for pairs
type: project
---

Replace `ProductCaseClassRepresentation.PairData` (case object) with `ProdBuiltinPair(fstRepr, sndRepr)` (case class).

**Why:** Currently all pair components are forced through Data representation (`mkPairData` requires Data args). With parameterized pair repr, components can stay in their native representation (e.g., `Constant` for Integer) and only convert to Data when actually needed.

**How to apply:**
- `PairData` becomes a val alias for `ProdBuiltinPair(defaultDataRepresentation(fstType), defaultDataRepresentation(sndType))` — needs type context
- Follows the same pattern as `SumDataList` → `SumBuiltinList(elementRepr)` refactoring
- Will affect `SumPairBuiltinListSirTypeGenerator`, `MapSirTypeGenerator`, `ProductCaseSirTypeGenerator`, and anywhere `PairData` is referenced
- `mkPairData` builtin still requires Data args — but `ProdBuiltinPair` with non-Data reprs would use typed pair construction instead
