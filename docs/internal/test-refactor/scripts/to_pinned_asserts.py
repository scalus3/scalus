#!/usr/bin/env python3
"""Rewrites legacy budget assertions of EvalTestKit to pinned `assertEvalEq` calls (D2).

Usage: to_pinned_asserts.py <file.scala> [<first-line> <last-line>]
       to_pinned_asserts.py --flip <file.scala>
       to_pinned_asserts.py --int-literals <file.scala>

--int-literals shortens BigInt literals where the type argument makes them safe:
  assertEvalEq(BigInt(1))(min(BigInt(1), BigInt(2)))  -> assertEvalEq[BigInt](1)(min(1, 2))
  The expected value must be a `BigInt(<int>)` literal. Inside the code, `BigInt(<int>)` changes
  only when the code is one plain call whose arguments are all literals. Receivers such as
  `BigInt(0).absolute`, and generic code such as `Cons(BigInt(1), Nil)` (it would become a
  List[Int]), keep their `BigInt(...)`. Pin keys change where the code text changes.

--flip rewrites the legacy order to expected first (D2):
  assertEvalEq(code, expected)       -> assertEvalEq(expected)(code)   or, when long,
                                        assertEvalEq(expected):
                                            code
  assertEvalEq(code, arg, expected)  -> assertEvalEq(expected, arg = arg)(code)
The pin keys do not change: they are the source text of `code`.

  assertEvalWithBudgets(code, [arg,] expected, Seq(compilerOptions -> ExUnits(...)))
  assertEvalWithBudget(code, [arg,] expected, ExUnits(...))
  assertEvalWithBudgetAndFee(code, [arg,] expected, ExUnits(...), Coin(...))
  assertEvalWithinBudget(code, expected, ExUnits(...))
      -> assertEvalEq(code, [arg,] expected)

The budget literals go away: the update run writes them into the suite's budget table. A call
whose `Seq` names other `Options` than `compilerOptions` is left alone and reported. Calls are found
with a bracket-matching scanner, not a regex, so nested calls, strings and comments are safe.
Prints the old literals to stdout as `<line>\t<literals>`, the oracle for the update run.
"""
import re
import sys

LEGACY = {
    "assertEvalWithBudgets": 1,  # number of trailing budget arguments to drop
    "assertEvalWithBudget": 1,
    "assertEvalWithinBudget": 1,
    "assertEvalWithBudgetAndFee": 2,
}
CALL = re.compile(r"\b(" + "|".join(sorted(LEGACY, key=len, reverse=True)) + r")\(")


def scan_args(text: str, open_paren: int) -> tuple[list[tuple[int, int]], int]:
    """Top-level argument spans of the call whose '(' is at `open_paren`, and the index of ')'."""
    depth, i, start, spans = 0, open_paren, open_paren + 1, []
    while i < len(text):
        c = text[i]
        if c == '"':
            if text.startswith('"""', i):
                i = text.index('"""', i + 3) + 3
                continue
            i += 1
            while text[i] != '"':
                i += 2 if text[i] == "\\" else 1
        elif text.startswith("//", i):
            i = text.index("\n", i)
            continue
        elif c in "([{":
            depth += 1
        elif c in ")]}":
            depth -= 1
            if depth == 0:
                spans.append((start, i))
                return spans, i
        elif c == "," and depth == 1:
            spans.append((start, i))
            start = i + 1
        i += 1
    raise ValueError("unbalanced call")


def main() -> None:
    path = sys.argv[1]
    first = int(sys.argv[2]) if len(sys.argv) > 2 else 1
    last = int(sys.argv[3]) if len(sys.argv) > 3 else 10**9
    text = open(path).read()
    out, pos = [], 0
    for m in CALL.finditer(text):
        if m.start() < pos:
            continue
        line = text.count("\n", 0, m.start()) + 1
        if not first <= line <= last:
            continue
        name = m.group(1)
        spans, close = scan_args(text, m.end() - 1)
        args = [text[a:b].strip() for a, b in spans]
        budget = args[-LEGACY[name]:]
        if name == "assertEvalWithBudgets" and not re.fullmatch(
            r"Seq\(\s*compilerOptions\s*->\s*ExUnits\([^()]*\)\s*,?\s*\)", budget[0]
        ):
            print(f"{line}\tSKIPPED (other Options): {budget[0]}", file=sys.stderr)
            continue
        kept = args[: -LEGACY[name]]
        indent = re.match(r"\s*", text[text.rfind("\n", 0, m.start()) + 1 :]).group(0)
        call = "assertEvalEq(\n" + ",\n".join(indent + "  " + a for a in kept) + "\n" + indent + ")"
        out.append(text[pos : m.start()] + call)
        pos = close + 1
        literals = " ".join(re.findall(r"ExUnits\([^()]*\)|Coin\([^()]*\)", " ".join(budget)))
        print(f"{line}\t{literals}")
    out.append(text[pos:])
    open(path, "w").write("".join(out))


FLIP = re.compile(r"\bassertEvalEq\(")


def flip(path: str) -> None:
    text = open(path).read()
    out, pos, n = [], 0, 0
    for m in FLIP.finditer(text):
        if m.start() < pos:
            continue
        spans, close = scan_args(text, m.end() - 1)
        if close + 1 < len(text) and text[close + 1] in "(:":
            continue  # already expected-first, or a label form
        args = [text[a:b].strip() for a, b in spans]
        line_start = text.rfind("\n", 0, m.start()) + 1
        indent = re.match(r"\s*", text[line_start:]).group(0)
        if len(args) == 2:
            code, expected, head = args[0], args[1], f"assertEvalEq({args[1]})"
        elif len(args) == 3:
            code, arg = args[0], args[1]
            # `arg` now comes before the lambda, so it fixes the argument type: `Cons(1, Nil)` would
            # become a List[Int]. Ascribe the lambda's declared parameter type to bare literals.
            declared = re.match(r"\(\s*\w+\s*:\s*(.+?)\)\s*=>", code)
            bare = re.search(r"(?<![\w.\"])-?\d+(?![\w.])", re.sub(r"BigInt\([^()]*\)|\"[^\"]*\"", "", arg))
            if declared and bare:
                arg = f"({arg}: {declared.group(1)})"
            head = f"assertEvalEq({args[2]}, arg = {arg})"
        else:
            continue
        one_line = f"{head}({code})"
        if "\n" not in one_line and len(indent) + len(one_line) <= 100:
            call = one_line
        else:
            # Re-indent the code under the colon: its lines move from (indent + 2) to (indent + 4).
            lines = code.split("\n")
            body = [indent + "    " + lines[0]] + [
                indent + "    " + (l[len(indent) + 2 :] if l.startswith(indent + "  ") else l.lstrip())
                for l in lines[1:]
            ]
            call = head + ":\n" + "\n".join(body)
        out.append(text[pos : m.start()] + call)
        pos = close + 1
        n += 1
    out.append(text[pos:])
    open(path, "w").write("".join(out))
    print(f"{path}: {n} calls flipped")


BIGINT_LIT = re.compile(r"BigInt\((-?\d+)\)")
SIMPLE_CALL = re.compile(
    r"[\w.]+\((?:\s*(?:\w+\s*=\s*)?(?:BigInt\(-?\d+\)|-?\d+)\s*,?)+\)"
)


def int_literals(path: str) -> None:
    text = open(path).read()
    out, pos, n = [], 0, 0
    for m in FLIP.finditer(text):
        if m.start() < pos:
            continue
        spans, close = scan_args(text, m.end() - 1)
        args = [text[a:b].strip() for a, b in spans]
        nxt = text[close + 1 : close + 2]
        exp = BIGINT_LIT.fullmatch(args[0]) if len(args) == 1 else None
        if not exp or nxt not in "(:":
            continue
        head = f"assertEvalEq[BigInt]({exp.group(1)})"
        if nxt == "(":
            code_spans, code_close = scan_args(text, close + 1)
            code = text[code_spans[0][0] : code_spans[0][1]].strip()
            if SIMPLE_CALL.fullmatch(code):
                code = BIGINT_LIT.sub(r"\1", code)
            out.append(text[pos : m.start()] + f"{head}({code})")
            pos = code_close + 1
        else:
            out.append(text[pos : m.start()] + head)
            pos = close + 1
        n += 1
    out.append(text[pos:])
    open(path, "w").write("".join(out))
    print(f"{path}: {n} calls shortened")


if __name__ == "__main__":
    if sys.argv[1] == "--int-literals":
        int_literals(sys.argv[2])
    elif sys.argv[1] == "--flip":
        flip(sys.argv[2])
    else:
        main()
