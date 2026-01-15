---
description: Knowledge for writing Pretty typeclass instances using paiges Doc DSL
globs: scalus-core/**/Pretty.scala, scalus-core/**/ledger/*.scala
---

# Pretty Printer Writing Guide

This skill provides knowledge for implementing `Pretty[A]` typeclass instances in the Scalus
codebase.

## Key Files

- `scalus-core/shared/src/main/scala/scalus/utils/Pretty.scala` - typeclass definition and helpers
- `scalus-core/shared/src/main/scala/scalus/cardano/ledger/*.scala` - example instances

## Pretty Typeclass Overview

The `Pretty[A]` typeclass converts values to `org.typelevel.paiges.Doc` for flexible pretty
printing.

### Key Methods

- `pretty(a: A, style: Style): Doc` - concise format
- `prettyDetailed(a: A, style: Style): Doc` - detailed format (optional override)

### Creating Instances

```scala
// Simple instance
given Pretty[MyType] with
    def pretty(a: MyType, style: Style): Doc = ...

// Instance with detailed view
given Pretty[MyType] = Pretty.instanceWithDetailed(
  concise = (a, style) => ...,
  detailed = (a, style) => ...
)
```

## Doc DSL Quick Reference

### Basic Combinators

- `text("str")` - literal text
- `str(value)` - convert to string then Doc
- `char('x')` - single character
- `empty` - empty Doc
- `space` - single space
- `comma` - comma character
- `line` - line break (or space when flattened)

### Composition Operators

- `a + b` - concatenate Docs directly (no space)
- `a & b` - concatenate Docs with space
- `a / b` - concatenate Docs with line break

### Layout Control

- `.nested(n)` - indent by n after line breaks
- `.indent(n)` - indent the whole thing
- `.hang(n)` - hanging indent (first line not indented)
- `.grouped` - try to fit on one line, break if needed
- `.flatten` - force single line

### Lists and Brackets

- `fill(sep, docs)` - fill paragraphs with flexible breaks
- `stack(docs)` - vertical stack (join with line breaks)
- `intercalate(sep, docs)` - join with separator
- `.tightBracketBy(open, close)` - wrap with brackets
- `.bracketBy(open, close)` - wrap with brackets and indent

## Pretty Helpers (from `scalus.utils.Pretty`)

### Bracket Helpers

- `Pretty.inParens(d)` - wrap in `(d)`
- `Pretty.inBraces(d)` - wrap in `{d}`
- `Pretty.inBrackets(d)` - wrap in `[d]`
- `Pretty.braceList(entries)` - `{ a, b, c }` format

### Field Helpers

- `Pretty.field(label, value, style)` - `label: value` with styled label
- `Pretty.optField(label, opt, style)` - optional field
- `Pretty.bulletList(label, items)` - bulleted list

### Styling (XTerm colors)

- `Pretty.kw(s, style)` - keyword (orange) - use for labels
- `Pretty.ctr(s, style)` - constructor (blue) - use for type names
- `Pretty.lit(d, style)` - literal (green) - use for values
- `Pretty.typ(d, style)` - type (purple)
- `Pretty.err(d, style)` - error (red)

### Rainbow Brackets (16 colors)

For nested structures, use rainbow brackets to color brackets based on nesting depth:

- `Pretty.rainbowChar(c, depth, style)` - single bracket char with rainbow color
- `Pretty.rainbowBracket(d, open, close, depth, style)` - wrap content in rainbow brackets

Colors cycle through 16 distinct XTerm colors: red → orange → yellow → lime → green → teal → cyan →
blue → indigo → purple → magenta → pink. Colors are chosen to avoid collision with `ctr`, `lit`, and
`kw` styling.

## Implementation Guidelines

When writing a `Pretty` instance:

1. **Read the type definition** to understand its structure

2. **Choose the right pattern:**
    - Enums/sealed traits: pattern match and show variant-specific content
    - Collections: use `fill` or `stack` for items
    - Wrappers: delegate to inner type's Pretty instance

3. **Apply styling consistently:**
    - `ctr` for constructor/type names
    - `lit` for literal values (numbers, booleans)
    - `kw` for field labels

4. Prefer using Composition Operators: &, /, + for clarity

5. **Use layout control:**
    - `.grouped` for content that should try to fit on one line
    - `fill(comma + space, items)` for lists that can wrap
    - `.hang(2)` or `.indent(2)` for nested content

6. **Add to companion object:**
   ```scala
   object MyType:
       import Doc.*
       import Pretty.{ctr, inParens, lit}

       given Pretty[MyType] with
           def pretty(a: MyType, style: Style): Doc = ...
   ```

7. **Delegate to other instances:**
   ```scala
   Pretty[OtherType].pretty(value, style)
   ```

8. **Test output:**
    - `.show` - render at 80 columns
    - `.showDetailed` - render detailed at 80 columns
    - `.showHighlighted` - render with XTerm colors
    - `.showDetailedHighlighted` - render detailed with XTerm colors

## Common Patterns

### Enum/Sealed Trait

```scala
given Pretty[MyEnum] with
    def pretty(a: MyEnum, style: Style): Doc = a match
        case MyEnum.VariantA(x) => ctr("VariantA", style) + inParens(lit(str(x), style))
        case MyEnum.VariantB(items) =>
            val inner = fill(comma + space, items.map(Pretty[Item].pretty(_, style)))
            (ctr("VariantB", style) + inParens(inner)).grouped
```

### Case Class with Fields

```scala
given Pretty[MyCaseClass] with
    def pretty(a: MyCaseClass, style: Style): Doc =
        val fields = List(
            text("name:" & text(a.name)),
            text("value:" & lit(str(a.value))
        )
        (ctr("MyCaseClass", style) / stack(fields).indent(2)).grouped
```

### Concise + Detailed Views

```scala
given Pretty[MyType] = Pretty.instanceWithDetailed(
  concise = (a, style) => text(a.hash.toHex),
  detailed = (a, style) => detailedInfo
)
```

### Recursive Types with Rainbow Brackets

For nested/recursive structures, track depth and use rainbow brackets:

```scala
given Pretty[MyTree] with
    def pretty(a: MyTree, style: Style): Doc = prettyWithDepth(a, style, 0)

    private def prettyWithDepth(a: MyTree, style: Style, depth: Int): Doc = a match
        case Leaf(value) => lit(str(value), style)
        case Branch(left, right) =>
            val inner = prettyWithDepth(left, style, depth + 1) + comma + space +
                        prettyWithDepth(right, style, depth + 1)
            rainbowBracket(inner, '(', ')', depth, style)
```

### Guidelines

- Never shorten hashes, always print full hash. No `hash.take(16) + "..."`.