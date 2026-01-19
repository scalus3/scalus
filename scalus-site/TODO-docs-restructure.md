# Documentation Restructure Plan

## Current State (55 pages, 10 sections)

```
├── Get Started
├── Onboarding (for-cardano-devs, for-scala-devs)
├── Language Guide
├── Smart Contracts (+ Optimizations subsection)
├── Transaction Builder
├── Off-chain Applications (1 page only)
├── Ledger Framework (Emulator, Yaci, Protocol Params - some unpublished)
├── Learning (Design Patterns, Vulnerabilities)
├── Catalyst
└── API Reference (external)
```

## Research: Competitor Documentation Structures

| Framework | Philosophy | Key Insight |
|-----------|-----------|-------------|
| **Aiken** | Fundamentals → Language → Examples → Advanced | Design Patterns in Fundamentals (top-level!) |
| **Anchor** | Getting Started → Core Concepts → References | Testing Libraries as dedicated section |
| **MeshJS** | SDK → Providers → Utilities | Multiple provider integrations |
| **Solidity** | Basics → Language → Compiler → Internals | Security/Advisory as separate section |

## Proposed Structure (Development Workflow)

```
├── Get Started                    # Quick start, installation
├── Concepts                       # Architecture, EUTxO, compilation pipeline
│
├── Language Guide                 # Scala 3, supported features, syntax
│   └── ...existing pages...
│
├── Smart Contracts                # Writing validators
│   ├── Your First Contract
│   ├── Validators
│   ├── Data Conversion
│   ├── Compiling
│   └── Optimizations/
│
├── Transactions                   # RENAMED from Transaction Builder
│   ├── Building Transactions
│   ├── Spending UTxOs
│   ├── Minting & Burning
│   ├── Staking & Governance
│   └── Advanced Features
│
├── Testing                        # NEW - promoted from Ledger
│   ├── Unit Testing
│   ├── Emulator
│   ├── Yaci DevKit
│   └── Debugging
│
├── Providers                      # NEW - integrations
│   ├── Protocol Parameters
│   ├── Blockfrost (if applicable)
│   └── Custom Providers
│
├── Design Patterns                # PROMOTED to top level
│   ├── Stake Validator
│   ├── Transaction Level Minter
│   ├── Merkelized Validator
│   ├── UTxO Indexer
│   ├── Linked List
│   └── Validity Range
│
├── Security                       # NEW - extracted from Learning
│   ├── Plutus Vulnerabilities
│   ├── Common Pitfalls
│   └── Auditing Checklist
│
├── Examples                       # NEW - end-to-end walkthroughs
│   ├── Hello World (end-to-end)
│   ├── NFT Minting
│   └── ...
│
├── Comparisons                    # NEW - high SEO value
│   ├── Scalus vs Aiken
│   └── Scalus vs Plutus/PlutusTx
│
├── FAQ                            # NEW - enables rich snippets
│
└── API Reference (external link)

Footer: Catalyst Projects, GitHub, Discord
```

## Key Changes Summary

| Change | Rationale |
|--------|-----------|
| Rename "Transaction Builder" → "Transactions" | Shorter, more search-friendly |
| Promote Design Patterns to top level | High SEO value, shows maturity (Aiken does this) |
| Create "Testing" section | Workflow-oriented, Anchor pattern |
| Create "Providers" section | Separate infrastructure concerns |
| Create "Security" section | Solidity pattern, high value |
| Create "Examples" section | End-to-end walkthroughs (Aiken pattern) |
| Create "Comparisons" section | High-intent SEO traffic |
| Create "FAQ" page | Rich snippets, GEO optimization |
| Move Catalyst to footer | Not core docs content |

## Open Questions to Decide

1. **Testing section** - Should "Debugging" stay with Smart Contracts or move to Testing?

2. **Providers vs Integrations vs Infrastructure** - Which naming?

3. **Examples section** - Separate section, or embed in each topic?

4. **Off-chain content** - Where does it fit? Transactions? Separate? Examples?

5. **Onboarding paths** - Keep separate section or merge into Get Started?

6. **Design Patterns naming** - "Design Patterns" vs "Patterns" vs "Advanced Patterns"?

## SEO Improvements (Technical)

- [ ] Add missing index.mdx files (ledger/, offchain-applications/)
- [ ] Fill empty _meta.js titles with SEO-friendly names
- [ ] Rename unclear URLs (from-data → data-conversion)
- [ ] Add breadcrumb structured data (JSON-LD)
- [ ] Add HowTo schema to tutorial pages
- [ ] Add FAQ schema to FAQ page
- [ ] Audit all meta descriptions (150-160 chars)
- [ ] Verify Core Web Vitals scores
- [ ] Optimize content for GEO (AI search - clear definitions, quotable answers)

## References

- [Aiken Docs](https://aiken-lang.org/fundamentals/getting-started)
- [Anchor Docs](https://www.anchor-lang.com/docs)
- [MeshJS Docs](https://meshjs.dev/)
- [Solidity Docs](https://docs.soliditylang.org/)
- [Google SEO Guide](https://developers.google.com/search/docs/fundamentals/seo-starter-guide)
- [Technical SEO Guide 2026](https://backlinko.com/technical-seo-guide)
