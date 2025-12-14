# Claude Code Guidelines for Scalus Project

## Project Overview

Scalus is a platform for developing decentralized applications (DApps) on the Cardano blockchain.
The goal is to make a full-stack development experience for Cardano DApps as smooth as possible.
Using the same language, tools and code for frontend, backend and smart contracts development.

Scalus compiles a subset of Scala 3 code to Scalus Intermediate Representation (SIR) and then lowers
in to Untyped Plutus Core (UPLC), the language of Cardano smart contracts.

## Quick Reference for Claude Code

**Essential Commands:**

- `sbtn quick` - Format, compile, and test on JVM (run before completing tasks when it makes sense)

**Key Patterns:**

- Always study existing code patterns before making changes
- Use `@Compile` annotation for Plutus smart contracts
- Extend `Validator` trait for validator scripts
- Place shared code in `shared/` directories for cross-platform support
- Use modern Scala 3 features: `given`, `using`, extension methods

## Key Principles for Claude Code

- Propose to run `sbtn quick` before considering any task complete** - This ensures formatting,
  compilation, and tests pass
- **Use existing patterns and conventions** - Study similar files before making changes
- **Prefer editing existing files over creating new ones** - Only create files when absolutely
  necessary
- **Follow Scala 3 idioms** - Use modern Scala 3 features like `given`, `using`, extension methods
- **Respect the multi-platform architecture** - Place shared code in `shared/` directories
- **Test thoroughly** - Use the existing test infrastructure and patterns
- **Follow Scalus Scala 3 formatting guideline** described on CONTRIBUTING.md
- **Consider context bloat** - Before invoking output-heavy commands, consider asking the user to run them for you and
  tell you the important output bits. Alternatively, when only the output code is important, redirect the standard
  output.

## Essential Commands

### Build and Development

```bash
# Format, compile on JVM, testQuick (fast iteration)
sbtn quick

# Clean compile and test everything (recommended before commits)
sbtn precommit

# Full CI build (includes formatting checks)
sbtn ci

# Format all code
sbtn scalafmtAll scalafmtSbt
```

### Compilation

```bash
# Compile all JVM projects
sbtn jvm/compile
sbtn jvm/Test/compile

# Compile specific cross-platform module
sbtn scalusJVM/compile
sbtn scalusCardanoLedgerJVM/compile

# Compile integration tests
sbtn scalusCardanoLedgerIt/Test/compile

# Clean and compile
sbtn cleanpile
```

### Testing

```bash
# Run all tests on JVM (most common)
sbtn jvm/test

# Run tests for specific cross-platform module (use JVM/JS suffix)
sbtn scalusJVM/test              # scalus-core JVM tests
sbtn scalusJS/test               # scalus-core JS tests
sbtn scalusExamplesJVM/test      # examples JVM tests
sbtn scalusTestkitJVM/test       # testkit JVM tests
sbtn scalusCardanoLedgerJVM/test # cardano-ledger JVM tests

# Run tests for JVM-only modules
sbtn scalusPlugin/test                      # compiler plugin tests
sbtn scalusPluginTests/test                 # plugin integration tests
sbtn scalus-bloxbean-cardano-client-lib/test # bloxbean integration tests

# Platform aggregate tests
sbtn jvm/test    # all JVM projects
sbtn js/test     # all JS projects
sbtn native/test # all Native projects

# Integration tests
sbtn it                          # clean and run integration tests
sbtn scalusCardanoLedgerIt/test  # cardano-ledger integration tests
```

### Documentation

```bash
# Generate documentation website
sbtn docs/mdoc
```

### Benchmarks

```bash
# Run all JMH benchmarks
sbtn benchmark

# Run JIT-specific benchmarks
sbtn benchmark-jit

# Run specific benchmark class
sbtn "bench/Jmh/run -i 1 -wi 1 -f 1 -t 1 .*CekBenchmark.*"
```

## Architecture Overview

### Module Structure

**Cross-platform modules** (JVM/JS, some with Native):
- **`scalus-core/`** (`scalusJVM`, `scalusJS`, `scalusNative`) - Core platform
    - Contains the Plutus VM implementation, UPLC evaluation, and standard library
    - Shared sources in `shared/` directory
    - Platform-specific implementations in `jvm/`, `js/`, `native/`

- **`scalus-cardano-ledger/`** (`scalusCardanoLedgerJVM`, `scalusCardanoLedgerJS`) - Cardano ledger
    - Low-level transaction building: `TransactionBuilder`, `LowLevelTxBuilder`
    - High-level API: `TxBuilder`

- **`scalus-testkit/`** (`scalusTestkitJVM`, `scalusTestkitJS`) - Testing utilities

- **`scalus-examples/`** (`scalusExamplesJVM`, `scalusExamplesJS`) - Smart contract examples

**JVM-only modules**:
- **`scalus-plugin/`** (`scalusPlugin`) - Scala 3 compiler plugin
    - Handles `@Compile` annotations and `Compiler.compile()` transformations
    - Compiles Scala code to Scalus Intermediate Representation (SIR)

- **`scalus-plugin-tests/`** (`scalusPluginTests`) - Plugin test suite

- **`scalus-uplc-jit-compiler/`** (`scalusUplcJitCompiler`) - Experimental JIT compiler for UPLC

- **`scalus-design-patterns/`** (`scalusDesignPatterns`) - Design pattern examples

- **`bloxbean-cardano-client-lib/`** (`scalus-bloxbean-cardano-client-lib`) - Bloxbean integration

- **`scalus-cardano-ledger-it/`** (`scalusCardanoLedgerIt`) - Integration tests

- **`bench/`** (`bench`) - JMH benchmarks

- **`scalus-docs/`** (`docs`) - Documentation (mdoc)

**Aggregate projects** (for convenience):
- `jvm` - All JVM projects
- `js` - All JS projects
- `native` - All Native projects

### Compilation Pipeline

1. **Scala Source** → **SIR** (Scalus Intermediate Representation) via compiler plugin
2. **SIR** → **UPLC** (Untyped Plutus Core) via SIR compiler
3. **UPLC** → **Plutus Script** (V1/V2/V3) for Cardano deployment

### Key Source Locations

- **SIR compiler**: `scalus-core/shared/src/main/scala/scalus/compiler/sir/`
- **SIR to UPLC lowering**: `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/`
- **UPLC implementation**: `scalus-core/shared/src/main/scala/scalus/uplc/`
- **Plutus VM (CEK machine)**: `scalus-core/shared/src/main/scala/scalus/uplc/eval/`
- **Compiler plugin**: `scalus-plugin/src/main/scala/scalus/plugin/`
- **Standard library (Prelude)**: `scalus-core/shared/src/main/scala/scalus/prelude/`
- **Transaction builder**: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/`
- **Examples**: `scalus-examples/shared/src/main/scala/scalus/examples/`

## Development Guidelines

### Smart Contract Development

- Use `@Compile` annotation on objects/methods to compile to Plutus Core
- Extend `Validator` trait for validator scripts
- Use `Compiler.compile()` to generate UPLC code
- Test contracts using the built-in Plutus VM or ScalaTest integration

### Scalus Plugin Development

- Shared sources are automatically copied from `scalus-core` to avoid code duplication
- Enable faster plugin development by uncommenting the dummy timestamp argument in `build.sbt`

### Multi-Platform Development

- Shared sources go in `shared/src/main/scala/`
- Platform-specific code goes in `jvm/`, `js/`, or `native/` directories
- Use `crossProject` for modules that need multi-platform support

### Testing

- Use ScalaTest for unit testing
- Use ScalaCheck for property-based testing
- Integration tests should use `Test/fork := true`
- Use `scalusTestkit` for contract testing utilities

## Important Files

- **`build.sbt`** - Main build configuration with all modules and dependencies
- **`project/plugins.sbt`** - SBT plugins for cross-compilation and tooling
- **`.scalafmt.conf`** - Code formatting configuration
- **`CONTRIBUTING.md`** - Detailed contribution guidelines
- **`README.md`** - Project overview and examples

## Useful Aliases

The build defines these command aliases:

**Development workflow:**
- `quick` - Format, compile JVM, run `testQuick` (fast iteration)
- `cleanpile` - Clean and compile JVM + integration tests
- `precommit` - Full clean, format, compile, test, docs (before commits)

**CI builds:**
- `ci` - Full CI: clean, format check, compile all platforms, test, docs, mima
- `ci-jvm` - JVM-only CI build
- `ci-js` - JS-only CI build
- `ci-native` - Native-only CI build

**Testing:**
- `it` - Clean and run integration tests (`scalusCardanoLedgerIt`)

**Benchmarks:**
- `benchmark` - Run all JMH benchmarks
- `benchmark-jit` - Run JIT-specific benchmarks

**Compatibility:**
- `mima` - Check binary compatibility for `scalus-bloxbean-cardano-client-lib`

## Environment Setup

For development, use Nix for reproducible builds:

```bash
nix develop
```

## Claude Code Specific Guidelines

### When Working with Smart Contracts

- Look at existing examples in `scalus-examples/shared/src/main/scala/scalus/examples/`
- Smart contracts should extend appropriate traits like `Validator`
- Use `@Compile` annotation for Plutus compilation
- Test contracts using `scalusTestkit` utilities

### When Working with the Compiler Plugin

- Plugin code is in `scalus-plugin/src/main/scala/scalus/plugin/`
- Shared sources are automatically copied from `scalus-core` - don't edit them directly
- Use debugging commands for plugin development
- Plugin tests are in `scalus-plugin-tests/`

### When Working with Core Platform

- Core logic is in `scalus-core/shared/src/main/scala/`
- Platform-specific implementations go in respective `jvm/`, `js/`, `native/` directories
- SIR compiler and lowering: `scalus/compiler/sir/`
- UPLC implementation: `scalus/uplc/`
- Plutus VM (CEK machine): `scalus/uplc/eval/`

### File Organization Patterns

- Follow the existing package structure: `scalus.{module}.{functionality}`
- Tests mirror source structure but in `test/` instead of `main/`
- Use object-oriented design with case classes for data structures
- Prefer sealed traits for sum types

### Common Tasks

- **Adding a new validator**: Create in `scalus-examples`, extend `Validator`, add tests
- **Modifying UPLC evaluation**: Edit files in `scalus-core/shared/src/main/scala/scalus/uplc/eval/`
- **Adding standard library functions**: Add to `scalus-core/shared/src/main/scala/scalus/prelude/`
- **Plugin modifications**: Work in `scalus-plugin/`, test with `scalus-plugin-tests/`
- **Transaction builder changes**: Edit files in `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/txbuilder/`
- **Running benchmarks**: Use `sbtn benchmark` or `sbtn "bench/Jmh/run ..."`

### Before Submitting Changes

When developing, use `sbtn quick` to ensure everything is formatted, compiles, and tests pass.
If `quick` fails due to class-related issues, try running `sbtn clean` first.

Run `sbtn precommit` before considering work complete.

### Commit Messages

- Use clear, descriptive commit messages
- Follow conventional commit style
- Never add yourself to the commit messages, no "Co-authored by Claude Code" or similar
- Suggest a commit message when you complete a task.
- Commit messages should be short: 1 or 2 paragraphs

### Libraries (in tests)
- Always use Scalus data types if it is appropriate and has meaning;
- For JSON parsing use json-iter;
- Always use com.lihaoyi.pprint to show result of complex data type

### Reference projects
- Cardano Ledger https://github.com/IntersectMBO/cardano-ledger
- Plutus https://github.com/IntersectMBO/plutus
- Amaru https://github.com/pragma-org/amaru

These projects are references of data model. They could be obtained in sibling directories (using ../) or directly on GitHub.
