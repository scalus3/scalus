#!/bin/bash

# Script to analyze JIT compiler bytecode and assembly generation
# Usage: ./analyze_jit_bytecode.sh [test_type]
# test_type: basic | inlining | assembly | classes | all (default: all)

set -e

PROJECT_ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$PROJECT_ROOT"

OUTPUT_DIR="jit-analysis-output"
mkdir -p "$OUTPUT_DIR"

TEST_CLASS="scalus.uplc.eval.AnalyzeJITBytecode"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

run_basic() {
    echo -e "${GREEN}=== Running Basic JIT Test ===${NC}"
    sbt "scalusUplcJitCompiler/Test/runMain $TEST_CLASS" | tee "$OUTPUT_DIR/basic_output.txt"
}

run_with_compilation_log() {
    echo -e "${GREEN}=== Running with Compilation Logging ===${NC}"
    sbt "set scalusUplcJitCompiler/Test/javaOptions ++= Seq(\"-XX:+PrintCompilation\", \"-XX:+UnlockDiagnosticVMOptions\"); scalusUplcJitCompiler/Test/runMain $TEST_CLASS" | tee "$OUTPUT_DIR/compilation_log.txt"
}

run_with_inlining_log() {
    echo -e "${GREEN}=== Running with Inlining Analysis ===${NC}"
    sbt "set scalusUplcJitCompiler/Test/javaOptions ++= Seq(\"-XX:+UnlockDiagnosticVMOptions\", \"-XX:+PrintInlining\", \"-XX:+PrintCompilation\"); scalusUplcJitCompiler/Test/runMain $TEST_CLASS" | tee "$OUTPUT_DIR/inlining_log.txt"

    echo -e "\n${YELLOW}Analyzing inlining decisions...${NC}"
    echo "=== Inlined methods ===" > "$OUTPUT_DIR/inlining_summary.txt"
    grep "@ " "$OUTPUT_DIR/inlining_log.txt" | grep -v "never executed" | head -50 >> "$OUTPUT_DIR/inlining_summary.txt" || true

    echo -e "\n=== Failed to inline ===" >> "$OUTPUT_DIR/inlining_summary.txt"
    grep "failed" "$OUTPUT_DIR/inlining_log.txt" | head -30 >> "$OUTPUT_DIR/inlining_summary.txt" || true

    cat "$OUTPUT_DIR/inlining_summary.txt"
}

run_with_tiered_compilation_disabled() {
    echo -e "${GREEN}=== Running with C2 Only (No Tiered Compilation) ===${NC}"
    sbt "set scalusUplcJitCompiler/Test/javaOptions ++= Seq(\"-XX:-TieredCompilation\", \"-XX:+PrintCompilation\"); scalusUplcJitCompiler/Test/runMain $TEST_CLASS" | tee "$OUTPUT_DIR/c2_only.txt"
}

run_with_assembly() {
    echo -e "${GREEN}=== Attempting to Print Assembly (requires hsdis plugin) ===${NC}"
    echo -e "${YELLOW}Note: This requires hsdis-amd64.dylib (on macOS) or hsdis-amd64.so (on Linux)${NC}"
    echo -e "${YELLOW}Download from: https://chriswhocodes.com/hsdis/${NC}"

    sbt "set scalusUplcJitCompiler/Test/javaOptions ++= Seq(\"-XX:+UnlockDiagnosticVMOptions\", \"-XX:+PrintAssembly\", \"-XX:PrintAssemblyOptions=intel\", \"-XX:CompileCommand=print,scalus/uplc/eval/ContinuationJitRepr\\\$.eval\", \"-XX:CompileCommand=print,*Lambda*apply\"); scalusUplcJitCompiler/Test/runMain $TEST_CLASS" 2>&1 | tee "$OUTPUT_DIR/assembly.txt" || {
        echo -e "${YELLOW}Assembly printing failed - hsdis plugin likely not installed${NC}"
    }
}

dump_staged_classes() {
    echo -e "${GREEN}=== Dumping Staged/Generated Classes ===${NC}"

    DUMP_DIR="$OUTPUT_DIR/staged-classes"
    mkdir -p "$DUMP_DIR"

    sbt "set scalusUplcJitCompiler/Test/javaOptions ++= Seq(\"-Dscala.quoted.dumpClassesTo=$DUMP_DIR\"); scalusUplcJitCompiler/Test/runMain $TEST_CLASS" | tee "$OUTPUT_DIR/class_dump.txt"

    echo -e "\n${YELLOW}Generated classes:${NC}"
    find "$DUMP_DIR" -name "*.class" 2>/dev/null || echo "No classes found"

    # Disassemble the generated classes
    if [ -d "$DUMP_DIR" ]; then
        echo -e "\n${YELLOW}Disassembling generated classes...${NC}"
        for classfile in $(find "$DUMP_DIR" -name "*.class"); do
            echo -e "\n${BLUE}=== $classfile ===${NC}"
            javap -c -v "$classfile" > "${classfile}.javap" 2>&1 || true
            echo "  Saved to: ${classfile}.javap"
        done
    fi
}

analyze_gc() {
    echo -e "${GREEN}=== Running with GC Analysis ===${NC}"
    sbt "set scalusUplcJitCompiler/Test/javaOptions ++= Seq(\"-Xlog:gc*:file=$OUTPUT_DIR/gc.log\"); scalusUplcJitCompiler/Test/runMain $TEST_CLASS" | tee "$OUTPUT_DIR/gc_output.txt"
}

run_jmh_with_profiling() {
    echo -e "${GREEN}=== Running JMH Benchmark with Profiling ===${NC}"

    # Check if JMH benchmarks exist
    if [ ! -f "bench/src/main/scala/scalus/uplc/eval/JITBenchmark.scala" ]; then
        echo -e "${YELLOW}JMH benchmark not found, skipping${NC}"
        return
    fi

    echo "Running JIT benchmark with gc profiler..."
    sbt "bench/Jmh/run -prof gc scalus.uplc.eval.JITBenchmark" | tee "$OUTPUT_DIR/jmh_gc.txt"

    echo "Running JIT benchmark with perfnorm profiler..."
    sbt "bench/Jmh/run -prof perfnorm scalus.uplc.eval.JITBenchmark" | tee "$OUTPUT_DIR/jmh_perfnorm.txt"
}

print_summary() {
    echo -e "\n${GREEN}=== Analysis Summary ===${NC}"
    echo "Output files in: $OUTPUT_DIR/"
    echo ""
    echo "Key files to review:"
    echo "  - compilation_log.txt: JIT compilation events"
    echo "  - inlining_summary.txt: Inlining decisions"
    echo "  - gc.log: Garbage collection statistics"
    echo "  - staged-classes/*.javap: Disassembled generated code"
    echo ""
    echo -e "${YELLOW}Next steps:${NC}"
    echo "  1. Review inlining_summary.txt for methods that failed to inline"
    echo "  2. Check staged-classes/*.javap to see actual generated bytecode"
    echo "  3. Look for allocation hotspots in GC logs"
    echo "  4. Compare compilation patterns between JIT and CEK"
}

# Main execution
case "${1:-all}" in
    basic)
        run_basic
        ;;
    compilation)
        run_with_compilation_log
        ;;
    inlining)
        run_with_inlining_log
        ;;
    assembly)
        run_with_assembly
        ;;
    classes)
        dump_staged_classes
        ;;
    gc)
        analyze_gc
        ;;
    jmh)
        run_jmh_with_profiling
        ;;
    all)
        run_basic
        echo -e "\n"
        run_with_compilation_log
        echo -e "\n"
        run_with_inlining_log
        echo -e "\n"
        dump_staged_classes
        echo -e "\n"
        analyze_gc
        echo -e "\n"
        print_summary
        ;;
    *)
        echo "Usage: $0 {basic|compilation|inlining|assembly|classes|gc|jmh|all}"
        exit 1
        ;;
esac
