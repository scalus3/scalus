#!/bin/bash
# JIT Performance Profiling Script

echo "=== JIT Compiler Performance Profiling ==="
echo ""

# Run basic benchmarks
echo "1. Running JMH benchmarks (this will take several minutes)..."
echo "   Running CEK interpreter baseline..."
sbtn "bench/Jmh/run -i 3 -wi 2 -f 1 .*CekJVMBenchmark.* -rf json -rff results_cek.json"

echo ""
echo "   Running JIT compiler..."
sbtn "bench/Jmh/run -i 3 -wi 2 -f 1 .*JITBenchmark.* -rf json -rff results_jit.json"

echo ""
echo "2. Running with GC profiler to check allocation pressure..."
sbtn "bench/Jmh/run -i 3 -wi 2 -f 1 -prof gc .*JITBenchmark.*" 2>&1 | tee jit_gc_profile.txt

echo ""
echo "3. Running with stack profiler..."
sbtn "bench/Jmh/run -i 3 -wi 2 -f 1 -prof stack:lines=20 .*JITBenchmark.*" 2>&1 | tee jit_stack_profile.txt

echo ""
echo "=== Results ==="
echo "Benchmark results saved to:"
echo "  - results_cek.json"
echo "  - results_jit.json"
echo "  - jit_gc_profile.txt (allocation data)"
echo "  - jit_stack_profile.txt (hotspot data)"
echo ""
echo "For detailed profiling, install async-profiler:"
echo "  https://github.com/async-profiler/async-profiler"
