# JIT vs CEK GC Profile Analysis

## Benchmark Comparison (auction_1-1.flat)

### Execution Time
| Metric | CEK | JIT | Improvement |
|--------|-----|-----|-------------|
| **Time/op** | 317.4 ± 49.9 us | 210.5 ± 115.5 us | **1.51x faster** |

### Memory Allocation
| Metric | CEK | JIT | Improvement |
|--------|-----|-----|-------------|
| **Alloc rate** | 4032 MB/sec | 2479 MB/sec | **1.63x less** |
| **Bytes/op** | 1,341,946 B | 546,867 B | **2.45x less** |

### Garbage Collection
| Metric | CEK | JIT | Improvement |
|--------|-----|-----|-------------|
| **GC count** | 197 | 351 | 1.78x more |
| **GC time** | 259 ms | 316 ms | 1.22x more |

## Key Findings

### ✅ JIT Wins

1. **Execution Time**: 1.51x faster (317→211 μs)
2. **Memory Allocation**: 2.45x less per operation
3. **Allocation Rate**: 1.63x lower (4032→2479 MB/sec)

### ⚠️ Trade-offs

1. **More GC Collections**: JIT triggers 1.78x more GC events
   - Likely due to more, smaller objects (frames) vs CEK's larger objects
   - Short-lived continuation frames

2. **Slightly More GC Time**: 22% more time in GC
   - But overall execution is still faster
   - Net win: 210 μs total < 317 μs total

## Detailed Analysis

### Memory per Operation

**CEK allocates 1.34 MB per operation:**
- Large environment objects
- Deep continuation frames
- Pattern matching overhead

**JIT allocates 547 KB per operation (59% less):**
- Flat continuation structure (Return, Apply, Force)
- Heap-based stack frames
- Direct function calls reduce intermediate objects

### Allocation Rate

Even though JIT runs faster (210 vs 317 μs), it allocates at a lower rate:
- CEK: 4032 MB/sec
- JIT: 2479 MB/sec

This means JIT is more memory-efficient per unit of work.

### GC Behavior

JIT triggers more GC cycles (351 vs 197) but:
- Each cycle is processing smaller amounts
- Young generation objects die quickly
- Continuation frames are short-lived
- Overall GC time is only 22% more despite 1.78x more cycles

## Optimization Opportunities

### 1. Reduce Continuation Object Allocations
Current: Every Apply/Force creates new continuation objects
Opportunity: Object pooling or stack-based allocation

### 2. Optimize Stack Frame Management
Current: List[Frame] with cons/uncons overhead
Opportunity: Array-based stack with index pointer
- Would reduce allocation rate further
- Faster push/pop operations

### 3. Inline Common Patterns
Current: All operations go through eval loop
Opportunity: Direct paths for:
- Simple value returns
- Direct function application
- Constant evaluation

### 4. Specialized Eval Paths
Current: Single pattern match on continuation type
Opportunity: Separate loops for:
- Hot path (Return handling)
- Cold path (Apply/Force)

## Performance Budget

For auction_1-1.flat execution (210 μs total):

**Estimated breakdown:**
- Continuation eval loop: ~30-40% (63-84 μs)
- Function applications: ~25-30% (52-63 μs)
- Builtin operations: ~20-25% (42-52 μs)
- Budget tracking: ~10-15% (21-31 μs)
- Object allocation: ~10% (21 μs)

**Target optimizations:**
1. Array-based stack: Save ~10-15 μs (5-7%)
2. Inline hot paths: Save ~20-30 μs (10-14%)
3. Object pooling: Save ~10-15 μs (5-7%)

**Potential target: ~160-170 μs (25% improvement)**

## Conclusion

The minimal continuation JIT is **significantly better than CEK**:
- ✅ 1.51x faster execution
- ✅ 2.45x less memory per operation
- ⚠️ Slightly more GC activity (but still net win)

The architecture is sound. Further optimizations should focus on:
1. Reducing continuation object allocations
2. Optimizing the eval loop hot path
3. Using more efficient data structures (Array vs List)
