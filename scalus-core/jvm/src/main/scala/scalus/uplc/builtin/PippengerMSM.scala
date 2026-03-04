package scalus.uplc.builtin

import java.math.BigInteger

/** Pippenger's bucket method for multi-scalar multiplication.
  *
  * Reduces N expensive scalar multiplications to O(N) cheap point additions plus a small number of
  * doublings, yielding ~10x speedup for large N (e.g. 32K points).
  */
private[builtin] object PippengerMSM {

    /** Threshold below which naive MSM is faster due to Pippenger overhead. */
    private val NaiveThreshold = 8

    /** Compute sum(scalars[i] * points[i]) using Pippenger's bucket method.
      *
      * @param scalars
      *   reduced scalars (already mod field order), length N
      * @param points
      *   group elements, length N
      * @param identity
      *   lazy identity element (only materialized if needed)
      * @param dup
      *   duplicate a point (deep copy)
      * @param addInPlace
      *   mutates and returns first arg: a += b
      * @param dblInPlace
      *   mutates and returns arg: a *= 2
      * @param mulScalar
      *   scalar multiplication for naive fallback: (point, scalar) => result
      */
    def msm[P <: AnyRef](
        scalars: Array[BigInteger],
        points: Array[P],
        identity: => P,
        dup: P => P,
        addInPlace: (P, P) => P,
        dblInPlace: P => P,
        mulScalar: (P, BigInteger) => P
    ): P = {
        val n = math.min(scalars.length, points.length)
        if n == 0 then return identity

        if n < NaiveThreshold then
            return naiveMsm(scalars, points, n, identity, dup, addInPlace, mulScalar)

        pippenger(scalars, points, n, identity, dup, addInPlace, dblInPlace)
    }

    private def naiveMsm[P <: AnyRef](
        scalars: Array[BigInteger],
        points: Array[P],
        n: Int,
        identity: => P,
        dup: P => P,
        addInPlace: (P, P) => P,
        mulScalar: (P, BigInteger) => P
    ): P = {
        var acc: P = null.asInstanceOf[P]
        var i = 0
        while i < n do
            if scalars(i).signum() != 0 then
                val term = mulScalar(points(i), scalars(i))
                acc = nullableAdd(acc, term, dup, addInPlace)
            i += 1
        if acc == null then identity else acc
    }

    private def pippenger[P <: AnyRef](
        scalars: Array[BigInteger],
        points: Array[P],
        n: Int,
        identity: => P,
        dup: P => P,
        addInPlace: (P, P) => P,
        dblInPlace: P => P
    ): P = {
        // Window size: c = max(1, floor(log2(n))), capped at 20
        val c = math.max(1, math.min(20, (math.log(n.toDouble) / math.log(2.0)).toInt))
        val numBuckets = 1 << c // 2^c
        val mask = numBuckets - 1

        // Find max bit length across all scalars
        var maxBits = 0
        var i = 0
        while i < n do
            val bl = scalars(i).bitLength()
            if bl > maxBits then maxBits = bl
            i += 1

        if maxBits == 0 then return identity

        val numWindows = (maxBits + c - 1) / c

        // Compute per-window sums
        val windowSums = new Array[AnyRef](numWindows)

        var w = 0
        while w < numWindows do
            val bitOffset = w * c
            // Buckets: index 0 unused (k=0 means skip), indices 1..numBuckets-1
            val buckets = new Array[AnyRef](numBuckets)

            i = 0
            while i < n do
                val k = extractWindow(scalars(i), bitOffset, c, mask)
                if k > 0 then
                    val bucket = buckets(k).asInstanceOf[P]
                    if bucket == null then buckets(k) = dup(points(i))
                    else addInPlace(bucket, points(i))
                i += 1

            // Reduce buckets: running accumulator from high to low
            var running: P = null.asInstanceOf[P]
            var sum: P = null.asInstanceOf[P]
            var k = numBuckets - 1
            while k >= 1 do
                val bucket = buckets(k).asInstanceOf[P]
                if bucket != null then running = nullableAdd(running, bucket, dup, addInPlace)
                if running != null then sum = nullableAdd(sum, running, dup, addInPlace)
                k -= 1

            windowSums(w) = sum
            w += 1

        // Combine windows via Horner's method: result = windowSums[last]
        // then for each lower window: double c times, add windowSum
        var result: P = windowSums(numWindows - 1).asInstanceOf[P]
        w = numWindows - 2
        while w >= 0 do
            if result != null then
                var d = 0
                while d < c do
                    dblInPlace(result)
                    d += 1
            val ws = windowSums(w).asInstanceOf[P]
            if ws != null then result = nullableAdd(result, ws, dup, addInPlace)
            w -= 1

        if result == null then identity else result
    }

    /** Extract c bits from scalar starting at bitOffset. */
    private def extractWindow(scalar: BigInteger, bitOffset: Int, c: Int, mask: Int): Int =
        scalar.shiftRight(bitOffset).intValue() & mask

    /** Add b into a, handling null (identity) cases. Returns the combined point. */
    private def nullableAdd[P <: AnyRef](
        a: P,
        b: P,
        dup: P => P,
        addInPlace: (P, P) => P
    ): P = {
        if a == null then dup(b)
        else addInPlace(a, b)
    }
}
