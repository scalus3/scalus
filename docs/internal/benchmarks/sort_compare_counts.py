"""Comparison counts for the candidate on-chain sorting algorithms.

Mirrors SortCandidates.scala exactly, on Python lists used as cons-lists (index 0 = head).
Counts every invocation of the Ord comparison, which is the dominant per-element cost on-chain
once the element type is anything more expensive than a machine integer.

Run: python3 compare_counts.py
"""

import sys

sys.setrecursionlimit(100000)

N_COUNT = [0]


def cmp(a, b):
    N_COUNT[0] += 1
    return (a > b) - (a < b)


# --- 1. prelude head-pivot quicksort (List.scala:351) -------------------------------------


def prelude_sort(xs):
    def partition(lst, pivot, before, after):
        for h in lst:
            if cmp(h, pivot) < 0:
                before = [h] + before
            else:
                after = [h] + after
        return before, after

    def sort_acc(lst, acc):
        if not lst:
            return acc
        pivot, rest = lst[0], lst[1:]
        before, after = partition(rest, pivot, [], [])
        return sort_acc(before, [pivot] + sort_acc(after, acc))

    return sort_acc(xs, [])


# --- 2. insertion sort, right-to-left (Aiken stdlib) --------------------------------------


def insertion_right(xs):
    def insert(lst, e):
        if not lst:
            return [e]
        x, rest = lst[0], lst[1:]
        if cmp(e, x) < 0:
            return [e] + lst
        return [x] + insert(rest, e)

    def go(lst):
        if not lst:
            return []
        return insert(go(lst[1:]), lst[0])

    return go(xs)


# --- 3. insertion sort, left-to-right -----------------------------------------------------


def insertion_left(xs):
    def insert(lst, e):
        if not lst:
            return [e]
        x, rest = lst[0], lst[1:]
        if cmp(e, x) < 0:
            return [e] + lst
        return [x] + insert(rest, e)

    acc = []
    for x in xs:
        acc = insert(acc, x)
    return acc


# --- 4. top-down merge sort with alternating deal -----------------------------------------


def merge_lists(a, b):
    out = []
    while a and b:
        if cmp(b[0], a[0]) < 0:
            out.append(b[0])
            b = b[1:]
        else:
            out.append(a[0])
            a = a[1:]
    return out + a + b


def merge_deal(xs):
    def deal(lst):
        l, r = [], []
        for x in lst:
            l, r = r, [x] + l
        return l, r

    def go(lst):
        if len(lst) <= 1:
            return lst
        l, r = deal(lst)
        return merge_lists(go(l), go(r))

    return go(xs)


# --- 5. natural merge sort (GHC sortBy shape) ---------------------------------------------


def merge_natural(xs):
    if not xs:
        return []

    # ascending-run detection, mirroring SortCandidates.runs
    runs = []
    cur = xs[0]
    acc = []
    for x in xs[1:]:
        if cmp(x, cur) < 0:
            runs.append(list(reversed([cur] + acc)))
            cur, acc = x, []
        else:
            acc = [cur] + acc
            cur = x
    runs.append(list(reversed([cur] + acc)))

    while len(runs) > 1:
        nxt = []
        i = 0
        while i + 1 < len(runs):
            nxt.append(merge_lists(runs[i], runs[i + 1]))
            i += 2
        if i < len(runs):
            nxt.append(runs[i])
        runs = nxt
    return runs[0]


# --- 6. quicksort, median-of-three pivot, three-way partition -----------------------------


def qsort_median3(xs):
    def last_of(lst, cur):
        for x in lst:
            cur = x
        return cur

    def mid_of(lst):
        slow = fast = 0
        cur = lst[0]
        while fast + 1 < len(lst):
            fast += 2
            cur = lst[slow]
            slow += 1
        return cur

    def median3(a, b, c):
        if cmp(a, b) < 0:
            if cmp(b, c) < 0:
                return b
            return c if cmp(a, c) < 0 else a
        if cmp(a, c) < 0:
            return a
        return c if cmp(b, c) < 0 else b

    def partition3(lst, pivot):
        lt, eq, gt = [], [], []
        for x in lst:
            c = cmp(x, pivot)
            if c < 0:
                lt = [x] + lt
            elif c == 0:
                eq = [x] + eq
            else:
                gt = [x] + gt
        return lt, eq, gt

    def sort_acc(lst, acc):
        if not lst:
            return acc
        if len(lst) == 1:
            return lst + acc
        pivot = median3(lst[0], mid_of(lst), last_of(lst[1:], lst[0]))
        lt, eq, gt = partition3(lst, pivot)
        return sort_acc(lt, list(reversed(eq)) + sort_acc(gt, acc))

    return sort_acc(xs, [])


# --- 7. verify-only floor -----------------------------------------------------------------


def is_sorted(xs):
    if not xs:
        return True
    prev = xs[0]
    for y in xs[1:]:
        if cmp(y, prev) < 0:
            return False
        prev = y
    return True


# --- 8. guarded merge sort ----------------------------------------------------------------


def merge_guarded(xs):
    if is_sorted(xs):
        return xs
    return merge_deal(xs)


ALGOS = [
    ("prelude(qsort)", prelude_sort),
    ("insertRight", insertion_right),
    ("insertLeft", insertion_left),
    ("mergeDeal", merge_deal),
    ("mergeNatural", merge_natural),
    ("qsortMedian3", qsort_median3),
    ("mergeGuarded", merge_guarded),
    ("isSorted(only)", is_sorted),
]


def patterns(n):
    import random

    rnd = random.Random(42)
    rand = list(range(1, n + 1))
    rnd.shuffle(rand)
    return [
        ("random", rand),
        ("ascending", list(range(1, n + 1))),
        ("descending", list(range(n, 0, -1))),
        ("allEqual", [1] * n),
        ("fewUnique", [i % 3 for i in range(1, n + 1)]),
        # adversarial: the classic median-of-3 killer, and a sawtooth
        ("sawtooth", [i % max(1, n // 4) for i in range(1, n + 1)]),
        ("organPipe", list(range(1, n // 2 + 1)) + list(range(n - n // 2, 0, -1))),
    ]


def main():
    sizes = [4, 8, 16, 32, 64]
    pat_names = [p[0] for p in patterns(8)]

    for n in sizes:
        print(f"\n=== n = {n} : comparison counts ===")
        header = f"{'algorithm':<16}" + "".join(f"{p:>11}" for p in pat_names) + f"{'WORST':>11}"
        print(header)
        print("-" * len(header))
        for name, fn in ALGOS:
            row = []
            for _, data in patterns(n):
                N_COUNT[0] = 0
                out = fn(list(data))
                if name != "isSorted(only)":
                    assert out == sorted(data), f"{name} wrong on n={n}"
                row.append(N_COUNT[0])
            print(
                f"{name:<16}"
                + "".join(f"{c:>11d}" for c in row)
                + f"{max(row):>11d}"
            )


if __name__ == "__main__":
    main()
