#!/usr/bin/env bash
# Checks the Linux JNI library against the platform policy: it needs only libc and the dynamic
# loader, no glibc newer than MAX_GLIBC, and it exports only the JNI entry points.
# Usage: check-linux.sh <libscalus_crypto.so>   (needs readelf, objdump, nm from binutils)
set -euo pipefail
so="$1"
max="${MAX_GLIBC:-2.34}"

needed=$(readelf -d "$so" | sed -n 's/.*Shared library: \[\(.*\)\]/\1/p')
for lib in $needed; do
    case "$lib" in
        libc.so.6 | ld-linux-*.so.*) ;;
        *) echo "FAIL: unexpected NEEDED library $lib"; exit 1 ;;
    esac
done

highest=$(objdump -T "$so" | grep -o 'GLIBC_[0-9.]*' | sed 's/GLIBC_//' | sort -uV | tail -1)
if [ "$(printf '%s\n%s\n' "$highest" "$max" | sort -V | tail -1)" != "$max" ]; then
    echo "FAIL: needs GLIBC_$highest, the policy allows up to GLIBC_$max:"
    objdump -T "$so" | grep "GLIBC_$highest"
    exit 1
fi

extra=$(nm -D --defined-only "$so" | awk '{print $NF}' | grep -v -E '^(Java_.*|JNI_OnLoad)$' || true)
if [ -n "$extra" ]; then
    echo "FAIL: exports symbols other than the JNI entry points:"
    echo "$extra"
    exit 1
fi

echo "OK: NEEDED = $(echo $needed), highest GLIBC_$highest (max $max), only JNI symbols exported"
