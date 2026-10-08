#!/usr/bin/env bash
# Checks that the macOS JNI library loads on the oldest supported macOS version.
# Usage: check-macos.sh <libscalus_crypto.dylib> <max minos, e.g. 11.0>
set -euo pipefail
dylib="$1"
max="$2"

minos=$(otool -l "$dylib" | awk '/LC_BUILD_VERSION/ { f = 1 } f && $1 == "minos" { print $2; exit }')
if [ -z "$minos" ]; then
    echo "FAIL: no LC_BUILD_VERSION in $dylib"
    exit 1
fi
if [ "$(printf '%s\n%s\n' "$minos" "$max" | sort -V | tail -1)" != "$max" ]; then
    echo "FAIL: minos $minos, the policy allows up to $max"
    exit 1
fi
echo "OK: minos $minos (max $max)"
