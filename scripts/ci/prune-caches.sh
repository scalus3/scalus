#!/usr/bin/env bash
# Deletes each GitHub Actions cache entry whose key starts with PREFIX, except KEEP_KEY.
#
#   scripts/ci/prune-caches.sh [--dry-run] PREFIX KEEP_KEY
#
# A job runs it after it saves an entry, so each cache kind keeps one entry per job (rule
# [ci.prune.after-save] of docs/superpowers/specs/2026-11-21-ci-speed-design.md). It needs GH_TOKEN
# with `actions: write`; --dry-run only lists the keys and needs `actions: read`.
set -euo pipefail

dry_run=false
if [[ "${1:-}" == "--dry-run" ]]; then
    dry_run=true
    shift
fi
if [[ $# -ne 2 || -z "$1" ]]; then
    echo "usage: $0 [--dry-run] PREFIX KEEP_KEY" >&2
    exit 2
fi
prefix=$1
keep=$2

# `gh cache list --key` matches by prefix; the check below keeps that true if gh ever changes it.
gh cache list --key "$prefix" --limit 1000 --json key --jq '.[].key' | sort -u |
    while read -r key; do
        [[ "$key" == "$prefix"* && "$key" != "$keep" ]] || continue
        if $dry_run; then
            echo "would delete $key"
        else
            echo "deleting $key"
            gh cache delete "$key"
        fi
    done
