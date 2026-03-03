#!/bin/bash
set -e
cd "$(dirname "$0")"
aiken build
echo "Done. Blueprint at: plutus.json"
