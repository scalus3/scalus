#!/usr/bin/env bash
# Adds a swap file to a GitHub-hosted Linux runner. The image sets the swap it starts with: on
# 2026-11-24 one CI-JS runner had none and the next had a 3 GB /swapfile, so add ours either way.
#
# Usage: scripts/ci/add-swap.sh <size>, for example 8G
#
# The file goes on /mnt when that is a separate disk (the temporary disk of the runner), else on /.
set -euo pipefail

size=$1
dir=/
if mountpoint -q /mnt; then dir=/mnt; fi
file="$dir/swapfile-ci"

df -h "$dir"
sudo fallocate -l "$size" "$file"
sudo chmod 600 "$file"
sudo mkswap "$file" > /dev/null
sudo swapon "$file"
swapon --show
