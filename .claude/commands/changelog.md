---
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git log:*)
argument-hint: [ version ]
description: Write a CHANGELOG.md entry for a version
---

## Context

## Your task

1. Read CHANGELOG.md to find the last version (e.g., 0.14.1)
2. Use the git tag for that version to get changes: `git log v<last_version>..HEAD --oneline`
   - Example: if CHANGELOG.md shows 0.14.1 as latest, run `git log v0.14.1..HEAD --oneline`
3. Analyze the commits and add a new entry using version from $ARGUMENTS

Follow the same style as in CHANGELOG.md

Put the most exciting and important features, changes and fixes first.
