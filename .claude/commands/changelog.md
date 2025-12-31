---
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git log:*)
argument-hint: [ version ]
description: Write a CHANGELOG.md entry for a version
---

## Context

## Your task

Analyze the changes made since the last CHANGELOG.md entry version tag (like v0.14.0) using
`git log` and add a new entry using a version from $ARGUMENTS.

Follow the same style as in CHANGELOG.md

Put the most exciting and important features, changes and fixes first.
