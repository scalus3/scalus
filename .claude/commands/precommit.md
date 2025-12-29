---
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git branch:*), Bash(git log:*), Bash(sbtn quick:*)
description: Review code, run tests, write a git commit message with context
---

## Context

- Current git status: !`git status`
- Current git diff: !`git diff HEAD`
- Current branch: !`git branch --show-current`
- Recent commits: !`git log --oneline -10`

## Your task

- Review code (use code reviewer agent)
- Run `optimize` command
- Run tests and code formatting: `sbtn quick`
- Run `commit` command and provide the commit message
