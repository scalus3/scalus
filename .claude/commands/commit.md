---
allowed-tools: Bash(git status:*), Bash(git diff:*)
argument-hint: [message]
description: Write a git commit message with context
---

## Context

- Current git status: !`git status`
- Current git diff: !`git diff HEAD`
- Current branch: !`git branch --show-current`
- Recent commits: !`git log --oneline -10`

## Your task

Analyze the changes and create an appropriate commit message following conventional commits format:
- `feat:` for new features
- `fix:` for bug fixes
- `docs:` for documentation changes
- `refactor:` for code refactoring
- `test:` for adding tests
- `chore:` for maintenance tasks

Include concise description of changes. Don't do the actual commit, just provide the message.