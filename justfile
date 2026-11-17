#!/usr/bin/env -S guix shell -m manifest.scm -- just --justfile

set shell := ["bash", "-euo", "pipefail", "-c"]

# List available recipes
default:
    @just --list

# Run full test suite (Stage 1 unit/kernel tests + Stage 2 verify-geasa)
test:
    ./scripts/test

# Verify all geasa bundle manifests, pins, and contracts
verify-geasa:
    ./scripts/verify-geasa

# Verify a single geis bundle by feature-id
verify fid:
    ./scripts/badb geis verify {{fid}}

# Run end-to-end tests (pass --integration or --tui)
e2e *args:
    ./scripts/test-e2e {{args}}

# Format Scheme files with cl-indentify, re-pin touched bundles, and stage changes
format +files:
    ./scripts/format-scheme {{files}}

# Check Scheme formatting without modifying files
format-check +files:
    ./scripts/format-scheme --check {{files}}

# Lint the codebase with semcp
lint *args:
    semcp --no-session lint {{args}}

# Re-pin a geis bundle manifest
pin fid:
    ./scripts/badb geis pin src/geasa/{{fid}}

# Preview the next SemVer version from Conventional Commits
bump-preview:
    git cliff --bumped-version

# Update CHANGELOG.md for the next SemVer release
changelog:
    git cliff --bump -o CHANGELOG.md

# Configure Git to use .githooks/
hooks:
    git config core.hooksPath .githooks

# Run tests, stage src/geasa/<fid>/, commit, and tag geis/<fid>
commit-feature fid msg:
    ./scripts/commit-feature {{fid}} "{{msg}}"

# Build distribution artifacts
dist *args:
    ./scripts/dist {{args}}

# Build single-file badb executable, install to ~/.local/bin/badb, and bootstrap base geasa
install:
    ./scripts/dist --single-file -o dist/badb
    mkdir -p ~/.local/bin
    cp dist/badb ~/.local/bin/badb
    chmod 0755 ~/.local/bin/badb
    ~/.local/bin/badb bootstrap --from ./src/geasa

# Run ACP TCK suite
test-acp-tck *args:
    ./scripts/test-acp-tck {{args}}

# Run adversarial test suite
test-adversarial *args:
    ./scripts/test-adversarial {{args}}

# Capture TUI screenshot
screenshot-tui *args:
    ./scripts/screenshot-tui {{args}}

# Run badb CLI
badb *args:
    ./scripts/badb {{args}}
