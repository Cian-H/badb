# Changelog

All notable changes to `badb` will be documented in this file.
## [2026.11.19.1] - 2026-11-19

### Bug Fixes

- *(surfaces-tui)* Remove redundant models menu entry
- *(surfaces-tui)* Run bash tool through /usr/bin/env bash
- *(surfaces-tui)* Clarify geis verify uses locked copy and how to re-lock
- *(surfaces-tui)* Re-init terminal backend after external editor exits
- *(surfaces-tui)* Ctrl+e expands latest collapsed message without selection
- *(surfaces-tui)* Animate terminal tab spinner by including frame in title signature
- *(surfaces-tui)* Queue steering past final answer and keep urgent steer from re-stopping the loop
- *(runtimes-react)* Skip result offload for ctx-virtualized bash output
- *(surfaces-tui)* Remove /models from slash autocomplete registry
- *(surfaces-tui)* Sort %slash-commands alphabetically
- *(surfaces-tui)* Remove redundant theme entry from top-level command menu
- *(surfaces-tui)* Nest endpoint, provider, features, journal, and test in command tree
- *(surfaces-tui)* Support Backspace to pop one menu level
- *(surfaces-tui)* Restore 'slash prompt mode instead of literal / on back-to-slash
- *(dist)* Finalize single-file guix pack wrapper, two-pass bootstrap, and persona pins
- *(geis)* Pass load paths to guild during bundle precompile and warn on failure (ui-fpl)
- *(dist)* Put bundled sema/guile on PATH in packed wrapper, report bootstrap failure reasons, silence dist compile warning
- *(capabilities-rtk)* Spawn rtk via posix_spawn instead of forking a threaded Guile (ui-4q8)
- *(geasa)* Resolve unbound vars, arity/shadowing bugs, and hot-loop allocations (ui-9t8, ui-nzn, ui-5do)
- *(surfaces-tui)* Log catch #t failures in tools and daemon accept loop (ui-09q)
- *(surfaces-tui)* Restore effective-demarcation-style and mode->badge-label
- *(surfaces-geis-updater)* Persist cycled update mode across calls

### CI

- Add pre-push test hook and GitHub Actions commitlint workflow
- Remove commitlint GitHub Actions workflow

### Documentation

- *(changelog)* Generate CHANGELOG.md with git-cliff
- Rename readme.md and license.md to uppercase stems
- Update README and TUI screenshot
- *(install)* Document single-file ~/.local/bin/badb and bootstrap flow (ui-2te)

### Features

- *(surfaces-tui)* Support module-declared command menu paths and arbitrary-depth nesting
- *(surfaces-tui)* Add Back navigation item to nested menus
- *(version)* Read app version from shared VERSION file via git-cliff hook
- *(surfaces-tui)* Attach Commands root as parent for direct slash-command menus
- *(surfaces-tui)* Return to slash command input palette on Back from direct command menus
- *(geis)* Decouple bundle discovery from repo cwd and add XDG user store (ui-67l)
- *(geis)* Add per-geis precompile option and post-verify ccache (ui-92p)
- *(geis)* Add geis-pack-base-collection and badb geis pack --all (ui-usn)
- *(geis)* Add geis-bootstrap-base! and badb bootstrap CLI (ui-4hh)
- *(guix)* Sync badb runtime closure and add badb-kernel package (ui-jk3)
- *(dist)* Add single-file self-extracting executable and just install (ui-lay)
- *(surfaces-tui)* Add standalone UI server daemon and remote SSH/socket client
- *(geis)* Pure-Scheme CalVer comparator, manifest source schema & auto-bump on pin (ui-99j)
- *(runtimes-geis-pkg)* Feat(runtimes-geis-pkg): add repo-cached git fetcher, lockfile, atomic verify/swap, rollback & enable/disable (ui-bek)
- *(geis)* Add declarative geasa config sync and badb geis CLI subcommands (ui-3y0)
- *(surfaces-widgets)* Extract widget primitives & refactor menus (ui-13v)
- *(runtimes-geis-updater)* Add per-scope TTL-cached background geasa updater
- *(surfaces-overlay)* Extract overlay & add modular modal shell (ui-7x8)
- *(surfaces-geis-manager)* Add /geasa modal, extension API, and TUI tool gating
- *(surfaces-geis-updater)* Add updater UI extensions, TUI startup check, and e2e scenario
- *(surfaces)* Unify /geis manager, add table-widget, fix modal layout & scrolling
- *(surfaces-tui)* Modular TUI overlays, themes, history, and /config UX
- *(surfaces-tui)* Refactor(surfaces-tui): remove redundant components, duplicates, and dead code

### Miscellaneous Tasks

- Added ignores for "beads"
- Sorted packages
- *(calver)* Migrate repo and base geasa manifests to CalVer (ui-0uq)
- Optimised AGENTS.md
- Gitignore dist/ and untrack dist/badb

### Refactor

- *(surfaces-tui)* Drop endwin/refresh bindings left unused by editor resume fix
- *(surfaces-tui)* Derive %slash-commands dynamically from %command-categories
- *(surfaces-tui)* Trim low-frequency and nested commands from top-level menu
- *(geasa)* Remove dead definitions and decompose TUI key handlers (ui-gku, ui-j2l, ui-p1v)
- Static analysis, peval optimization, and lint cleanup pass

### Testing

- Disable srfi-64 root logs, rename wp* suites, and ignore dist/
- Deduplicate suites, remove legacy E2E overlap, and strengthen thin specs

## [0.144.9] - 2026-11-16

### Documentation

- *(readme)* Add TUI screenshot

## [0.144.8] - 2026-11-16

### Documentation

- *(surfaces-tui)* Update tui screenshot

## [0.144.7] - 2026-11-16

### Miscellaneous Tasks

- *(dev)* Add justfile for maintenance and release tasks

## [0.144.6] - 2026-11-16

### Miscellaneous Tasks

- *(git)* Configure semver changelog and conventional commit hook

## [0.144.5] - 2026-11-16

### Bug Fixes

- *(surfaces-tui)* Keep crow animation feet anchored on frame 2

## [0.144.4] - 2026-11-16

### Refactor

- *(geasa)* Decouple bundle imports, deduplicate helpers, and resolve lint anti-patterns

## [0.144.3] - 2026-11-16

### Bug Fixes

- *(runtimes-synthesis)* Re-record synthesis-fixture cassette for updated prompt

## [0.144.2] - 2026-11-16

### Refactor

- *(surfaces-tui)* Modularize run-tui and add pluggable overlay protocol

## [0.144.1] - 2026-11-16

### Bug Fixes

- *(surfaces-tui)* Exit scheme mode when deleting leading '('

## [0.144.0] - 2026-11-16

### Features

- *(surfaces-tui)* Unify top and bottom status bars into bottom bar

## [0.143.0] - 2026-11-16

### Features

- *(surfaces-tui)* Wire ncvisual images, ncuplot geis bindings, and extract run-tui helpers

## [0.142.0] - 2026-11-16

### Features

- *(surfaces-tui)* Insert newline on shift+enter

## [0.141.1] - 2026-11-16

### Bug Fixes

- *(foundation-config)* Escape quotes in chat provider docstrings

## [0.141.0] - 2026-11-16

### Features

- *(surfaces-tui)* Animate B4 Braille hooded crow startup banner

## [0.140.0] - 2026-11-16

### Features

- *(surfaces-tui)* Integrate WP1-WP5 UI modules, menus, markdown, keymaps and subagents (ui-wp6.2)

## [0.139.2] - 2026-11-16

## [0.139.1] - 2026-11-16

### Bug Fixes

- *(surfaces-tui)* Import (srfi srfi-43) for vector-map in protocol.scm

## [0.139.0] - 2026-11-16

### Features

- *(surfaces-tui)* Implement WP2 markdown, transcript highlighting, thinking collapse fix, spinner/OSC title, and terminal protocols

## [0.138.0] - 2026-11-16

### Features

- *(surfaces-tui)* Mid-run safe slash commands, force-stop-agent!, urgent steering and redesigned steering bar (ui-wp6.1)

## [0.137.0] - 2026-11-16

### Features

- *(agents)* Add subagent awareness, event bus, ReAct bubbling, and hierarchy overview

## [0.136.0] - 2026-11-16

### Features

- *(surfaces-tui)* Add slash mode, Ctrl+R fuzzy history, @ completion, keymaps, shell/sudo/editor, and resume/REPL helpers

## [0.135.0] - 2026-11-16

### Features

- *(surfaces-tui)* Add modular widgets and UI server/client protocol (WP1)

## [0.134.0] - 2026-11-16

### Features

- *(ui-wp4)* Nested menus, command dedup, views cleanup, plots & CLI alignment

## [0.133.0] - 2026-11-16

### Features

- *(surfaces-tui)* Use guile-notcurses and add TUI screenshot script

## [0.132.1] - 2026-11-16

### Refactor

- *(capabilities-fs-tools,runtimes-react,surfaces-tui)* Add pre/post hooks across file mutations, read_file, shell aliases, and tool dispatch

## [0.132.0] - 2026-11-16

### Features

- *(capabilities-rtk)* Add rtk geis bundle and wire bash pre-hook in surfaces-tui

## [0.131.0] - 2026-11-16

### Features

- *(guix)* Add rtk package and include in manifest

## [0.130.0] - 2026-11-16

### Features

- *(foundation-config,surfaces-tui)* Added endpoint kind filtering

## [0.129.0] - 2026-11-16

### Features

- *(workflows)* Integrate semcp lint gate and AST tool routing

## [0.128.0] - 2026-11-16

### Features

- *(capabilities-structural-editing)* Package semcp via Guix and integrate geis bundle

## [0.127.0] - 2026-11-16

### Features

- *(capabilities-context-index)* Port context-mode to SQLite FTS5 with code chunking and batch search

## [0.126.5] - 2026-11-16

### Bug Fixes

- *(surfaces-tui,runtimes-react)* Fix checkpoint_create HAMT length and 1-arg alist tool dispatch

## [0.126.4] - 2026-11-16

### Bug Fixes

- *(runtimes-react)* Clear fs read cache on context compaction

## [0.126.3] - 2026-11-16

### Bug Fixes

- *(config,providers,tui)* Prevent api-key leakage and fix context-limit detection

## [0.126.2] - 2026-11-16

### Bug Fixes

- *(providers-model,surfaces-tui)* Validate endpoint URIs before uri-path

## [0.126.1] - 2026-11-16

### Miscellaneous Tasks

- Added `.serena` to gitignore

## [0.126.0] - 2026-11-13

### Features

- *(agents-subagent,surfaces-acp)* Implement ACP-based local/remote subagent and superagent model

## [0.125.2] - 2026-11-13

### Bug Fixes

- *(runtime,tui)* Resolve conversation validation, 429 recovery, TUI corruption, large allocs, and loop-guard static analysis

## [0.125.1] - 2026-11-13

### Bug Fixes

- Corrected http errors in react loop

## [0.125.0] - 2026-11-13

### Features

- *(runtimes-react)* Deduplicate collected tool specs by name

## [0.124.0] - 2026-11-13

### Features

- *(providers-model)* Strip strict-endpoint reasoning fields and deduplicate tools

## [0.123.0] - 2026-11-13

### Features

- *(providers-model)* Resolve Gemini native model context limits

## [0.122.1] - 2026-11-13

### Bug Fixes

- Fixed enter key on QMK keyboards and other kbs with burst key layers

## [0.122.0] - 2026-11-13

### Features

- *(systemone-gate)* Add optional TypeSafe System One geis gate

## [0.121.6] - 2026-11-13

### Miscellaneous Tasks

- Removed accidentally committed description of kernel ipc bug

## [0.121.5] - 2026-11-12

### Bug Fixes

- *(capabilities-fs-tools)* Make cached-read note actionable and unblock force

## [0.121.4] - 2026-11-12

### Bug Fixes

- *(capabilities-fs-tools)* Content-hash based read-cache invalidation

## [0.121.3] - 2026-11-12

### Bug Fixes

- *(capabilities-fs-tools)* Pointer-preserving read cache + bounded force re-read

## [0.121.2] - 2026-11-12

### Bug Fixes

- *(ipc)* Make ipc-call/mcp-call error shapes descriptive and refresh stale tests

## [0.121.1] - 2026-11-12

### Documentation

- Add issues.md ledger for known defects and stale test expectations

## [0.121.0] - 2026-11-12

### Features

- *(surfaces-tui)* Bound shell-tool output and add redirect feedback tips

## [0.120.0] - 2026-11-12

### Features

- *(runtimes-prompt)* Assemble loadout-aware tool routing rules into the system prompt

## [0.119.0] - 2026-11-12

### Features

- *(policies-guardrail)* Soft-intercept raw sed/awk/grep file operations with instructional denials and escape hatch

## [0.118.0] - 2026-11-12

### Features

- *(capabilities-fs-tools)* Add file_search tool and 1-based line addressing to read_file; co-locate tool-routing constraints in the bash tool schema

## [0.117.0] - 2026-11-12

### Features

- *(harness)* Fix context-length fetching and reduce read/search inefficiencies

## [0.116.0] - 2026-11-12

### Features

- Added manual override for context window size in config.scm

## [0.115.0] - 2026-11-12

### Features

- Add guile lsp server to devenv

## [0.114.7] - 2026-11-11

### Bug Fixes

- *(capabilities-lsp)* Compare JSON characters with char=?

## [0.114.6] - 2026-11-11

### Miscellaneous Tasks

- *(dev)* Add Guile LSP server to environment

## [0.114.5] - 2026-11-11

### Bug Fixes

- *(agents-subagent)* Simplify nested test bindings

## [0.114.4] - 2026-11-11

### Bug Fixes

- *(capabilities-context-index)* Expect first execution id

## [0.114.3] - 2026-11-11

### Bug Fixes

- *(surfaces-tui)* Stream bounded file reads

## [0.114.2] - 2026-11-11

### Bug Fixes

- *(capabilities-context-index)* Stop persisting exec logs

## [0.114.1] - 2026-11-11

### Bug Fixes

- *(surfaces-tui)* Refresh context limit on model switch

## [0.114.0] - 2026-11-11

### Features

- *(capabilities-fs-tools)* Content-hash line anchors and hash-verified edit_lines

## [0.113.0] - 2026-11-11

### Features

- *(capabilities-bubblewrap)* Sandbox project geis with ocap

## [0.112.1] - 2026-11-11

## [0.112.0] - 2026-11-11

## [0.111.0] - 2026-11-11

## [0.110.0] - 2026-11-11

## [0.109.0] - 2026-11-11

## [0.108.0] - 2026-11-11

## [0.107.0] - 2026-11-11

### Features

- *(surfaces-acp)* Expose badb over the Agent Client Protocol as an editor surface

## [0.106.0] - 2026-11-11

### Features

- *(providers-model)* Add ephemeral prompt-cache markers and native provider profiles

## [0.105.0] - 2026-11-11

### Features

- *(runtimes-geis-loader)* Add manifest capability groundwork

## [0.104.0] - 2026-11-11

### Features

- *(agents-subagent)* Worktree-isolated subagent transactions and sandboxed registries

## [0.103.1] - 2026-11-11

### Bug Fixes

- *(runtimes-react)* Reduce repeated tool-loop noise

## [0.103.0] - 2026-11-11

### Features

- *(surfaces-tui)* Add conversation replay and forks

## [0.102.0] - 2026-11-11

### Features

- *(capabilities-macro)* Wire input macro expansion into TUI chat dispatch

## [0.101.0] - 2026-11-11

### Features

- *(surfaces-tui)* Virtualize viewport, gate syntax highlighting, and keep spinner/scrolling live during agent runs

## [0.100.4] - 2026-11-10

### Miscellaneous Tasks

- Added agent context

## [0.100.3] - 2026-11-10

### Bug Fixes

- Restore harness launch and macro bundle tests

## [0.100.2] - 2026-11-10

## [0.100.1] - 2026-11-10

### Bug Fixes

- *(runtime)* Land todo.md section 5 reliability fixes

## [0.100.0] - 2026-11-10

### Features

- *(runtimes-loop-guard)* Land tool-loop breaking guard bundle with react session seams

## [0.99.0] - 2026-11-10

### Features

- *(capabilities-macro)* Land input macro expansion bundle with plugin, spec, and manifest

## [0.98.0] - 2026-11-10

### Features

- *(capabilities-git)* Feat(git): land capabilities-git bundle with kernel contract spec

## [0.97.0] - 2026-11-10

### Features

- *(dist)* Add zero-install relocatable distribution tooling and pack manifest

## [0.96.2] - 2026-11-10

### Bug Fixes

- *(tui)* Make render mutex recursive and fix string-index type mismatch in segment-line

## [0.96.1] - 2026-11-10

### Bug Fixes

- *(tui)* Suppress compiler warnings, isolate shell stdin/stderr, and harden event loop

## [0.96.0] - 2026-11-10

### Features

- *(lsp)* Land capabilities-lsp bundle with kernel contract spec

## [0.95.0] - 2026-11-09

### Features

- *(tui)* Responsive background agent execution and immediate interrupt handling

## [0.94.2] - 2026-11-09

### Bug Fixes

- *(surfaces-tui)* Restore dynamic environment and workspace outline in system prompt

## [0.94.1] - 2026-11-09

### Bug Fixes

- *(providers-model)* Wrap flat tool specs in OpenAI function envelope

## [0.94.0] - 2026-11-09

### Features

- *(geis)* Support configuring synthesis model via /geis model and badb geis model

## [0.93.5] - 2026-11-09

### Scripts

- Convert harness scripts to native Guile, integrate prek pre-commit runner

## [0.93.4] - 2026-11-09

### Miscellaneous Tasks

- Integrate cl-indentify, format codebase & add guile pre-commit hook

## [0.93.3] - 2026-11-09

### Refactor

- Structural AST cleanup, CLI decomposition & kernel robustness

## [0.93.2] - 2026-11-09

## [0.93.1] - 2026-11-09

### Refactor

- Enforce kernel architectural invariants via test suite & streamline scripts

## [0.93.0] - 2026-11-09

### Features

- *(geasa)* Progressive tool calibration and context-preserving discovery

## [0.92.0] - 2026-11-08

### Features

- Modular TUI surfaces, capability geasa, ecosystem libraries, and DRY refactor

## [0.91.0] - 2026-11-04

### Features

- Dogfood-driven hardening + synthesized runtimes-freshness geasa

## [0.90.5] - 2026-11-04

### Performance

- *(tui)* Stop re-reading the journal on every frame

## [0.90.4] - 2026-11-04

### Refactor

- Structural quality pass over the recently churned files

## [0.90.3] - 2026-11-04

### Performance

- *(tui)* Cache the assembled transcript view for scrolling renders

## [0.90.2] - 2026-11-04

### Bug Fixes

- *(tui)* No-op scroll no longer renders; scroll offsets clamp at the source

## [0.90.1] - 2026-11-04

### Miscellaneous Tasks

- Re-pin surfaces-tui manifest after input rework

## [0.90.0] - 2026-11-04

### Merge

- BADB_DEBUG log gating + guix package with pre-compiled modules (guix-pack-debug)

## [0.89.1] - 2026-11-04

### Bug Fixes

- *(tui)* Correct input-loop tail; map KEY_ENTER in paste bursts

## [0.89.0] - 2026-11-04

### Features

- Guix package for badb with pre-compiled modules

## [0.88.0] - 2026-11-04

### Merge

- Nc paste-burst input API (tui-nc-input)

## [0.87.0] - 2026-11-04

### Features

- *(tui)* Arrows cycle history, paste batching, scheme-mode and startup fixes

## [0.86.0] - 2026-11-04

### Features

- Add paste-burst input API to the notcurses backend

## [0.85.0] - 2026-11-04

### Features

- Gate debug log files behind BADB_DEBUG

## [0.84.1] - 2026-11-04

### Cleanup

- Drop the local guile-quickcheck package

## [0.84.0] - 2026-11-04

### Features

- Guardrail and exec-log hardening against quoting-layer corruption

## [0.83.2] - 2026-11-04

### Testing

- Pin the geis-bundle search-path default contract

## [0.83.1] - 2026-11-04

### Documentation

- *(persona)* Teach the project-local geasa landing convention

## [0.83.0] - 2026-11-04

### Features

- Land synthesized geasa in the project, not the harness tree

## [0.82.0] - 2026-11-03

### Features

- Default synthesis budget raised to 200k tokens

## [0.81.0] - 2026-11-03

### Features

- Thread the synthesis role's provider endpoint into the sema workflow

## [0.80.0] - 2026-11-03

### Features

- Adopt triage-first delegation persona for the main agent

## [0.79.2] - 2026-11-03

### Bug Fixes

- Persona-badb authorizes tool-driven geis synthesis, not just audits

## [0.79.1] - 2026-11-03

## [0.79.0] - 2026-11-03

### Features

- Unlimited ReAct loop by default, optional max-iterations config

## [0.78.4] - 2026-11-03

### Testing

- Add badb self-check fixture for harness checkpoint/rollback verification

## [0.78.3] - 2026-11-03

### Bug Fixes

- Make checkpoint tool verdicts scope-honest

## [0.78.2] - 2026-11-03

### Bug Fixes

- Gate non-protocol message fields behind the strict-OpenAI profile

## [0.78.1] - 2026-11-03

### Bug Fixes

- Strip _badb_* tool provenance keys from outgoing chat requests

## [0.78.0] - 2026-11-03

### Features

- Export synthesis-role provider credentials to the sema subprocess

## [0.77.0] - 2026-11-03

### Features

- Configure the synthesis agent role (provider, model, persona)

## [0.76.0] - 2026-11-03

### Features

- Refactor personae into a family over a shared persona-base geis

## [0.75.0] - 2026-11-03

### Features

- Support strict OpenAI-compatible endpoints (add mistral provider)

## [0.74.0] - 2026-11-03

### Features

- Rewrite persona-badb identity as a capability-aware harness persona

## [0.73.1] - 2026-11-03

### Refactor

- Runtimes-prompt delegates its default identity to persona-badb

## [0.73.0] - 2026-11-03

### Features

- Add persona-badb geasa bundle for the harness persona

## [0.72.0] - 2026-11-03

### Features

- Add agents-coordination geasa bundle for multi-agent coordination

## [0.71.0] - 2026-11-03

### Features

- Add agents-lifecycle geasa bundle for agent start/stop

## [0.70.0] - 2026-11-03

### Features

- Add capabilities-skills geasa bundle for skill loading

## [0.69.0] - 2026-11-03

### Features

- Add runtimes-retry geasa bundle for error recovery with backoff

## [0.68.7] - 2026-11-02

### Miscellaneous Tasks

- Add greppable DEV-DEBT labels at known technical debt sites

## [0.68.6] - 2026-11-02

### Documentation

- Document the geasa bundle workflow in the readme

## [0.68.5] - 2026-11-02

### Cleanup

- Re-pin capabilities-history-tools manifest after comment fix

## [0.68.4] - 2026-11-02

### Documentation

- Refresh readme layout, fix stale path references

## [0.68.3] - 2026-11-02

### Cleanup

- Correct providers-model plugin header and re-pin manifest

## [0.68.2] - 2026-11-02

### Cleanup

- Structural lint fixes and migration artifact removal

## [0.68.1] - 2026-11-02

### Refactor

- *(geasa)* Move bundles to src/geasa and functional code into bundles

## [0.68.0] - 2026-11-01

### Features

- *(geis)* Prompt capture, bundle assembly, and cassette replay

## [0.67.1] - 2026-11-01

### Bug Fixes

- *(geis)* Make runtimes-prompt file-tree test hermetic

## [0.67.0] - 2026-11-01

### Features

- *(geis)* Migrate policies-guardrail, providers-model to bundle layout

## [0.66.0] - 2026-11-01

### Features

- *(geis)* Migrate surfaces-spec-tools, surfaces-theme, surfaces-tui to bundle layout

## [0.65.0] - 2026-11-01

### Features

- *(geis)* Migrate runtimes-react, runtimes-react-stream, runtimes-rollback to bundle layout

## [0.64.0] - 2026-11-01

### Features

- *(geis)* Migrate runtimes-context, runtimes-prompt, runtimes-verify to bundle layout

## [0.63.0] - 2026-11-01

### Features

- *(geis)* Migrate capabilities-mcp, capabilities-mcp-server, capabilities-history-tools to bundle layout

## [0.62.0] - 2026-11-01

### Features

- *(geis)* Migrate foundation-config, foundation-srfi-253 to bundle layout

## [0.61.0] - 2026-11-01

### Features

- *(geis)* Mechanical flat-spec-to-bundle migration helper + two pilot conversions

## [0.60.0] - 2026-11-01

### Features

- *(geis)* Pilot bundle geis/runtimes-synthesis + dual-layout geasa verification

## [0.59.0] - 2026-11-01

### Features

- *(geis)* Bundle packaging — manifests, pack/install CLI, kernel lock/verify hooks

## [0.58.0] - 2026-11-01

### Features

- *(test)* Refactor non-kernel tests to dogfooded geasa contracts

## [0.57.2] - 2026-11-01

### Bug Fixes

- *(tui)* Bound command selector popup indices within visible slice window

## [0.57.1] - 2026-11-01

### Refactor

- Decompose badb into pure microkernel architecture

## [0.57.0] - 2026-11-01

### Features

- *(synthesis)* Sema plugin-synthesis workflow and /geis dynamic harness command

## [0.56.1] - 2026-10-29

### Refactor

- Quality pass, srfi-253 hygiene fix, and harness hardening

## [0.56.0] - 2026-10-29

### Features

- *(tui)* Spec.lock, spec.verify and spec.status kernel tools

## [0.55.0] - 2026-10-29

### Features

- *(spec)* Declarative contract DSL and structured runner

## [0.54.0] - 2026-10-29

### Features

- *(spec)* [**breaking**] Kernel-owned spec gate replaces the Lema oracle seam

## [0.53.0] - 2026-10-29

### Features

- *(srfi)* Vendor SRFI-253 (Checks)

## [0.52.0] - 2026-10-29

### Features

- *(guix)* Package guile-quickcheck 0.1.0

## [0.51.1] - 2026-10-29

### Bug Fixes

- *(prompt)* Exclude .log runtime noise from workspace file tree

## [0.51.0] - 2026-10-29

### Features

- *(lema)* Oracle seam with fail-closed kernel prove gate

## [0.50.0] - 2026-10-29

### Features

- *(tui)* History/summarize kernel tool

## [0.49.0] - 2026-10-29

### Features

- *(mcp)* Filetree MCP server with tree/find/outline/read tools

## [0.48.0] - 2026-10-29

### Features

- *(mcp)* Shared MCP server core for serving over stdio

## [0.47.0] - 2026-10-29

### Features

- *(tui)* Mouse support - wheel scroll, click-to-select, click-to-apply

## [0.46.0] - 2026-10-29

### Features

- *(tui)* Mode-aware terminal cursor styling

## [0.45.1] - 2026-10-29

### Testing

- *(react)* Agentic tool call round-trip through dummy MCP server

## [0.45.0] - 2026-10-29

### Features

- *(guix)* Package sema 2.0.0 prebuilt release binary

## [0.44.0] - 2026-10-29

### Features

- *(theme)* Helix-style theme library with /theme, and alternate-screen exit

## [0.43.2] - 2026-10-29

### Bug Fixes

- *(nc)* Render black backgrounds as transparent terminal background

## [0.43.1] - 2026-10-29

### Bug Fixes

- *(tui,nc)* Crash on bare modifier keys, stale canvas, screen-corrupting stderr

## [0.43.0] - 2026-10-29

### Features

- *(tui)* /usage token report and /resume session persistence

## [0.42.0] - 2026-10-29

### Features

- *(tui)* Modern polish pass + visibility features

## [0.41.1] - 2026-10-29

### Refactor

- Code quality pass over the kernel modules

## [0.41.0] - 2026-10-29

### Features

- *(tui)* Swap ncurses for notcurses via a Guile FFI backend

## [0.40.3] - 2026-10-29

### Testing

- *(e2e)* Hermetic chat round-trip via a static fake model endpoint

## [0.40.2] - 2026-10-29

### Testing

- *(e2e)* Playwright-style TUI suite over tui-test

## [0.40.1] - 2026-10-29

### Bug Fixes

- *(tui)* Consume KEY_RESIZE instead of typing U+019A into the prompt

## [0.40.0] - 2026-10-29

### Features

- *(guix)* Add tui-test CLI to the pinned dev environment

## [0.39.0] - 2026-10-29

### Features

- *(tui)* Message selection moves to Ctrl+Up/Ctrl+Down

## [0.38.2] - 2026-10-29

### Bug Fixes

- *(tui)* Enter key submits again

## [0.38.1] - 2026-10-28

### Bug Fixes

- *(model)* Deterministic curl-based request timeout

## [0.38.0] - 2026-10-28

### Features

- *(tui)* Arrow scrollback and vim-tradition selection keys

## [0.37.0] - 2026-10-28

### Features

- *(tui)* Incremental rendering via a per-message formatted-row cache

## [0.36.0] - 2026-10-28

### Features

- *(react,tui)* Stream the ReAct loop for live output and responsive input

## [0.35.0] - 2026-10-28

### Features

- *(model)* Streamed tool-call assembly and non-SSE completion handling

## [0.34.0] - 2026-10-28

### Features

- *(tui)* Wrapped multi-line input and opaque pop-up rendering

## [0.33.0] - 2026-10-28

### Features

- *(model)* Request timeout so a stalled endpoint cannot freeze the harness

## [0.32.1] - 2026-10-28

### Bug Fixes

- *(context)* Compaction always retains the newest user message

## [0.32.0] - 2026-10-28

### Features

- *(tui)* Harden tool-failure display, error log wiring, /dump, /exit

## [0.31.0] - 2026-10-28

### Features

- *(log)* Append-only error log inside .badb

## [0.30.3] - 2026-10-28

### Bug Fixes

- *(react)* Isolate hook failures so they never abort the agent loop

## [0.30.2] - 2026-10-28

### Bug Fixes

- *(tui)* Keep the hardware cursor at the editing position

## [0.30.1] - 2026-10-28

### Bug Fixes

- *(tui)* Repair frozen first render and escape/control-key dispatch

## [0.30.0] - 2026-10-28

### Features

- *(tui)* Steer and interrupt a running agent from the input

## [0.29.0] - 2026-10-28

### Features

- *(tui)* Automatic model detection and live model menus

## [0.28.0] - 2026-10-28

### Features

- *(tui)* Modern visual overhaul with end-to-end theme integration

## [0.27.0] - 2026-10-28

### Features

- *(tui)* Interactive menus for configuration navigation

## [0.26.0] - 2026-10-28

### Features

- *(tui)* Collapsible messages with thinking collapsed by default

## [0.25.0] - 2026-10-28

### Features

- *(react)* Steering queue for mid-run agent course correction

## [0.24.0] - 2026-10-28

### Features

- *(tui)* Input history with recall and cross-session persistence

## [0.23.0] - 2026-10-28

### Features

- *(tui)* Real editing cursor in the text input

## [0.22.0] - 2026-10-28

### Features

- *(model)* List available models and auto-detect model selection

## [0.21.0] - 2026-10-28

### Features

- *(theme,config)* Add themeable semantic color-role module

## [0.20.3] - 2026-10-28

### Miscellaneous Tasks

- Updated readme.md

## [0.20.2] - 2026-10-28

### Bug Fixes

- *(model)* Fall back to reasoning_content when content is empty string

## [0.20.1] - 2026-10-28

### Bug Fixes

- *(model,react)* Decode bytevector error responses and sanitize tool parameter schemas

## [0.20.0] - 2026-10-27

### Features

- *(config,tui)* Add lmstudio local provider preset and /provider command

## [0.19.41] - 2026-10-27

### Refactor

- *(model,react)* Streamline raw tool arguments parsing with safe-json-parse

## [0.19.40] - 2026-10-27

### Refactor

- *(tui,test)* Extract safe-curs-set and mock HTTP responder generator

## [0.19.39] - 2026-10-27

### Refactor

- *(model,transaction)* Deduplicate message predicates and transaction revert logging

## [0.19.38] - 2026-10-27

### Refactor

- *(react,mcp,guardrail,tui)* Standardize on prop-ref and safe-json-parse

## [0.19.37] - 2026-10-27

### Refactor

- *(util,config,mcp)* Extract and use alist-remove and alist-merge helpers

## [0.19.36] - 2026-10-27

### Refactor

- *(rollback)* Extract resolve-workspace-path helper to deduplicate relative path resolution

## [0.19.35] - 2026-10-27

### Refactor

- *(react)* Extract %empty-tool-parameters and use non-empty-string?

## [0.19.34] - 2026-10-27

### Refactor

- *(react)* Use parenthesized catch handler in invoke-registry-procedure

## [0.19.33] - 2026-10-27

### Refactor

- *(tui)* Extract make-system-turn to deduplicate system prompt initialization

## [0.19.32] - 2026-10-27

### Refactor

- *(model)* Centralize port cleanup in dynamic-wind in model-stream

## [0.19.31] - 2026-10-27

### Refactor

- *(config)* Delegate MCP server env normalization to normalize-env-alist via AST modify

## [0.19.30] - 2026-10-27

### Refactor

- *(model)* Use safe-json-parse, extract parse-single-tool-call, and use ensure-string directly

## [0.19.29] - 2026-10-27

### Refactor

- *(react)* Decompose execute-tool-call into invoke-registry-procedure and log-tool-execution!

## [0.19.28] - 2026-10-27

### Refactor

- *(test)* Extract capture-request mock HTTP helper in model test suite

## [0.19.27] - 2026-10-27

### Refactor

- *(tui)* Extract clamp-selector-index! to deduplicate selector navigation clamping

## [0.19.26] - 2026-10-27

### Refactor

- *(config,registry)* Use ensure-string for uniform identifier normalization

## [0.19.25] - 2026-10-27

### Refactor

- *(mcp)* Deduplicate server lifecycle helpers and normalize identifier strings

## [0.19.24] - 2026-10-27

### Refactor

- *(tui)* Flatten nested let forms in segment-line

## [0.19.23] - 2026-10-27

### Refactor

- *(test)* Flatten cascading let bindings in mcp and model test suites

## [0.19.22] - 2026-10-27

### Refactor

- *(prompt,tui)* Use shared non-empty-string? helper for content presence checks

## [0.19.21] - 2026-10-27

### Refactor

- *(verify,context)* Combine nested lets in gated execution and deduplicate non-empty-string?

## [0.19.20] - 2026-10-27

### Refactor

- *(mcp)* Invert entry and response id checks and use ensure-string

## [0.19.19] - 2026-10-27

### Refactor

- *(rollback)* Invert checkpoint existence check in rollback-to-checkpoint!

## [0.19.18] - 2026-10-27

### Refactor

- *(model)* Invert pair checks in delta and content extraction

## [0.19.17] - 2026-10-27

### Refactor

- *(journal)* Invert negated conditions, eliminate iteration shadowing, and use ensure-string

## [0.19.16] - 2026-10-27

### Refactor

- *(transaction)* Simplify optional fallbacks, delegate to ensure-string, and combine nested lets

## [0.19.15] - 2026-10-27

### Refactor

- *(util)* Invert alist check in prop-ref, export non-empty-string?, and eliminate format-exception shadowing

## [0.19.14] - 2026-10-27

### Refactor

- *(react)* Avoid duplicate registry lookup using cond arrow and eliminate binding shadowing

## [0.19.13] - 2026-10-27

### Refactor

- *(tui)* Extract try-save-config! and refresh-client! helpers, and invert condition in split-thoughts-and-content

## [0.19.12] - 2026-10-27

### Refactor

- *(config)* Flatten nested-let forms, eliminate variable shadowing, and use case dispatch

## [0.19.11] - 2026-10-27

### Style

- *(core)* Deduplicate get-prop and normalize Scheme definition layout across core modules

## [0.19.10] - 2026-10-27

### Refactor

- *(test)* Compact assertion syntax and prune vertical layout bloat across test suite

## [0.19.9] - 2026-10-27

### Refactor

- *(tui)* Deduplicate slash command dispatch, add message helpers, and prune curses layout fat

## [0.19.8] - 2026-10-27

### Refactor

- *(util)* Consolidate split-words, format-exception, and sanitize-for-json into shared util

## [0.19.7] - 2026-10-27

### Performance

- *(tui)* In-place target-cell mutation during model-stream to eliminate O(M) message copying

## [0.19.6] - 2026-10-27

### Performance

- *(registry)* Use hashq operations for symbol keys and move sort out of mutex

## [0.19.5] - 2026-10-27

### Performance

- *(main)* Use static autoload for tui and test runner instead of runtime reflection

## [0.19.4] - 2026-10-27

### Performance

- *(prompt)* Flat accumulator tree traversal to eliminate O(N*D) reversals and appends

## [0.19.3] - 2026-10-27

### Performance

- *(transaction)* Avoid unbounded module chaining in stage-patch, short-circuit entry resolution, and fix merge-metadata equality

## [0.19.2] - 2026-10-27

### Bug Fixes

- *(rollback)* Trigger rollback on #f in with-checkpoint, skip unchanged file writes, and fix directory creation

## [0.19.1] - 2026-10-27

### Performance

- *(mcp)* O(1) inverted tool-index, non-allocating whitespace check, and single-pass json-rpc dispatch

## [0.19.0] - 2026-10-27

### Features

- *(react)* Connect context-compactor to react loop and optimize tool dispatch and turn concatenation

## [0.18.12] - 2026-10-27

### Bug Fixes

- *(context)* Safely serialize vector tool-calls without vector->list array corruption

## [0.18.11] - 2026-10-27

### Performance

- *(config)* Batch merge mcp servers, O(1) overlay lookup, and multi-form read

## [0.18.10] - 2026-10-27

### Performance

- *(model)* Stream curl-http-post over stdin pipeline and eliminate /tmp tempfiles

## [0.18.9] - 2026-10-27

### Performance

- *(context)* Single-pass string-count, word-heuristic scanner, and single-pass split-at

## [0.18.8] - 2026-10-27

### Bug Fixes

- *(verify)* Guarantee port closure, unwind-protect test chdir, and prune unused imports

## [0.18.7] - 2026-10-27

### Bug Fixes

- *(prompt)* Protect working directory mutations in exec-cmd-output using dynamic-wind

## [0.18.6] - 2026-10-27

### Performance

- *(util)* Single-pass prop-ref, fast-path normalize-role, and single-scan env parsing

## [0.18.5] - 2026-10-27

### Performance

- *(guardrail)* Pre-compile regexes, lazify argument stringification, and optimize security level dispatch

## [0.18.4] - 2026-10-27

### Performance

- *(journal)* Optimize next-iteration, journal-latest, and count via seek and prop-ref

## [0.18.3] - 2026-10-27

### Refactor

- *(policy)* Adopt shared (badb util) across context compaction and guardrails

## [0.18.2] - 2026-10-27

### Refactor

- *(core)* Adopt shared (badb util) across mcp, model, and react

## [0.18.1] - 2026-10-27

### Refactor

- *(fs)* Centralize mkdir-p and directory management across journal, config, and rollback

## [0.18.0] - 2026-10-27

### Features

- *(util)* Introduce shared utility module for filesystem, coercion, alist, and json operations

## [0.17.10] - 2026-10-27

### Refactor

- *(context)* Introduce strategy-cascading pipeline and decompose compaction strategies via structural editing

## [0.17.9] - 2026-10-27

### Refactor

- *(guardrail)* Extract dangerous-operation? and mutating-operation? predicates and flatten default-cautious-evaluator

## [0.17.8] - 2026-10-27

### Refactor

- *(react)* Decompose react-step! into determine-step-content, build-assistant-tool-turn, and execute-step-tools

## [0.17.7] - 2026-10-27

### Refactor

- *(config)* Decompose parse-config-sexp and unify parse-mcp-server-spec via structural editing

## [0.17.6] - 2026-10-27

### Refactor

- *(mcp)* Extract exec argument builder and unify server state updates

## [0.17.5] - 2026-10-27

### Refactor

- *(model)* Decompose delta and content extractors into provider helpers

## [0.17.4] - 2026-10-27

### Bug Fixes

- *(tui,model,react)* Harden continuation, error recovery, and reasoning turns

## [0.17.3] - 2026-10-27

### Bug Fixes

- *(react,tui,model)* Preserve LLM transcript across turns, isolate UI tool cards, and validate tool_call_id

## [0.17.2] - 2026-10-27

### Bug Fixes

- *(react,guardrail)* Resolve guardrail naming, reasoning preservation, and tool dispatch

## [0.17.1] - 2026-10-27

### Bug Fixes

- *(tui,mcp,react)* Fix crash when talking to model in TUI

## [0.17.0] - 2026-10-27

### Features

- *(model)* Transparent curl HTTPS fallback when guile-gnutls is not present

## [0.16.0] - 2026-10-27

### Features

- *(model)* Add max-tokens configuration and reasoning extraction for MoE reasoning models

## [0.15.0] - 2026-10-27

### Features

- *(tui)* Modern card delineation, semantic color hierarchy, and token highlighting

## [0.14.2] - 2026-10-27

### Bug Fixes

- *(tui)* Resolve MCP auto-start resolution, status keys, and multi-server commands

## [0.14.1] - 2026-10-27

### Bug Fixes

- *(mcp)* Only flush configuration to disk when source path is bound

## [0.14.0] - 2026-10-27

### Features

- *(config)* Support environment variables in MCP server specifications

## [0.13.0] - 2026-10-27

### Features

- *(verify)* Neuro-symbolic verification engine and autonomous TUI integration

## [0.12.0] - 2026-10-27

### Features

- *(rollback)* Transactional time-travel and workspace checkpoint restoration

## [0.11.0] - 2026-10-27

### Features

- *(prompt)* Dynamic project rules, workspace context, and system prompt assembler

## [0.10.0] - 2026-10-27

### Features

- *(context)* Pluggable context compaction engine with multiple strategies and token estimators

## [0.9.0] - 2026-10-27

### Features

- *(guardrail)* Pluggable policy engine with dynamic levels and System 1 evaluators

## [0.8.0] - 2026-10-27

### Features

- *(react)* Autonomous ReAct tool-calling loop and schema bridge

## [0.7.0] - 2026-10-27

### Features

- *(config)* Hierarchical Guile config system and on-the-fly MCP manager

## [0.6.0] - 2026-10-27

### Features

- *(tui)* Modal prompt switching for shell and Scheme with empty backspace return

## [0.5.0] - 2026-10-27

### Features

- *(tui)* Modern 2020s UI polish with separated thoughts, morphing prompts, and slash selector

## [0.4.0] - 2026-10-27

### Features

- *(cli)* Make 'tui' the default command when badb is run without arguments

## [0.3.0] - 2026-10-27

### Features

- *(tui)* Add '!' shell execution and Scheme REPL evaluation

## [0.2.0] - 2026-10-27

### Features

- *(tui)* Add interactive ncurses terminal user interface

## [0.1.0] - 2026-10-27

### Features

- Implement kernel components and bootstrap test suite

## [0.0.3] - 2026-10-27

### Miscellaneous Tasks

- Added license and readme

## [0.0.2] - 2026-10-27

### Miscellaneous Tasks

- Added basic placeholder program

## [0.0.1] - 2026-10-27

### Miscellaneous Tasks

- Bootstrapped devenv

