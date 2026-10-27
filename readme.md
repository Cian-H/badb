# badb

A lightweight, agentic microharness.

badb is a small harness core designed to grow at runtime rather than
ship a fixed feature set. The intended workflow: when you need a new
capability, you ask an agent to

1. design a TDD contract for the feature,
2. design a workflow that fulfills that contract, and
3. installs the resulting module into the harness

either system-wide or scoped to a single project.

## About the name

In Celtic mythology, **Badb** (pronounced "bave", like "cave" with a
"b") is a shapeshifting deity associated with cunning. She granted
heroes great powers, but only through rigid *geasa* (contracts);
power that was lost if the contract was broken. That is how this
harness is meant to work: the harness can morph into the shape you
require, and grants powerful capabilities through unbreakable contracts.

**Status:** pre-alpha. The harness currently consists of a placeholder
CLI (`OK`).

## Quickstart

The development environment is fully reproducible via [GNU Guix],
pinned to a specific channel revision. Drop into it with:

```sh
./scripts/devenv
```

or, explicitly:

```sh
guix time-machine -C channels.scm -- shell -m manifest.scm
```

Run the CLI:

```sh
./scripts/devenv -- ./scripts/badb            # prints OK
./scripts/devenv -- ./scripts/badb --help
./scripts/devenv -- ./scripts/badb --version
```

Colored output is enabled on terminals and respects the `NO_COLOR`
and `FORCE_COLOR` conventions.

## Layout

```text
├── channels.scm         Guix channel pin (reproducibility)
├── manifest.scm          Guix package manifest for the dev environment
├── license.md            LGPLv3
├── scripts/
│   ├── badb              CLI launcher (Guile)
│   └── devenv            one-command dev environment
└── src/
    └── badb/
        ├── cli.scm       presentation helpers (color support)
        └── main.scm      CLI entry point
```

## Development

Everything runs inside the pinned Guix environment via `./scripts/devenv`.
The harness is written in [GNU Guile]; new modules live under
`src/badb/` following the `(badb ...)` module naming convention.

## License

LGPLv3 — see [license.md](license.md).

[GNU Guix]: https://guix.gnu.org
[GNU Guile]: https://www.gnu.org/software/guile/
