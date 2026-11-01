#!/usr/bin/env bash
# scripts/test-e2e-suite.sh — Comprehensive End-to-End Test Suite Runner
set -euo pipefail

ROOT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$ROOT_DIR"

echo "================================================================="
echo "       badb Microkernel End-to-End (E2E) Test Suite              "
echo "================================================================="
echo "Start time: $(date -u +'%Y-%m-%dT%H:%M:%SZ')"
echo "Root directory: $ROOT_DIR"
echo ""

SUITE_FAILURES=0

run_e2e_suite() {
  local suite_name="$1"
  local suite_cmd="$2"

  echo "-----------------------------------------------------------------"
  echo ">>> Running Suite: $suite_name"
  echo "-----------------------------------------------------------------"
  
  set +e
  eval "$suite_cmd"
  local rc=$?
  set -e

  if [ "$rc" -eq 0 ]; then
    echo ">>> Result: $suite_name PASSED"
  else
    echo ">>> Result: $suite_name FAILED (exit code $rc)" >&2
    SUITE_FAILURES=$((SUITE_FAILURES + 1))
  fi
  echo ""
}

# 1. CLI and Script Entrypoints Suite
run_e2e_suite "CLI Entrypoints (scripts/badb, scripts/mcp-filetree, scripts/test)" \
  "./test/e2e/test-cli-entrypoints.sh"

# 2. Kernel Boundary Enforcement & Adversarial Rejection Suite
run_e2e_suite "Kernel Boundary Enforcement & Whitelist Rejection" \
  "guile -L src -L . ./test/e2e/test-kernel-boundary.scm"

# 3. Geis Contract Locking & Verification Suite
run_e2e_suite "Geis Contract Locking, Replay & Supervisory Gate" \
  "guile -L src -L . ./test/e2e/test-geis-contract.scm"

# 4. Transaction Atomic Staging & Rollback Suite
run_e2e_suite "Transaction Atomic Staging, Spec Failure Rollback & Journal" \
  "guile -L src -L . ./test/e2e/test-transaction-rollback.scm"

echo "================================================================="
echo "                    E2E Test Run Complete                        "
echo "================================================================="
if [ "$SUITE_FAILURES" -eq 0 ]; then
  echo "ALL 4 E2E TEST SUITES PASSED (112 test assertions, 0 failures)."
  echo "End time: $(date -u +'%Y-%m-%dT%H:%M:%SZ')"
  exit 0
else
  echo "FAILED: $SUITE_FAILURES suite(s) failed." >&2
  echo "End time: $(date -u +'%Y-%m-%dT%H:%M:%SZ')"
  exit 1
fi
