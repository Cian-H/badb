#!/bin/sh
# scripts/verify-geasa.sh — Lock and verify all subsystem geasa contracts in parallel
#
# Two layouts are discovered:
#   legacy flat:  test/specs/<fid>.geis.scm      -> locked into .badb/specs/<fid>.scm
#   bundle:       src/geasa/<fid>/spec.geis.scm       -> locked into <gate>/src/geasa/<fid>/
# Both are replayed in a disposable kernel subprocess and must emit the
# gate's JSON diagnostics contract with verdict "pass".
set -e
# DEV-DEBT [silent-failure]: set -e + '>/dev/null 2>&1' on the lock loops means a
# stale sha256 pin or failed lock kills the script with ZERO output (stage 2 then
# reports '0 passed, 0 failed'). Validate all manifests up front and keep the
# failure output. Re-pin bundles with compute-geis-sha256-pins after any edit.

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT_DIR"

NUM_JOBS=${BADB_PARALLEL_JOBS:-4}

# 1a. Lock all authored flat specs from test/specs/*.geis.scm into .badb/specs/
mkdir -p .badb/specs
for spec in test/specs/*.geis.scm; do
  [ -f "$spec" ] || continue
  fid=$(basename "$spec" .geis.scm)
  guile --no-auto-compile -L src -L . -c "
(use-modules (badb kernel geis-gate))
(lock-geis \"$spec\" \"$fid\")
" >/dev/null 2>&1
done

# 1b. Lock all geis bundles from src/geasa/*/spec.geis.scm into <gate>/src/geasa/<fid>/
for bundle in src/geasa/*/spec.geis.scm; do
  [ -f "$bundle" ] || continue
  bdir=$(dirname "$bundle")
  fid=$(basename "$bdir")
  guile --no-auto-compile -L src -L . -c "
(use-modules (badb geis bundle) (badb kernel geis-gate))
(let ((errors (geis-manifest-valid? \"$bdir\")))
  (unless (null? errors)
    (format (current-error-port) \"bundle manifest invalid: ~a~%\" errors)
    (exit 1)))
(lock-geis-bundle \"$bdir\" \"$fid\")
" >/dev/null 2>&1
done

# 2. Run parallel verification via xargs
verify_spec() {
  fid="$1"
  out=$(./scripts/badb geis verify "$fid" 2>&1)
  if echo "$out" | grep -q "verdict: PASS"; then
    echo "PASS $fid $out"
  else
    echo "FAIL $fid $out"
  fi
}

export -f verify_spec

# Print header
echo "=== Verifying Subsystem Geasa via Kernel Supervisory Gate ==="

# Flat specs verify through the legacy locked-spec gate
ls test/specs/*.geis.scm 2>/dev/null | sed -e 's|test/specs/||' -e 's|\.geis\.scm$||' | \
  xargs -n 1 -P "$NUM_JOBS" bash -c '
    fid="$1"
    out=$(./scripts/badb geis verify "$fid" 2>&1)
    if echo "$out" | grep -q "verdict: PASS"; then
      summary=$(echo "$out" | grep "summary:" | head -n 1)
      hash=$(echo "$out" | grep "spec-hash:" | head -n 1 | awk "{print \$2}")
      echo "  * PASS: geis $fid ($summary)"
    else
      echo "  * FAIL: geis $fid"
      echo "$out"
      exit 1
    fi
' _

# Bundles verify through the locked-bundle gate (with the locked bundle
# itself on the subprocess load path)
ls -d src/geasa/*/ 2>/dev/null | sed -e 's|src/geasa/||' -e 's|/$||' | \
  xargs -n 1 -P "$NUM_JOBS" bash -c '
    fid="$1"
    out=$(guile --no-auto-compile -L src -L . -c "
(use-modules (badb kernel geis-gate) (ice-9 format))
(define res (verify-locked-geis-bundle \"$fid\" #:timeout 120))
(format #t \"verdict: ~a summary: total=~a passed=~a failed=~a spec-hash: ~a~%\"
        (string-upcase (assoc-ref res (quote verdict)))
        (or (assoc-ref res (quote summary-total)) \"?\")
        (or (assoc-ref res (quote summary-passed)) \"?\")
        (or (assoc-ref res (quote summary-failed)) \"?\")
        (assoc-ref res (quote spec-hash)))
(unless (string=? (assoc-ref res (quote verdict)) \"pass\") (exit 1))
" 2>&1)
    if echo "$out" | grep -q "verdict: PASS"; then
      summary=$(echo "$out" | grep "summary:" | head -n 1)
      echo "  * PASS: geis bundle $fid ($summary)"
    else
      echo "  * FAIL: geis bundle $fid"
      echo "$out"
      exit 1
    fi
' _
