#!/bin/sh
# scripts/verify-geasa.sh — Lock and verify all subsystem geasa contracts in parallel
set -e

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT_DIR"

NUM_JOBS=${BADB_PARALLEL_JOBS:-4}

# 1. Lock all authored specs from test/specs/*.geis.scm into .badb/specs/
mkdir -p .badb/specs
for spec in test/specs/*.geis.scm; do
  [ -f "$spec" ] || continue
  fid=$(basename "$spec" .geis.scm)
  guile --no-auto-compile -L src -L . -c "
(use-modules (badb kernel geis-gate))
(lock-geis \"$spec\" \"$fid\")
" >/dev/null 2>&1
done

# 2. Run parallel verification via xargs
format_str='Kernel replay verdict: PASS for feature '\''%s'\''.'

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

ls test/specs/*.geis.scm | sed -e 's|test/specs/||' -e 's|\.geis\.scm$||' | \
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
