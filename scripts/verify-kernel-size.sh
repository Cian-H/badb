#!/usr/bin/env bash
# scripts/verify-kernel-size.sh — Enforce kernel whitelist and LOC budget
set -euo pipefail

KERNEL_DIR="${1:-src/badb/kernel}"
MAX_LOC=1240

# Allowed compulsory kernel modules
ALLOWED_MODULES=(
  "registry.scm"
  "geis-gate.scm"
  "spec-gate.scm"
  "transaction.scm"
  "journal.scm"
  "transport.scm"
  "ipc.scm"
  "digest.scm"
)

echo "=== Checking Kernel Whitelist & LOC in '$KERNEL_DIR' ==="

if [ ! -d "$KERNEL_DIR" ]; then
  echo "FAIL: Kernel directory '$KERNEL_DIR' does not exist." >&2
  exit 1
fi

# 1. Check whitelist: only allowed files present
unauthorized=0
for file in "$KERNEL_DIR"/*; do
  [ -e "$file" ] || continue
  filename=$(basename "$file")
  is_allowed=0
  for allowed in "${ALLOWED_MODULES[@]}"; do
    if [ "$filename" = "$allowed" ]; then
      is_allowed=1
      break
    fi
  done
  if [ "$is_allowed" -eq 0 ]; then
    echo "FAIL: Unauthorized file in kernel directory: $filename" >&2
    unauthorized=$((unauthorized + 1))
  fi
done

if [ "$unauthorized" -gt 0 ]; then
  echo "FAIL: $unauthorized unauthorized file(s) found in $KERNEL_DIR." >&2
  exit 1
fi

# 2. Check compulsory modules presence
missing=0
compulsory=("registry.scm" "transaction.scm" "journal.scm" "digest.scm")
for comp in "${compulsory[@]}"; do
  if [ ! -f "$KERNEL_DIR/$comp" ]; then
    echo "FAIL: Missing compulsory kernel module: $comp" >&2
    missing=$((missing + 1))
  fi
done

# geis-gate.scm (or legacy spec-gate.scm) must be present
if [ ! -f "$KERNEL_DIR/geis-gate.scm" ] && [ ! -f "$KERNEL_DIR/spec-gate.scm" ]; then
  echo "FAIL: Missing compulsory contract supervisor gate: either geis-gate.scm or spec-gate.scm must exist." >&2
  missing=$((missing + 1))
fi

# transport.scm OR ipc.scm must be present
if [ ! -f "$KERNEL_DIR/transport.scm" ] && [ ! -f "$KERNEL_DIR/ipc.scm" ]; then
  echo "FAIL: Missing compulsory message passing module: either transport.scm or ipc.scm must exist." >&2
  missing=$((missing + 1))
fi

if [ "$missing" -gt 0 ]; then
  echo "FAIL: $missing compulsory module(s) missing from $KERNEL_DIR." >&2
  exit 1
fi

# 3. Check LOC budget
total_loc=0
echo "Module LOC breakdown:"
for file in "$KERNEL_DIR"/*.scm; do
  [ -f "$file" ] || continue
  loc=$(wc -l < "$file")
  total_loc=$((total_loc + loc))
  printf "  %-25s : %4d lines\n" "$(basename "$file")" "$loc"
done

echo "----------------------------------------"
printf "  Total Kernel LOC          : %4d lines (Limit: %d)\n" "$total_loc" "$MAX_LOC"

if [ "$total_loc" -gt "$MAX_LOC" ]; then
  echo "FAIL: Kernel total LOC ($total_loc) exceeds budget ($MAX_LOC)." >&2
  exit 1
fi

echo "SUCCESS: Kernel whitelist and LOC limit verified ($total_loc <= $MAX_LOC lines)."
exit 0
