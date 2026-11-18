# Geasa Manager & Updater E2E: /geasa modal, local file:// git fixture,
# tab switching, status badges, check (c), policy cycle (m), toggle (Space), close (q).
. "$(dirname "$0")/lib.sh"

# Build an isolated local file:// git repo fixture with a minimal geis bundle.
REMOTE_REPO="$E2E_SCRATCH/remote-repo"
BUNDLE_REL="geasa/capabilities-demo"
mkdir -p "$REMOTE_REPO/$BUNDLE_REL" "$E2E_SCRATCH/user-geasa" "$E2E_SCRATCH/xdg-config" "$E2E_SCRATCH/repo-cache"

cat >"$REMOTE_REPO/$BUNDLE_REL/plugin.scm" <<'EOF'
(define-module (geasa capabilities-demo plugin)
  #:export (demo-version))
(define (demo-version) "v0")
EOF

cat >"$REMOTE_REPO/$BUNDLE_REL/spec.geis.scm" <<'EOF'
(define-module (test capabilities-demo-spec)
  #:use-module (srfi srfi-64)
  #:use-module (badb spec))
(spec-begin "capabilities-demo")
(test-group "demo"
  (test-assert "ok" #t))
(spec-end)
(spec-exit)
EOF

cat >"$REMOTE_REPO/$BUNDLE_REL/manifest.scm" <<EOF
((bundle-version . 1)
 (feature-id . "capabilities-demo")
 (version . "2026.11.18.0")
 (spec . "spec.geis.scm")
 (plugin (module geasa capabilities-demo plugin) (file . "plugin.scm"))
 (source (type . git)
         (url . "file://$REMOTE_REPO")
         (ref . "main")
         (path . "$BUNDLE_REL")))
EOF

"$E2E_ROOT/scripts/badb" geis pin "$REMOTE_REPO/$BUNDLE_REL" >/dev/null 2>&1 || true

(
  cd "$REMOTE_REPO"
  git init -q -b main
  git config user.email "e2e@badb.local"
  git config user.name "badb e2e"
  git add -A
  git commit -q -m "initial demo geis"
)

# Install v0 into the scratch project store before launching TUI.
(
  cd "$E2E_SCRATCH"
  GUILE_LOAD_PATH="$E2E_ROOT/src${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}" \
  BADB_REPO_CACHE_DIR="$E2E_SCRATCH/repo-cache" \
  BADB_USER_GEIS_DIR="$E2E_SCRATCH/user-geasa" \
  XDG_CONFIG_HOME="$E2E_SCRATCH/xdg-config" \
    "$E2E_ROOT/scripts/badb" geis --project add "file://$REMOTE_REPO#$BUNDLE_REL@main" >/dev/null
)

# Bump remote repo fixture to next CalVer patch so update check detects a pending update.
cat >"$REMOTE_REPO/$BUNDLE_REL/plugin.scm" <<'EOF'
(define-module (geasa capabilities-demo plugin)
  #:export (demo-version))
(define (demo-version) "v1")
EOF
"$E2E_ROOT/scripts/badb" geis pin "$REMOTE_REPO/$BUNDLE_REL" >/dev/null
NEXT_VER=$(guile -c "(display (assoc-ref (call-with-input-file \"$REMOTE_REPO/$BUNDLE_REL/manifest.scm\" read) 'version))")
(
  cd "$REMOTE_REPO"
  git add -A
  git commit -q -m "bump demo geis to $NEXT_VER"
)

E2E_TUI_ENV_ARGS="--env BADB_REPO_CACHE_DIR=$E2E_SCRATCH/repo-cache --env BADB_USER_GEIS_DIR=$E2E_SCRATCH/user-geasa --env XDG_CONFIG_HOME=$E2E_SCRATCH/xdg-config"

launch_tui
expect_visible "badb microharness" 20000

type_text "/geis"
expect_visible "↑↓:choose · tab:complete · enter:run · esc:dismiss" 5000
submit_line ""

expect_visible "Geis Manager" 5000
expect_visible "Policy:" 5000
expect_visible "Status" 5000
expect_visible "Project" 5000

# Switch to Project scope tab where capabilities-demo is installed.
press Tab
expect_visible "capabilities-demo" 5000

# Trigger force check against local file:// git fixture.
press c
expect_visible "Checked project updates" 10000
expect_visible "$NEXT_VER" 5000

# Cycle update policy (notify -> auto).
press m
expect_visible "Update policy: auto" 5000

# Toggle non-core geis inactive.
press Space
expect_visible "Disabled capabilities-demo" 5000

# Close modal cleanly.
press q
expect_hidden "Geis Manager" 3000

finish
