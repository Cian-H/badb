# Known Issues & Deferred Items

## `/provider` preset kind filtering (System-1 Geis Gate stretch)

- **Area:** `src/geasa/foundation-config/plugin.scm`, `src/geasa/surfaces-tui/plugin.scm`
- **Detail:** `surfaces-tui` `/provider` listing (`badb-config-switch-provider!`) currently offers every configured `(provider ...)` preset as a main chat-model switch candidate, so a `systemone`-kind preset such as `typesafe` is selectable there.
- **Deferred work:** Add a tolerant `badb-config-provider-kind` reader in `foundation-config` (defaulting to `"chat"`) and filter the `/provider` listing in `surfaces-tui` to `"chat"`-kind presets. Left out to keep `foundation-config` untouched during the System-1 Geis Gate implementation.
