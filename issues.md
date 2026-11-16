# Known Issues & Deferred Items

1. [ ] `/provider` preset kind filtering (System-1 Geis Gate stretch)
2. [x] `api-key` leakage risk in config.scm

## `/provider` preset kind filtering (System-1 Geis Gate stretch)

- **Area:** `src/geasa/foundation-config/plugin.scm`, `src/geasa/surfaces-tui/plugin.scm`
- **Detail:** `surfaces-tui` `/provider` listing (`badb-config-switch-provider!`) currently offers every configured `(provider ...)` preset as a main chat-model switch candidate, so a `systemone`-kind preset such as `typesafe` is selectable there.
- **Deferred work:** Add a tolerant `badb-config-provider-kind` reader in `foundation-config` (defaulting to `"chat"`) and filter the `/provider` listing in `surfaces-tui` to `"chat"`-kind presets. Left out to keep `foundation-config` untouched during the System-1 Geis Gate implementation.

## `api-key` leakage risk in config.scm

- **Area:** `src/geasa/foundation/plugin.scm`
- **Detail:** If a model that requires an API key is loaded then swapped for a model that doesn't, the API kep of the previous model remains in the config.
- **Expected Behaviour:** Model config should only load the variables required for the selected model. No more, no less!
- **Next Steps:**
  - Assess blast radius (this may affect other variables too, it may have only been *noticed* on the API keys)
  - Correct bugs where found to ensure no leakage of variables
