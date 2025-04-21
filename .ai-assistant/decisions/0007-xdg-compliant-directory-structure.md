# 7. XDG-Compliant Directory Structure

Date: 2025-04-21

## Status

Draft - Under Consideration

## Context

The EMX configuration currently uses the traditional `~/.emacs.d` location for its shims, while the actual source code lives either in the Nix store (immutable case) or in a local development directory specified by `localPath` (e.g., `~/src/emx`). 

We want to decouple EMX from the standard `.emacs.d` location to:
1. Make it more self-contained
2. Follow XDG Base Directory Specification
3. Allow it to coexist with other Emacs configurations
4. Clearly identify it as a distinct distribution

This requires rethinking how we handle:
- Directory detection
- Shim implementation
- Separation of source, config, data, and cache
- Development workflow

## Alternatives Considered

### Alternative A: Standard XDG Paths with Environment Variables

```nix
# Nix module creates a launcher that sets:
export EMX_SOURCE_DIR="/path/to/source"
export EMX_CONFIG_DIR="$HOME/.config/emx"
export EMX_DATA_DIR="$HOME/.local/share/emx"
export EMX_CACHE_DIR="$HOME/.cache/emx"

# Shim simply loads from source:
(load (getenv "EMX_SOURCE_DIR")/init.el)

# Source init.el uses env variables:
(setq emx-source-dir (getenv "EMX_SOURCE_DIR"))
(setq emx-config-dir (getenv "EMX_CONFIG_DIR"))
```

**Pros:**
- Clear separation of directories
- Explicit control through environment variables
- Easy to override for testing
- Self-documenting approach (clear variable names)

**Cons:**
- Fails if Emacs is launched without the environment variables
- External dependency not intrinsic to the code
- Less self-contained (relies on variables being set)
- Harder to debug if variables are missing

### Alternative B: XDG Paths with Shim Variables

```nix
# Nix module creates a shim:
xdg.configFile."emx/init.el".text = ''
  (setq emx-source-dir "${sourceDir}")
  (setq emx-config-dir "${configDir}")
  (setq emx-data-dir "${dataDir}")
  (setq emx-cache-dir "${cacheDir}")
  (load (expand-file-name "init.el" emx-source-dir))
'';

# Source init.el uses these pre-set variables:
(unless (boundp 'emx-config-dir)
  (setq emx-config-dir "~/.config/emx/"))
```

**Pros:**
- Variables are set reliably by the shim
- No dependency on environment variables
- More resistant to user environment differences
- Cleaner separation of concerns

**Cons:**
- Less flexible for debugging (hardcoded in shim)
- Harder to override temporarily
- Less transparent (variables set implicitly)
- Still fails if init.el is evaluated directly

### Alternative C: Hybrid Approach with Fallbacks

```nix
# Nix module creates a shim:
xdg.configFile."emx/init.el".text = ''
  (setq emx-source-dir "${sourceDir}")
  (setq emx-config-dir "${configDir}")
  (load (expand-file-name "init.el" emx-source-dir))
'';

# Source init.el has fallbacks:
(defvar emx-source-dir
  (or (bound-and-true-p emx-source-dir) ; Set by shim
      (file-name-directory            ; Self-detection fallback
        (or load-file-name buffer-file-name)))
  "Source directory of EMX configuration.")

(defvar emx-config-dir
  (or (bound-and-true-p emx-config-dir) ; Set by shim
      (expand-file-name "~/.config/emx/"))
  "Directory for EMX configuration files.")
```

**Pros:**
- Works in all contexts (with or without shim)
- Preserves development workflow
- Robust against different launch methods
- Self-contained fallbacks
- Clear separation of source from runtime

**Cons:**
- More complex implementation
- Potentially confusing with multiple paths
- Requires careful documentation
- Slightly more code to maintain

## Decision

[Decision pending - preliminary thoughts]

We're leaning toward **Alternative C: Hybrid Approach with Fallbacks** for these reasons:

1. It preserves the development workflow with `localPath`
2. It works reliably in all contexts (with or without shim)
3. It achieves XDG compliance while maintaining flexibility
4. It has graceful fallbacks for direct evaluation

The implementation would:
- Create a launcher script that sets up the XDG environment
- Use `~/.config/emx` for configuration files
- Use `~/.local/share/emx` for data files (including Elpaca)
- Use `~/.cache/emx` for cache files
- Set explicit variables in the shim that loads the source
- Provide fallbacks in the source for direct evaluation
- Clearly separate source code from runtime files

## Consequences

[To be determined after final decision]

Potential considerations:
- Need to ensure proper directory creation
- Must update Elpaca configuration to use the new directories
- Should consider no-littering integration for other paths
- Need to document the recommended usage (launcher vs direct)
- Should establish migration path for existing users
- Must thoroughly test both development and deployment scenarios