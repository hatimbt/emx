# 6. User-Emacs-Directory Strategy

Date: 2025-04-20

## Status

Draft - Under Consideration

## Context

The `user-emacs-directory` is where Emacs stores configuration files, packages, and state data. For a Nix-managed Emacs configuration like EmX, we need to decide how to handle this directory in relation to our flake structure.

Our hybrid approach (Nix for binaries, Elpaca for Elisp) complicates this decision. We need to determine:

1. Where configuration files should live
2. Where Elpaca should install packages
3. Where Emacs should store state (history, bookmarks, etc.)
4. How reproducible vs. customizable the setup should be

## Alternatives Considered

### Alternative A: Use Flake Directory as user-emacs-directory
```elisp
;; Set user-emacs-directory to flake location
(setq user-emacs-directory "/path/to/emx/")
```

**Pros:**
- Complete configuration reproducibility
- All configuration lives in version control
- Clear mapping between code and runtime environment

**Cons:**
- State files (history, etc.) would be in the repo
- Would need extensive .gitignore rules
- Non-standard location might confuse Emacs tools
- Requires absolute path configuration

### Alternative B: Keep Traditional user-emacs-directory
```elisp
;; Use default ~/.emacs.d or ~/.config/emacs (XDG)
;; Just copy/generate init files there
```

**Pros:**
- Works with Emacs' expectations
- Better compatibility with existing tools
- Familiar to users coming from traditional Emacs
- Separates volatile state from configuration

**Cons:**
- Less reproducible
- Harder to version control user customizations
- Configuration split between Nix and user directory

### Alternative C: Hybrid with XDG Compliance
```elisp
;; Configuration from flake
;; State in XDG directories
(setq user-emacs-directory "~/.config/emacs/")
(setq elpaca-directory "~/.local/share/emacs/elpaca/")
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t)))
```

**Pros:**
- Follows XDG specification
- Separates config, data, and cache
- Balance of reproducibility and state management
- More organized directory structure

**Cons:**
- More complex setup
- Requires careful configuration
- Some Emacs packages may not respect these paths

## Decision

[Decision pending - preliminary thoughts]

We're leaning toward **Alternative C: Hybrid with XDG Compliance** for these reasons:

1. XDG compliance is increasingly the standard on Linux systems
2. It properly separates configuration, state, and cache
3. It works well with our hybrid Nix/Elpaca approach
4. It offers a balance between reproducibility and customization

This approach would mean:
- Configuration template lives in the flake
- User data properly stored in XDG directories
- Elpaca packages in appropriate data directory
- Cache files kept separate from configuration

## Consequences

[To be determined after final decision]

Potential considerations:
- Need to ensure proper XDG environment variables are set
- May need to modify package configurations to respect XDG paths
- Need strategy for user customizations that preserves reproducibility
- Should establish clear boundaries between Nix-managed and user-managed files