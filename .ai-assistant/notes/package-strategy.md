# EMX Package Management Strategy

## Overview

EMX employs a hybrid package management strategy that leverages both Nix and Elpaca to provide a balance between reproducibility and flexibility:

- **Nix**: Manages the Emacs binary, system dependencies, and stable core packages
- **Elpaca**: Handles dynamic Elisp packages, especially those that might need customization

## Package Selection Criteria

### Packages Managed by Nix

Packages that are suitable for Nix management:

1. **Packages with C dependencies**:
   - vterm (terminal emulation)
   - pdf-tools (PDF viewing and annotation)
   - Any packages requiring compilation or system libraries

2. **Stable core packages**:
   - magit (Git interface)
   - org (Org mode)
   - use-package (declarative package configuration)

3. **Essential support libraries**:
   - transient
   - with-editor
   - dash
   - s
   - f

4. **Tree-sitter grammars**: Managed through Nix for system-level integration

### Packages Managed by Elpaca

Packages that benefit from Elpaca's dynamic management:

1. **Packages in active development**:
   - lsp-bridge and other rapidly evolving tools
   - Packages that receive frequent updates

2. **Customized packages**:
   - Any packages the user might need to modify or extend
   - Packages with personal configurations or patches

3. **Mode-specific packages**:
   - Language-specific modes and tools
   - Domain-specific packages (research, writing, etc.)

## Implementation

The implementation follows this structure:

1. **Nix Home Manager Module** (`nix/home-module.nix`):
   - Defines `coreEmacsPackages` for packages managed by Nix
   - Creates a unified Emacs package that includes both treesitter grammars and core packages

2. **Elpaca Configuration** (`emx/emx-package.el`):
   - Bootstraps Elpaca package manager
   - Provides use-package integration for declarative package configuration

3. **Module-Specific Packages** (`emx/*.el`):
   - Each module uses use-package + Elpaca to declare its package dependencies
   - Packages not provided by Nix will be automatically installed via Elpaca

## Benefits

This hybrid approach provides several benefits:

1. **Stability**: Core packages are pinned through Nix, providing a stable foundation
2. **Flexibility**: Domain-specific packages can be easily added or modified through Elpaca
3. **Reproducibility**: Both Nix flake.lock and Elpaca's lockfile provide dependency locking
4. **Development-Friendly**: Easy to experiment with new packages without rebuilding Nix environment

## Maintenance

When considering a new package:

1. Ask: "Is this a stable package with system dependencies?"
   - If YES: Add to `coreEmacsPackages` in Nix
   - If NO: Use Elpaca via use-package in the appropriate module file

2. Periodically review packages in `coreEmacsPackages` to ensure they still meet the criteria
   - Packages that need frequent customization might be better moved to Elpaca
   - Stable packages that rarely change might be better moved to Nix

3. When updating, use Nix flake update for core packages and let Elpaca handle dynamic packages