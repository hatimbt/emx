# Using localPath for Development Workflow

## Overview

The `localPath` option in the EMX home-manager module has been validated and works as intended. This option enables a development-friendly workflow while maintaining reproducibility.

## Usage

When configuring the home-manager module:

```nix
programs.emacs-emx = {
  enable = true;
  
  # IMPORTANT: Must be an absolute path, not a tilde path
  localPath = "/home/hatim/src/emx";  
  
  # Alternative using home-manager variables (more portable)
  # localPath = "${config.home.homeDirectory}/src/emx";
}
```

## Common Errors

- **Error**: `error: A definition for option 'programs.emacs-emx.localPath' is not of type 'null or absolute path'`
- **Solution**: Use a full absolute path (`/home/username/...`) rather than a tilde path (`~/...`)

## Benefits

1. **Development Workflow**:
   - Edit Emacs Lisp files directly in your checkout
   - See changes immediately without rebuilding Nix configuration
   - Rapid iteration on configuration

2. **Source vs. Runtime Separation**:
   - Source code remains in your git repository
   - Runtime artifacts (packages, cache) stored in standard locations
   - Clean separation between code and build artifacts

3. **Implementation**:
   - Creates a small shim in `~/.emacs.d/init.el`
   - Shim loads configuration from local path when specified
   - Falls back to immutable store path when not specified

## Relationship to Decision Records

This implements the approach outlined in [DR-0005: Elisp Configuration Source & Runtime Workflow](../decisions/0005-elisp-config-flake-shim.md), which chose "Alternative 2: Flake as config source + external runtime directory" to balance reproducibility with a fast development workflow.

## Validation

This approach has been validated to work correctly, allowing:
- Configuration changes without Nix rebuilds
- Proper loading of all EMX modules
- Reproducible builds when needed