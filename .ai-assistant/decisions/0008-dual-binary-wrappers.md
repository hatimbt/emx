# 8. Flag-Based Development Mode for EMX

Date: 2025-04-21

## Status

Draft - Under Consideration

## Context

EMX supports two primary usage modes:

1. **Stable mode**: Using the immutable Nix store version of the configuration
2. **Development mode**: Using a local checkout for rapid iteration (via `localPath`)

Currently, these modes are both accessed through a single `emx` command, with the behavior determined by whether `localPath` is set in the Home Manager configuration. This approach has several limitations:

- Unclear which mode is active without checking Home Manager configuration
- Potential for accidental use of development code in production
- No visual or command-line distinction between stable and development environments
- Limited ability to customize behaviors specifically for development

## Alternatives Considered

### Alternative A: Single Binary with Flag

```bash
emx --dev    # Development mode
emx          # Stable mode
```

**Pros:**
- Simple, single command with consistent base name
- Flag clearly indicates intent without remembering different commands
- Easier to document and discover
- Could later add additional flags for other modes
- More flexibility to add other options later

**Cons:**
- Requires parsing command-line arguments
- Slightly more complex implementation
- User might not know the flag exists without documentation
- Requires handling unknown flags gracefully

### Alternative B: Dual Binary Wrappers

```bash
emx     # Stable mode, always uses Nix store
emxx    # Development mode, requires localPath
```

**Pros:**
- Clear distinction through command name
- Explicit intent at invocation time
- Can have separate configuration directories
- Consistent with the "top-down flexibility, bottom-up reliability" philosophy
- No need to parse command-line arguments

**Cons:**
- Two separate commands to remember
- Slight duplication in Home Manager configuration
- Introduces a naming convention that might not scale well for other modes
- Potentially confusing for new users

### Alternative C: Environment Variable Control

```bash
EMX_DEV=1 emx   # Development mode
emx             # Stable mode
```

**Pros:**
- Single command
- Flexible control
- Works well in scripts and makefiles

**Cons:**
- Less discoverable
- Easy to forget the environment variable
- Requires shell-level knowledge
- Less visible in process listings

## In-Depth Analysis of Option A (Flag-based)

The `--dev` flag approach offers several interesting advantages worth exploring:

```nix
(pkgs.writeShellScriptBin "emx" ''
  # Parse arguments
  DEV_MODE=0
  NEW_ARGS=()
  
  for arg in "$@"; do
    if [ "$arg" = "--dev" ]; then
      DEV_MODE=1
    else
      NEW_ARGS+=("$arg")
    fi
  done
  
  # Choose config directory based on mode
  if [ "$DEV_MODE" = "1" ]; then
    # Development mode requires localPath
    if [ -z "${if cfg.localPath != null then cfg.localPath else ""}" ]; then
      echo "Error: localPath must be set in configuration to use --dev flag"
      exit 1
    fi
    INIT_DIR="$HOME/.config/emx-dev"
  else
    INIT_DIR="$HOME/.config/emx"
  fi
  
  # Launch Emacs with appropriate configuration
  ${finalEmacsPackage}/bin/emacs --init-directory "$INIT_DIR" "''${NEW_ARGS[@]}"
'')
```

This implementation:
- Provides a clear command-line flag for development mode
- Uses separate configuration directories for each mode
- Validates that localPath is set for development mode
- Removes the flag before passing arguments to Emacs

Additional benefits:
- Could later add other flags like `--profile=<name>` for different configurations
- Single command maintains simplicity for users
- Flag is self-documenting when seen in process listings
- Help text could be added to explain available options

## Decision

We will implement **Alternative A: Single Binary with Flag**, creating a single `emx` command with an optional `--dev` flag:

```bash
emx         # Stable mode (default)
emx --dev   # Development mode
```

The implementation will:

1. Create a single command wrapper that accepts the `--dev` flag
2. Default to stable mode (Nix store configuration) when no flag is provided
3. Use development mode (localPath) only when the `--dev` flag is explicitly provided
4. Maintain separate configuration directories for each mode
5. Validate that localPath is configured when using development mode

This approach provides a clear, explicit way to switch between environments while keeping the command interface simple and consistent. The default behavior prioritizes stability and reproducibility, requiring users to consciously opt into development mode.

## Consequences

The flag-based approach will provide several benefits:

- Users have explicit control over which environment they're using
- The stable configuration is the default, prioritizing reliability
- Development mode requires conscious opt-in with a clear flag
- Each environment has separate configuration directories, preventing conflicts
- The single command maintains simplicity while the flag provides flexibility
- Future extensions (e.g., different profiles) can be added as additional flags
- The implementation aligns with the philosophy of balancing reproducibility and flexibility

This approach should satisfy both stability needs and development workflows while keeping the interface simple and discoverable.