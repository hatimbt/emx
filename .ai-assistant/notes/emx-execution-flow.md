# EMX Execution Flow

## Initialization Sequence

The EMX initialization follows this sequence:

1. **Launcher Script** (`emx`):
   - Executes: `emacs --init-directory ~/.config/emx`
   - Sets up Emacs to load from the XDG-compliant location

2. **XDG Shim: Early Init** (`~/.config/emx/early-init.el`):
   - First file loaded by Emacs
   - Sets core EMX directory variables:
     - `emx-source-dir` → `~/src/emx` (or Nix store path)
     - `emx-config-dir` → `~/.config/emx`
     - `emx-data-dir` → `~/.local/share/emx`
     - `emx-cache-dir` → `~/.cache/emx`
   - Loads source early-init.el: `(load (expand-file-name "early-init.el" emx-source-dir))`

3. **Source: Early Init** (`~/src/emx/early-init.el`):
   - Second file loaded
   - Loads EMX early initialization module
   - Path: `(load (expand-file-name "emx/emx-early-init.el" emx-source-dir))`
   - **Critical**: Needs `emx-source-dir` to be set by the shim

4. **Module: EMX Early Init** (`~/src/emx/emx/emx-early-init.el`):
   - Third file loaded
   - Contains actual early initialization code
   - Sets GC thresholds, disables UI elements, etc.

5. **XDG Shim: Main Init** (`~/.config/emx/init.el`):
   - Fourth file loaded (after Emacs UI initialization)
   - Sets/confirms EMX directory variables
   - Sets `user-emacs-directory` to `emx-config-dir`
   - Loads source init.el: `(load (expand-file-name "init.el" emx-source-dir))`

6. **Source: Main Init** (`~/src/emx/init.el`):
   - Fifth file loaded
   - Loads EMX package management module
   - Loads EMX initialization module
   - Loads all other EMX modules
   - **Critical**: Needs `emx-source-dir` to be set by the shim

## Directory Variables

- **`emx-source-dir`**: Location of EMX source code
  - Set by shim to either local path or Nix store path
  - Must be available in early-init.el
  - Critical for locating all EMX module files

- **`emx-config-dir`**: XDG-compliant config directory
  - Set to `~/.config/emx`
  - Used as `user-emacs-directory`

- **`emx-data-dir`**: XDG-compliant data directory
  - Set to `~/.local/share/emx`
  - Used for Elpaca package repositories and builds

- **`emx-cache-dir`**: XDG-compliant cache directory
  - Set to `~/.cache/emx`
  - Used for temporary files, compilation caches, etc.

## Direct Evaluation Support

To support `eval-buffer` during development:

1. Each file needs fallback mechanisms to detect its own location
2. Variables must be declared with `defvar` to avoid overwriting existing values
3. The `or` operator should check for pre-existing values before fallbacks
4. Key paths should be able to be derived from available information

This ensures that EMX can be initialized through the standard launcher or evaluated directly during development.