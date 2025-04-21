# EMX Directory Detection Strategy

## Overview

This document explains the design decision for how EMX detects its root directory during startup and configuration loading.

## Problem

To properly load modules and resources, EMX needs to reliably determine the directory where its configuration files are located. This must work in multiple contexts:

1. When loaded via the Nix shim during normal startup
2. When evaluated directly during development
3. In various initialization and edge cases

The original implementation used a simple approach:

```elisp
(setq emx-directory (file-name-directory buffer-file-name))
```

This worked in some situations but would fail when `buffer-file-name` was nil, resulting in errors like "received nil, but expected stringp".

## Solution

A robust fallback mechanism that tries multiple approaches in sequence:

```elisp
(defvar emx-directory
  (file-name-directory
   (or load-file-name
       buffer-file-name
       (expand-file-name "init.el" user-emacs-directory)))
  "The root directory of EMX configuration.")
```

### Implementation Details

The solution uses three fallback options in order of preference:

1. **`load-file-name`**:
   - Available when a file is being loaded with `load` or similar functions
   - This is the primary case during normal Emacs startup via the Nix shim
   - Most reliable when your init.el is being loaded programmatically

2. **`buffer-file-name`**:
   - Available when the file is being visited in a buffer
   - This is the primary case during development when evaluating the buffer
   - Only evaluated if `load-file-name` is nil

3. **`(expand-file-name "init.el" user-emacs-directory)`**:
   - Final fallback assuming the standard location
   - A conservative approach to prevent nil errors in edge cases
   - Only used if both `load-file-name` and `buffer-file-name` are nil

## Relationship to localPath

This implementation complements the `localPath` option in the Nix home module:

- `localPath` determines where Nix looks for the configuration source
- The directory detection mechanism determines where Emacs looks for modules at runtime

Together, they bridge the gap between the Nix build system and the Emacs runtime environment.

## Benefits

1. **Reliability**: Works in all contexts (loading, evaluation, etc.)
2. **Development-friendly**: Supports both normal use and development workflows
3. **Error-resistant**: Prevents nil/stringp errors by always providing a value
4. **Self-contained**: Doesn't require external configuration
5. **Documentation**: Uses a `defvar` with docstring rather than a simple `setq`

## Testing

To verify this works in different contexts:
- Normal startup via Home Manager: The shim uses `load` → `load-file-name` is used
- Evaluating init.el during development: `load-file-name` is nil → `buffer-file-name` is used
- Edge cases: If both are nil → fallback to user-emacs-directory is used