# 4. TreeSit Grammar Implementation Fix

Date: 2025-04-20

## Status

Implemented

## Context

Our initial implementation of tree-sitter grammar support (DR-0002) had a flaw in how we referenced and included grammar packages. After examining the actual implementation in nixpkgs, we found that our approach was incorrect.

The nixpkgs implementation of treesit-grammars requires using either:
1. `treesit-grammars.with-all-grammars` to include all grammars
2. `treesit-grammars.with-grammars (p: [ p.tree-sitter-bash p.tree-sitter-c ])` to include specific grammars

Our implementation was attempting to access non-existent attributes with patterns like `treesit-grammars."with-${name}-grammar"`.

## Decision

We've revised the implementation to correctly use the nixpkgs treesit-grammars package:

1. For the "all" option:
   - Use `treesit-grammars.with-all-grammars` directly

2. For specific grammars:
   - Use `treesit-grammars.with-grammars` with a function that maps our simple language names to the corresponding `tree-sitter-${name}` attributes

3. Handle empty grammar list gracefully by returning null

The implementation maintains the same user-friendly interface from DR-0002 where users specify grammars with simple strings:
```nix
treesitGrammars = [ "rust" "python" "nix" ];
# OR
treesitGrammars = [ "all" ];
```

## Consequences

- Fixed the implementation to correctly use the nixpkgs treesit-grammars package
- Maintained the simple string-based user interface
- Added proper error handling for unknown grammar names
- Ensured grammar packages are correctly bundled with the Emacs package
- Kept the same user experience while fixing the implementation details
