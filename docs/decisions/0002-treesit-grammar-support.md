# 2. TreeSit Grammar Support

Date: 2025-04-19
Updated: 2025-04-20

## Status

Implemented

## Context

Tree-sitter provides syntax highlighting and structural editing in Emacs, requiring language-specific grammar files.

## Alternatives Considered

### Alternative 1: Boolean Flag
```nix
enableTreeSitGrammars = true; # Add all grammars
```

### Alternative 2: Function Option (Original Implementation)
```nix
withTreeSitGrammars = epkgs: [ 
  epkgs.treesit-grammars.with-rust-grammar
];
```

### Alternative 3: String List (Current Implementation)
```nix
treesitGrammars = [ "rust" "nix" "python" ];
# OR use ["all"] for all grammars
```

## Decision

Changed from Alternative 2 to Alternative 3:
- More intuitive and user-friendly interface
- Simpler configuration while maintaining flexibility
- Uses `emacsWithPackages` to bundle grammars directly with Emacs
- Ensures proper visibility of grammar libraries to Emacs

## Consequences

- Users specify languages with simple strings
- More concise configuration
- Improved discoverability with helpful errors for invalid grammar names
- Properly integrates treesitter grammars into Emacs package
