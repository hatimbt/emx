# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## PROJECT PHILOSOPHY
- Hybrid package management: Nix for binaries, Elpaca for Elisp
- Nix provides stable foundation (Emacs binary, system dependencies)
- Elpaca handles dynamic Elisp packages with its own locking mechanism
- Focus on enabling fast iteration while maintaining reproducibility
- Nix (flake.lock) and Elpaca (lockfiles) both provide dependency locking

## BUILD/LINT/TEST COMMANDS
- Build: `nix build`
- Dev environment: `nix develop`
- Lint Nix files: `nixpkgs-fmt <filename>.nix`
- Check Nix: `nix flake check`

## CODE STYLE GUIDELINES
### Nix
- Use explicit lib references (lib.mkOption, not mkOption)
- Prefer descriptive variable names in camelCase
- Sort attribute sets alphabetically
- Minimize use of with statements

### Emacs Lisp
- Use elpaca for package management (not nix for Elisp packages)
- Follow Emacs Lisp conventions (2 spaces, kebab-case)
- Leverage elpaca's lockfile mechanism for reproducibility

### Module Design
- Keep Nix as non-intrusive as possible to Emacs workflow
- Provide sane defaults but allow easy customization
- Document clearly where Nix ends and Elisp begins