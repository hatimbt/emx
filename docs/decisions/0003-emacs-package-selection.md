# 3. Emacs Package Selection

Date: 2025-04-20

## Status

Implemented

## Context

The module needs to provide a suitable Emacs package for users. We provide the latest stable Emacs PGTK package on Nixpkgs. This can be overridden by the user if a different Emacs binary is needed.

## Alternatives Considered

1. Depend on emacs-overlay ourselves and select the latest binary. This would mean we have a dependancy on emacs-overaly.

## Decision

Select `emacs-pgtk` and allow the package to be swapped.

## Consequences

- The module is more portable and overlay-agnostic
- Users can easily substitute their preferred Emacs package 
- The Elisp configuration may have to support a wider range of Emacs binaries.
