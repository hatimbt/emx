# EMX Library Evolution Strategy

## Overview

This document outlines a staged approach for evolving the EMX configuration from a collection of use-package declarations into a proper Emacs Lisp library with a clean abstraction layer. 

## Staged Evolution Approach

### Stage 1: Import Existing Configurations
Begin by organizing existing use-package configurations into modular files (already done):

```elisp
;; emx-programming.el
(use-package lsp-bridge
  :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode))

(use-package paredit
  :ensure t
  :hook emacs-lisp-mode)
```

### Stage 2: Add EMX Options Layer
Introduce customization options that control package behavior without changing the underlying implementation:

```elisp
;; Define customization options
(defgroup emx nil
  "EMX Emacs configuration framework."
  :group 'applications)

(defgroup emx-programming nil
  "EMX programming configuration options."
  :prefix "emx-programming-"
  :group 'emx)

(defcustom emx-programming-enable-lsp t
  "Whether to enable LSP support."
  :type 'boolean
  :group 'emx-programming)

(defcustom emx-programming-lsp-implementation 'lsp-bridge
  "The LSP implementation to use."
  :type '(choice (const :tag "LSP Bridge" lsp-bridge)
                 (const :tag "Eglot" eglot)
                 (const :tag "None" nil))
  :group 'emx-programming)

;; Use options to control existing use-package configs
(when emx-programming-enable-lsp
  (pcase emx-programming-lsp-implementation
    ('lsp-bridge
     (use-package lsp-bridge
       :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                :build (:not compile))
       :init
       (global-lsp-bridge-mode)))
    ('eglot
     (use-package eglot
       :ensure t))))
```

### Stage 3: Full EMX Abstraction Layer
Develop a complete abstraction layer with internal implementation functions and a clean public API:

```elisp
(defun emx-programming-setup ()
  "Set up EMX programming environment."
  (interactive)
  (emx-programming-setup-lsp)
  (emx-programming-setup-lisp-editing)
  (emx-programming-setup-completion))

(defun emx-programming-setup-lsp ()
  "Set up LSP according to EMX configuration."
  (when emx-programming-enable-lsp
    (pcase emx-programming-lsp-implementation
      ('lsp-bridge (emx-programming--setup-lsp-bridge))
      ('eglot (emx-programming--setup-eglot)))))

;; Internal implementation functions
(defun emx-programming--setup-lsp-bridge ()
  "Set up lsp-bridge for LSP support."
  (emx-ensure 'lsp-bridge
              :type 'git
              :host 'github
              :repo "manateelazycat/lsp-bridge"
              :files '(:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build '(:not compile))
  (global-lsp-bridge-mode))
```

## Package Management Abstraction Over Elpaca

Here's an example of how to create a thin abstraction layer over elpaca:

```elisp
(defun emx-ensure (package &rest args)
  "Ensure PACKAGE is installed with ARGS passed to elpaca.
If ARGS is nil, install with default options.
Examples:
  (emx-ensure 'magit)                   ;; Simple package
  (emx-ensure 'lsp-bridge :host 'github :repo \"user/repo\") ;; With options"
  (if (null args)
      ;; Simple case: just the package name
      (elpaca package)
    ;; Complex case: with custom arguments
    (apply #'elpaca (cons package args)))
  ;; Return the package symbol for chaining
  package)

(defun emx-ensure-all (&rest packages)
  "Ensure all PACKAGES are installed.
Example: (emx-ensure-all 'magit 'org 'paredit)"
  (dolist (pkg packages)
    (emx-ensure pkg)))

(defun emx-featurep (feature)
  "Return non-nil if FEATURE is available via elpaca."
  (or (featurep feature)
      (elpaca--queued-p (elpaca-q feature))
      (elpaca-installed-p feature)))
```

### Reasoning Behind Package Abstraction

Creating an abstraction layer over Elpaca provides several important benefits:

1. **Future-proofing**: If EMX ever needs to switch package managers, only update `emx-ensure`, not every package declaration across the codebase.

2. **Enhanced functionality**:
   - Add custom error handling and logging
   - Implement conditional installation based on environment
   - Create hooks for before/after package installation

3. **Integration with EMX customization**:
   - Package installation can respect EMX's feature toggles
   - Add EMX-specific behaviors to package management

4. **Consistency**:
   - All packages are installed following the same patterns
   - Package management behavior can be modified in one place

This approach follows established patterns in other robust Emacs configurations like Doom Emacs (`package!`) and Spacemacs (layers), creating a vocabulary specific to EMX while enabling independent evolution of the package management strategy.

## Core EMX Customization Groups

The following are suggested core customization groups for EMX:

```elisp
(defgroup emx nil
  "EMX Emacs configuration framework."
  :group 'applications)

(defgroup emx-appearance nil
  "EMX appearance customization options."
  :prefix "emx-appearance-"
  :group 'emx)

(defgroup emx-base nil
  "EMX base functionality options."
  :prefix "emx-base-"
  :group 'emx)

(defgroup emx-completion nil
  "EMX completion framework options."
  :prefix "emx-completion-"
  :group 'emx)

(defgroup emx-programming nil
  "EMX programming environment options."
  :prefix "emx-programming-"
  :group 'emx)

(defgroup emx-research nil
  "EMX research and note-taking options."
  :prefix "emx-research-"
  :group 'emx)

(defgroup emx-filesystem nil
  "EMX filesystem management options."
  :prefix "emx-filesystem-"
  :group 'emx)

(defgroup emx-window nil
  "EMX window management options."
  :prefix "emx-window-"
  :group 'emx)
```

## Feature Toggles and Configuration Options

Example of feature toggles and configuration options for each module:

```elisp
;; Base configuration
(defcustom emx-base-enable-xdg-compliance t
  "Whether to enforce XDG compliance for directories."
  :type 'boolean
  :group 'emx-base)

;; Appearance
(defcustom emx-appearance-theme 'modus-vivendi
  "Default theme to use."
  :type 'symbol
  :group 'emx-appearance)

(defcustom emx-appearance-font-family "Iosevka"
  "Default font family."
  :type 'string
  :group 'emx-appearance)

;; Programming
(defcustom emx-programming-languages '(elisp scheme common-lisp)
  "List of programming languages to configure."
  :type '(repeat symbol)
  :group 'emx-programming)
```

## Implementation Strategy

1. **Start with Core Definitions**:
   - Create `emx-core.el` with customization groups and abstraction functions
   - Include this at the beginning of the configuration chain

2. **Gradual Migration**:
   - Begin by wrapping existing use-package declarations with EMX options
   - Don't try to rewrite everything at once

3. **Module Enhancements**:
   - Update one module at a time to use the new abstraction layer
   - Keep compatibility with old code during transition

4. **Testing Process**:
   - Test each module after conversion
   - Ensure that behavior is unchanged or improved

## Example: Migrating a Module

Here's how migration might look for the LSP functionality:

### Before:
```elisp
(use-package lsp-bridge
  :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode))
```

### After (Stage 2):
```elisp
(defcustom emx-programming-enable-lsp t
  "Whether to enable LSP support."
  :type 'boolean
  :group 'emx-programming)

(when emx-programming-enable-lsp
  (use-package lsp-bridge
    :ensure '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile))
    :init
    (global-lsp-bridge-mode)))
```

### After (Stage 3):
```elisp
(defcustom emx-programming-enable-lsp t
  "Whether to enable LSP support."
  :type 'boolean
  :group 'emx-programming)

(defun emx-programming-setup-lsp ()
  "Set up LSP functionality."
  (when emx-programming-enable-lsp
    (emx-ensure 'lsp-bridge
                :type 'git
                :host 'github
                :repo "manateelazycat/lsp-bridge"
                :files '(:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                :build '(:not compile))
    (global-lsp-bridge-mode)))

;; Call this during initialization
(emx-programming-setup-lsp)
```

This approach maintains functionality at each stage while progressively building toward a cleaner architecture.