 # 5. Elisp Configuration Source & Runtime Workflow

 Date: 2025-04-20

 ## Status

 Accepted

 ## Context

 We want to manage the Emacs binary and system dependencies via Nix flakes, but use Elpaca for dynamic Elisp package management. Our configuration must be both reproducible (locking via `flake.lock` and `elpaca-lockfile.el`) and support a rapid development workflow (hot reloading, direct editing of `.el` files without rerunning Nix).

 ## Alternatives Considered

 ### Alternative 1: Use flake directory as `user-emacs-directory`
 - All config, Elpaca repos, builds, and native-comp caches live under the flake.
 - Pros: single git-tracked tree, easy to clone/share.
 - Cons: build artifacts pollute git, require `.gitignore`, slower workflow (must ignore or clean artifacts).

 ### Alternative 2: Flake as config source + external runtime directory
 - Flake tracks only source Elisp and lockfiles.
 - Home Manager writes a tiny `~/.emacs.d/init.el` shim that loads `emacs/init.el` from the flake.
 - Build artifacts (Elpaca package repos, builds, native-comp caches) live under XDG_CACHE (e.g. `~/.cache/emacs/elpaca/`).
 - Pros: flake repo stays pristine, fast dynamic workflow, reproducible locks.
 - Cons: requires a small Home Manager bootstrap and additional indirection via shim.

 ## Decision

 We choose Alternative 2: Maintain the flake directory as the source of truth for Elisp configurations and lockfiles, and use a separate runtime directory for build artifacts. The Home Manager module will:
 
 - Install a `~/.emacs.d/init.el` shim that:
   ```elisp
   (setq user-emacs-directory (expand-file-name ".emacs.d" (getenv "HOME")))
   (load "<FLAKE_PATH>/emacs/init.el")
   ```
 - Track `emacs/init.el` and `emacs/elpaca-lockfile.el` in git.
 - Store Elpaca package repos, builds, and native-comp caches under `~/.cache/emacs/elpaca`.
 - Pin both `flake.lock` and `elpaca-lockfile.el` to guarantee reproducibility.

 ## Consequences

 - Developers can edit `.el` files in `emacs/` and hot-reload immediately without rerunning Nix.
 - The flake repo remains free of build artifacts; only source and lockfiles are committed.
 - Both Nix inputs and Elpaca package snapshots are reproducibly locked.
 - Minor indirection is introduced via the Home Manager shim, but it simplifies day-to-day workflow.