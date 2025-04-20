{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.emacs-emx;
  
  # Get the Emacs package set for the selected Emacs package
  emacsPackages = pkgs.emacsPackagesFor cfg.package;
  
  # Process tree-sitter grammar selections based on user-provided grammar names
  treesitGrammars =
    if builtins.elem "all" cfg.treesitGrammars then
      # Include all available grammars when the special "all" name is specified
      emacsPackages.treesit-grammars.with-all-grammars
    else if cfg.treesitGrammars != [] then
      # For specific grammar selections, map our simple names to package references
      emacsPackages.treesit-grammars.with-grammars (grammars:
        map (name:
          grammars."tree-sitter-${name}" or
            (throw "Unknown treesit grammar: ${name}")
        ) cfg.treesitGrammars
      )
    else null;

  # Define grammar packages for inclusion in the final Emacs package
  grammarPackages = if treesitGrammars != null then [ treesitGrammars ] else [];
  
  # Define core Emacs packages to be managed by Nix
  # These should be stable packages or those with C dependencies
  coreEmacsPackages = epkgs: [
    # Core packages with C dependencies
    epkgs.vterm
    epkgs.pdf-tools

    # Stable packages unlikely to need customization
    epkgs.magit
    epkgs.org
    epkgs.use-package

    # Additional stable packages
    epkgs.with-editor
    epkgs.transient
    epkgs.dash
    epkgs.s
    epkgs.f
  ];

  # Create the final Emacs package with grammars and core packages included
  finalEmacsPackage = emacsPackages.emacsWithPackages (epkgs:
    grammarPackages ++ (coreEmacsPackages epkgs));

  # Config source location
  src = if cfg.localPath != null then "${cfg.localPath}/emacs" else "${../emacs}";
in
{
  options.programs.emacs-emx = {
    enable = lib.mkEnableOption "Emacs EmX configuration";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.emacs-pgtk;
      description = "The default Emacs derivation to use.";
    };
    
    treesitGrammars = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      example = [ "rust" "python" "nix" ];
      description = ''
        List of tree-sitter grammar languages to include.
        Use "all" to include all available grammars.
        Example: [ "rust" "python" "nix" ]
        For all grammars: [ "all" ]
      '';
    };
    localPath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Local path to your flake checkout (containing the "emacs" folder).
        If set, Emacs will load config from this directory at runtime,
        bypassing the immutable store copy.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Install the Emacs package with treesitter grammars included
    home.packages = [ finalEmacsPackage ];
    
    # Set up desktop entry for the managed Emacs
    xdg.desktopEntries.emacs-emx = {
      name = "Emacs (EmX)";
      genericName = "Text Editor";
      comment = "Edit text";
      exec = "${finalEmacsPackage}/bin/emacs %F";
      terminal = false;
      type = "Application";
      icon = "emacs";
      categories = [ "Development" "TextEditor" ];
      mimeType = [ "text/plain" ];
      startupNotify = true;
    };

    # Shim to load Emacs config from either local path or embedded flake source
    home.file.".emacs.d/init.el" = {
      text = ''
        (setq user-emacs-directory (expand-file-name ".emacs.d" (getenv "HOME")))
        (load "${src}/init.el")
      '';
    };
  };

}
