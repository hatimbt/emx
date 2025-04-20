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
  
  # Create the final Emacs package with grammars included
  finalEmacsPackage = emacsPackages.emacsWithPackages (_: grammarPackages);
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
  };
}
