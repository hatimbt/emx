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
  
  # Convert grammar name strings to actual package references
  getGrammarPackage = name: 
    if name == "all" then
      emacsPackages.treesit-grammars.with-all-grammars
    else
      emacsPackages.treesit-grammars."with-${name}-grammar" or 
        (throw "Unknown treesit grammar: ${name}");
        
  # Get all grammar packages based on user's selection
  grammarPackages = map getGrammarPackage cfg.treesitGrammars;
  
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
