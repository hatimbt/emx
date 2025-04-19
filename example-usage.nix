# Example home-manager configuration using emx
{ config, pkgs, ... }:

{
  # Import the module directly if developing locally
  imports = [ ./nix/home-module.nix ];
  
  # Enable emx
  programs.emacs-emx = {
    enable = true;
    # Optional: Override the default Emacs package
    # package = pkgs.emacs-pgtk;
    
    # Configure tree-sitter grammars
    treesitGrammars = [
      # Option 1: Include all available grammars
      # "all"
      
      # Option 2: Select specific grammars for languages you use
      "rust"
      "nix"
      "python"
    ];
  };
  
  # Other home-manager configuration...
  
  # You can add additional system packages separately if needed
  home.packages = with pkgs; [
    ripgrep      # For better search
    fd           # For faster file finding
    sqlite       # For org-roam and other packages
    imagemagick  # For image processing
  ];
}