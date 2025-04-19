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
    
    # Additional system packages for Emacs
    extraPackages = with pkgs; [
      ripgrep      # For better search
      fd           # For faster file finding
      sqlite       # For org-roam and other packages
      imagemagick  # For image processing
    ];
  };
  
  # Other home-manager configuration...
}