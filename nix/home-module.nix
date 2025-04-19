{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.emacs-emx;
in
{
  options.programs.emacs-emx = {
    enable = lib.mkEnableOption "Emacx EmX configuration";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.emacs-pgtk;
      description = "The default Emacs derivation to use.";
    };
  };

  config = lib.mkIf cfg.enable {
    # Install Emacs and any additional packages specified by the user
    home.packages = [ cfg.package ] ++ cfg.extraPackages;
    
    # Set up desktop entry for the managed Emacs
    xdg.desktopEntries.emacs-emx = {
      name = "Emacs (EmX)";
      genericName = "Text Editor";
      comment = "Edit text";
      exec = "${cfg.package}/bin/emacs %F";
      terminal = false;
      type = "Application";
      icon = "emacs";
      categories = [ "Development" "TextEditor" ];
      mimeType = [ "text/plain" ];
      startupNotify = true;
    };
  };
}
