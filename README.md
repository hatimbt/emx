# EmX - Emacs on Nix

A home-manager module for Emacs that:
- Uses Nix for Emacs binary and system dependencies
- Uses Elpaca for Elisp packages

## Usage

In your flake.nix:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    emx.url = "github:yourusername/emx.flake";
  };

  outputs = { nixpkgs, home-manager, emx, ... }: {
    homeConfigurations."yourusername" = home-manager.lib.homeManagerConfiguration {
      # Your configuration
      modules = [
        emx.homeManagerModules.default
        {
          programs.emacs-emx = {
            enable = true;
            extraPackages = with pkgs; [ ripgrep fd ];
          };
        }
      ];
    };
  };
}
```

## License

MIT