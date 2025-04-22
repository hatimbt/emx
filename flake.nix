{
  description = "Emacs configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ nixpkgs, ... }:
    let
      overlays = import ./overlays.nix { inherit inputs; };
    in
    {
      homeManagerModules.default = import ./nix/home-module.nix;
    };
}

