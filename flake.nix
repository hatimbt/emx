{
  description = "Emacs configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ nixpkgs, ... }:
    {
      homeManagerModules.default = import ./nix/home-module.nix;
    };
}

