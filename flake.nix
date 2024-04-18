{
  description = "anilist-behind";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    gleam2nix = {
      url = "github:mtoohey31/gleam2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, utils, gleam2nix }: {
    overlays = rec {
      expects-gleam2nix = final: _: {
        anilist-behind = final.buildGleamProgram {
          bin-name = "anilist-behind";
          src = builtins.path { path = ./.; name = "anilist-behind-src"; };
          doCheck = false;
        };
      };
      default = nixpkgs.lib.composeManyExtensions [
        gleam2nix.overlays.default
        expects-gleam2nix
      ];
    };
  } // utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        overlays = [ self.overlays.default ];
        inherit system;
      };
      inherit (pkgs) anilist-behind mkShell stdenvNoCC;
    in
    {
      packages.default = anilist-behind;

      devShells.default = mkShell.override { stdenv = stdenvNoCC; } {
        inputsFrom = [ anilist-behind ];
      };
    });
}
