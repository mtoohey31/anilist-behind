{
  description = "anilist-behind";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: {
    overlays.default = final: _: {
      # TODO: Use checksums in manifest to avoid having to commit packages.
      anilist-behind = final.callPackage
        ({ erlang, gleam, makeWrapper, rebar3, stdenvNoCC }: stdenvNoCC.mkDerivation rec {
          pname = "anilist-behind";
          inherit (builtins.fromTOML (builtins.readFile (src + "/gleam.toml")))
            version;
          src = builtins.path { path = ./.; name = "anilist-behind-src"; };
          buildInputs = [ erlang ];
          nativeBuildInputs = [ gleam makeWrapper rebar3 ];
          buildPhase = ''
            runHook preBuild

            gleam export erlang-shipment

            runHook postBuild
          '';
          installPhase = ''
            runHook preInstall

            mkdir -p $out/{bin,share}
            cp -r build/erlang-shipment $out/share/erlang
            makeWrapper $out/share/erlang/entrypoint.sh \
              $out/bin/anilist-behind --add-flags run \
              --prefix PATH : ${erlang}/bin

            runHook postInstall
          '';
        })
        { };
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
