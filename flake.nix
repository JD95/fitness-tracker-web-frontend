{
  description = "fitness trakcer frontend";

  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
      flake-utils.url = "github:numtide/flake-utils";
      spago2nix.url = "github:justinwoo/spago2nix";
      easy-purescript-nix = {
        url = "github:justinwoo/easy-purescript-nix";
        flake = false;
      };
  };

  outputs = { self, spago2nix, flake-utils, ... }@inputs:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
    let
      nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      easy-purescript-nix = import inputs.easy-purescript-nix {pkgs = nixpkgs;};
      pursTools = [
            easy-purescript-nix.purs-0_14_0
        ] ++ (
          spago2nix.packages.${system}.spago2nix_nativeBuildInputs {
            srcs-dhall = [./spago.dhall ./packages.dhall];
          }
        );

    in
    {
      devShells.default = nixpkgs.mkShell {
        packages = pursTools;
      };

      packages.default = nixpkgs.stdenv.mkDerivation {
        name = "fitness-tracker-frontend";
        src = ./.;
        nativeBuildInputs = pursTools;
        unpackPhase = ''
          cp -r $src/src .
          cp $src/style.css .
          cp $src/index.html .
          cp $src/spago.dhall .
          cp $src/packages.dhall .
          install-spago-style
          '';
        buildPhase = ''
          build-spago-style "./src/**/*.purs" 
          purs bundle output/*/*.js -m Main --main Main -o index.js
          '';
        installPhase = ''
          mkdir -p $out
          mv index.js $out/
          mv index.html $out/
          mv style.css $out/
          '';
      };
  });
}
