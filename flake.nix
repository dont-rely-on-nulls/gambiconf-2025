{
  description = "Development Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, devenv, nixpkgs, ... }:
    let
      # System types to support.
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      });

      mkPkgs = system: nixpkgsFor."${system}";

      mkEnv = pkgs: pkgs.texlive.combine {
        inherit (pkgs.texlive)
          beamer
          collection-basic
          collection-fontsextra
          collection-fontsrecommended
          collection-langenglish
          collection-langportuguese
          collection-latex
          collection-latexextra
          collection-mathscience
          enumitem
          fancyhdr
          graphics
          graphics-cfg
          graphviz
          hyphen-portuguese
          latexmk
          textcase
          scheme-medium
        ;
      };
    in
    {
      # nix build
      packages = forAllSystems (system:
        let
          pkgs = mkPkgs system;
          texenv = mkEnv pkgs;
          customEmacs = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (
            epkgs:
            with epkgs.melpaPackages;
            [
              citeproc
              htmlize
            ]
            ++ (with epkgs.elpaPackages; [
              org
            ])
          );
        in
        {
          # nix build
          default = pkgs.stdenvNoCC.mkDerivation rec {
            name = "presentation";
            src = ./.;
            buildInputs = with pkgs; [ bash coreutils customEmacs gnumake texenv ];
            phases = ["unpackPhase" "buildPhase" "installPhase"];
            buildPhase = ''
              export PATH="${pkgs.lib.makeBinPath buildInputs}:$PATH"
              export XDG_CACHE_HOME="$(mktemp -d)"
              export HOME="$(mktemp -d)"
              make build
            '';
            installPhase = ''
              mkdir -p $out
              cp presentation.pdf $out
            '';
          };
        }
      );

      defaultPackage = forAllSystems (system: self.packages.${system}.slides);

      devShells = forAllSystems (system:
        let
          pkgs = mkPkgs system;
          texenv = mkEnv pkgs;
          customEmacs = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (
            epkgs:
            with epkgs.melpaPackages;
            [
              citeproc
              htmlize
            ]
            ++ (with epkgs.elpaPackages; [
              org
            ])
          );
        in
        {
          # To be run with:
          #   nix develop .#ci
          # Reduces the number of packages to the bare minimum needed for CI
          ci = pkgs.mkShell {
            buildInputs = with pkgs; [ 
              gnumake
              customEmacs
              graphviz
              texenv
            ];
          };

          # `nix develop`
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              ({ pkgs, lib, ... }: {
                packages = with pkgs; [
                  bash
                  gnumake
                  graphviz
                  texenv
                ];

                languages.texlive.enable = true;
              })
            ];
          };
        });
    };
}
