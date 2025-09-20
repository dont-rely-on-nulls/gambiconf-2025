{
  description = "Development Environment";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
  };

  outputs = inputs@{ self, devenv, flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ 
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = { pkgs, system, ... }: 
        let
          texenv = pkgs.texlive.combine {
            inherit (pkgs.texlive)
              beamer
              beamertheme-simpleplus
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
              fontawesome
              graphics
              graphics-cfg
              graphviz
              hyphen-portuguese
              latexmk
              textcase
              scheme-medium
            ;
          };

          customEmacs = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages (
            epkgs:
            (with epkgs.melpaPackages; 
              [ citeproc htmlize ]
              ++ (with epkgs.elpaPackages; [ org ])
            )
          );
        in
        {
          # This sets `pkgs` to a nixpkgs with allowUnfree option set.
          _module.args.pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          # nix build
          packages = {
            default = pkgs.stdenvNoCC.mkDerivation rec {
              name = "presentation";
              src = ./.;
              buildInputs = with pkgs; [ bash coreutils customEmacs gnumake texenv ];
              phases = ["unpackPhase" "buildPhase" "installPhase"];
              buildPhase = ''
                export PATH="${pkgs.lib.makeBinPath buildInputs}:$PATH"
                export XDG_CACHE_HOME="$(mktemp -d)"
                export HOME="$(mktemp -d)"
                export SOURCE_DATE_EPOCH="${toString self.lastModified}"
                make build
              '';
              installPhase = ''
                mkdir -p $out
                cp presentation.pdf $out
              '';
            };
          };

          # nix develop
          devShells = {
            ci = pkgs.mkShell {
              buildInputs = with pkgs; [ gnumake customEmacs graphviz texenv ];
            };

            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                ({ pkgs, lib, ... }: {
                  packages = with pkgs; [ bash gnumake graphviz texenv ];

                  env = {
                    SOURCE_DATE_EPOCH="${toString self.lastModified}";
                  };

                  languages.texlive.enable = true;
                })
              ];
            };
          };
        };

        flake = {};
    };
}
