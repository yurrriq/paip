{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.05";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        overlays = [
          (
            final: prev: {
              # Use a patched noweb that includes autodefs.lisp
              noweb = prev.noweb.overrideAttrs (oldAttrs: {
                src = prev.fetchFromGitHub {
                  owner = "yurrriq";
                  repo = "noweb";
                  rev = "2eed27256016e1cf28324480549060643404f255";
                  hash = "sha256-gZjmx11FcoSfrPh+Un0MkYW9qvn/1ExDkeEg9X+nOyE=";
                };
              });
            }
          )
        ];
        system = "x86_64-linux";
      };
    in
    {
      # h/t https://github.com/eugeneia/athens/blob/cc9d456e/default.nix
      defaultPackage.x86_64-linux = pkgs.stdenv.mkDerivation {
        pname = "paip";
        version = builtins.readFile ./VERSION;
        src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

        preBuild = ''
          export XDG_CACHE_HOME="$TMP/.cache"
          export CL_SOURCE_REGISTRY="(:source-registry :ignore-inherited-configuration)"
        '';

        installPhase = ''
          mkdir -p $out
          cp tex/paip.pdf $out/
        '';
      };

      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixpkgs-fmt
          noweb
          python3Packages.pygments
          rlwrap
          sbcl
          (
            texlive.combine {
              inherit noweb;
              inherit (texlive) scheme-small
                catchfile
                framed
                fvextra
                hardwrap
                latexmk
                mathpazo
                minted
                palatino
                titlesec
                tufte-latex
                xetex
                xstring
                ;
            }
          )
        ];
      };
    };
}
