{
  description = "Camulus: OCaml 5 / Ecaml reader built with opam-nix";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
    opam-nix.inputs.opam-repository.url = "github:ocaml/opam-repository";
  };
  outputs = { self, nixpkgs, opam-nix }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      on = opam-nix.lib.${system};
      source = pkgs.lib.cleanSourceWith {
        src = ./.;
        name = "camulus-source";
        filter = path: type:
          baseNameOf path != "_build" && pkgs.lib.cleanSourceFilter path type;
      };
      scope = (on.buildOpamProject {
        inherit pkgs;
        resolveArgs.env.sys-ocaml-version = pkgs.ocaml-ng.ocamlPackages_5_3.ocaml.version;
      } "camulus" source { ocaml-system = "5.3.0"; }).overrideScope
        (final: prev: {
          ecaml = prev.ecaml.overrideAttrs (old: {
            patches = (old.patches or []) ++ [
              ./patches/ecaml-domain-finalizers.patch
              ./patches/ecaml-emacs31-obarray.patch
            ];
          });
          eio_posix = prev.eio_posix.overrideAttrs (old: {
            patches = (old.patches or []) ++ [ ./patches/eio-host-signals.patch ];
          });
        });
    in {
      packages.${system} = {
        default = scope.camulus;
        emacs-camulus = self.lib.mkEmacsPackage pkgs.emacsPackages;
      };
      checks.${system}.reader = scope.camulus.overrideAttrs (old: {
        doCheck = true;
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
          pkgs.gnumake pkgs.python3 pkgs.emacs-nox pkgs.mu
        ];
        checkPhase = ''
          make test BUILD_DIR="$PWD/_build"
        '';
      });
      # An ordinary Emacs package; no OCaml compiler is needed at runtime.
      lib.mkEmacsPackage = epkgs: epkgs.trivialBuild {
        pname = "camulus";
        version = "0.1.0";
        src = "${scope.camulus}/share/emacs/site-lisp/camulus";
        buildPhase = "true";
        installPhase = ''
          mkdir -p "$out/share/emacs/site-lisp"
          cp *.el *.so "$out/share/emacs/site-lisp/"
        '';
      };
      devShells.${system}.default = pkgs.mkShell {
        inputsFrom = [ scope.camulus ];
        packages = [ pkgs.gnumake pkgs.python3 pkgs.emacs-nox pkgs.openssl pkgs.mu ];
      };
    };
}
