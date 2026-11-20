{
  description = "Camulus: OCaml 5 / Ecaml reader built with opam-nix";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
    opam-nix.inputs.opam-repository.url = "github:ocaml/opam-repository/7b5e9a2b084fe7ff750c4a9449289065ef9f9216";
  };
  outputs = { self, nixpkgs, opam-nix }:
    let
      inherit (nixpkgs) lib;
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forSystems = lib.genAttrs systems;
      source = lib.cleanSourceWith {
        src = ./.;
        name = "camulus-source";
        filter = path: type:
          baseNameOf path != "_build" && lib.cleanSourceFilter path type;
      };
      scopes = forSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in (opam-nix.lib.${system}.buildOpamProject {
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
          }));
    in {
      packages = forSystems (system: {
        default = scopes.${system}.camulus;
        emacs-camulus = self.lib.mkEmacsPackage nixpkgs.legacyPackages.${system}.emacsPackages;
      });
      lib.mkWebkitPackage = epkgs:
        let system = epkgs.emacs.stdenv.hostPlatform.system;
        in nixpkgs.legacyPackages.${system}.callPackage ./nix/webkit {
          emacsPackages = epkgs;
        };
      # An ordinary Emacs package; no OCaml compiler is needed at runtime.
      lib.mkEmacsPackage = epkgs:
        let
          platform = epkgs.emacs.stdenv.hostPlatform;
          scope = scopes.${platform.system} or (throw "Camulus has no Nix build for ${platform.system}");
        in epkgs.trivialBuild {
          pname = "camulus";
          version = "0.1.0";
          packageRequires = lib.optional platform.isLinux (self.lib.mkWebkitPackage epkgs);
          src = "${scope.camulus}/share/emacs/site-lisp/camulus";
          buildPhase = ''
            emacs --batch -Q --eval '(progn (require (quote loaddefs-gen)) (loaddefs-generate default-directory (expand-file-name "camulus-autoloads.el")))'
          '';
          installPhase = ''
            packageDir="$out/share/emacs/site-lisp/elpa/camulus-0.1.0"
            mkdir -p "$packageDir"
            cp *.el *.so "$packageDir/"
            cat > "$packageDir/camulus-pkg.el" <<'EOF'
            (define-package "camulus" "0.1.0" "Native OCaml reader for Emacs" '((emacs "31.1")))
            EOF
          '';
        };
      devShells = forSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            inputsFrom = [ scopes.${system}.camulus ];
            packages = [ pkgs.emacs-nox pkgs.mu ];
          };
        });
    };
}
