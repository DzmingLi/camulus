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
      checks.${system}.github =
        let editor = pkgs.emacs-nox.pkgs.withPackages (epkgs: [ (self.lib.mkEmacsPackage epkgs) epkgs.forge ]);
        in pkgs.runCommand "camulus-github-integration" { nativeBuildInputs = [ pkgs.git ]; } ''
          export XDG_CONFIG_HOME="$TMPDIR/config"
          export XDG_STATE_HOME="$TMPDIR/state"
          export XDG_CACHE_HOME="$TMPDIR/cache"
          mkdir -p "$XDG_CONFIG_HOME" "$XDG_STATE_HOME" "$XDG_CACHE_HOME"
          ${editor}/bin/emacs --batch --module-assertions -l ${./test/github-integration.el}
          ${editor}/bin/emacs --batch --module-assertions -l ${./test/github-forge.el}
          touch "$out"
        '';
      # An ordinary Emacs package; no OCaml compiler is needed at runtime.
      lib.mkWebkitPackage = epkgs: pkgs.callPackage ./nix/webkit {
        emacsPackages = epkgs;
      };
      lib.mkEmacsPackage = epkgs: epkgs.trivialBuild {
        pname = "camulus";
        version = "0.1.0";
        packageRequires = [ (self.lib.mkWebkitPackage epkgs) ];
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
      devShells.${system}.default = pkgs.mkShell {
        inputsFrom = [ scope.camulus ];
        packages = [ pkgs.gnumake pkgs.emacs-nox pkgs.mu ];
      };
    };
}
