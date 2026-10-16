# Both shell entry points use the locked opam-nix dependency graph.
(builtins.getFlake (toString ./.)).devShells.${builtins.currentSystem}.default
