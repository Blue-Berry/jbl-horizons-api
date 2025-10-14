{
  description = "Horizons API";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        inherit (pkgs) dockerTools mkShell;
        inherit (dockerTools) buildImage;
        # Use specific version of ocamlPackages
        # inherit (pkgs) ocamlPackages;
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
        inherit (ocamlPackages) buildDunePackage;
        name = "jbl-horizons-api";
        version = "0.0.1";
      in {
        devShells = {
          default = mkShell {
            inputsFrom = [self'.packages.default];
            buildInputs = with ocamlPackages; [
              utop
              ocamlformat
              # patch ocaml-lsp so that inlay hints dont hide ghost values
              ocaml-lsp
            ];
          };
        };

        packages = {
          default = buildDunePackage {
            inherit version;
            pname = name;
            src = ./.;
            buildInputs = with ocamlPackages; [
              core
              core_unix
              ppx_jane
              lwt
              cohttp-lwt-unix
            ];
          };

          docker = buildImage {
            inherit name;
            tag = version;
            config = {
              Cmd = ["${self'.packages.default}/bin/${name}"];
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
            };
          };
        };
      };
    };
}
