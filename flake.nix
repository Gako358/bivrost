{
  description = "A flake for specific versions of Tomcat and MySQL";

  inputs = {
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs-unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs-unstable {
          inherit system;
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
          ];
        };
      });
}
