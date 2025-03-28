{
  description = "Bivrost emacs theme flake";

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
        packages.bivrost-theme = pkgs.emacsPackages.melpaBuild {
          pname = "bivrost-theme";
          version = "1.0.0";
          src = ./.;
          buildInputs = [ pkgs.emacs ];
          meta = with pkgs.lib; {
            description = "A custom Emacs theme called Bivrost";
            license = licenses.gpl3;
            maintainers = [ maintainers.yourname ];
          };
        };

        defaultPackage = self.packages.${system}.bivrost-theme;

        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.emacs
            self.packages.${system}.bivrost-theme
          ];
        };

        apps.default =
          let
            # Create a script to properly load the theme
            loadThemeScript = pkgs.writeText "load-bivrost-theme.el" ''
              (add-to-list 'custom-theme-load-path "${self.packages.${system}.bivrost-theme}/share/emacs/site-lisp/elpa/bivrost-theme-1.0.0")
              (load-theme 'bivrost t)
              (message "Bivrost theme loaded successfully!")
            '';
          in
          {
            type = "app";
            program = "${pkgs.writeShellScript "emacs-with-bivrost" ''
              ${pkgs.emacs}/bin/emacs --no-init-file --no-site-file --load ${loadThemeScript} "$@"
            ''}";
          };
      });
}
