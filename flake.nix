{
  description = "Bivrost emacs theme development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        # Create a custom minimal init.el for theme testing
        themeTestInit = pkgs.writeText "theme-test-init.el" ''
          ;; Minimal init file for theme testing only

          ;; Load the required packages
          (require 'package)
          (require 'autothemer)

          ;; Initialize evil mode first (before other packages)
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)  ;; Needed for evil-collection
          (require 'evil)
          (evil-mode 1)
  
          ;; Initialize evil-collection for better integration
          (when (require 'evil-collection nil t)
            (evil-collection-init))

          ;; Initialize evil-commentary
          (when (require 'evil-commentary nil t)
            (evil-commentary-mode))

          ;; Initialize vertico, marginalia, consult, and orderless
          (require 'vertico)
          (vertico-mode 1)

          (require 'marginalia)
          (marginalia-mode 1)

          (require 'consult)

          ;; Configure orderless for better fuzzy completion
          (require 'orderless)
          (setq completion-styles '(orderless basic)
                completion-category-defaults nil
                completion-category-overrides '((file (styles basic partial-completion))))

          ;; Corfu completion configuration
          (require 'corfu)
  
          ;; Basic Corfu settings for nice UI
          (setq corfu-cycle t                ;; Enable cycling through candidates
                corfu-auto t                 ;; Enable auto completion
                corfu-auto-delay 0.2         ;; Short delay before showing completions
                corfu-auto-prefix 2          ;; Show completions after entering 2 characters
                corfu-preview-current t      ;; Show current candidate preview
                corfu-preselect 'prompt      ;; Preselect the prompt
                corfu-on-exact-match nil     ;; Don't auto-complete on exact match
                corfu-scroll-margin 5)       ;; Use scroll margin
  
          ;; Enable Corfu globally
          (global-corfu-mode)
  
          ;; Set up Cape for better Corfu completion sources
          (when (require 'cape nil t)
            ;; Add useful completion backends
            (add-to-list 'completion-at-point-functions #'cape-file)
            (add-to-list 'completion-at-point-functions #'cape-dabbrev)
            (add-to-list 'completion-at-point-functions #'cape-keyword))
  
          ;; Add kind-icon for prettier completion UI
          (when (require 'kind-icon nil t)
            (setq kind-icon-use-icons t
                  kind-icon-default-face 'corfu-default
                  kind-icon-blend-background nil)
            ;; Add icons to completion candidates
            (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

          ;; Set up some convenient Consult keybindings
          (global-set-key (kbd "C-s") 'consult-line)
          (global-set-key (kbd "C-x b") 'consult-buffer)
          (global-set-key (kbd "M-g g") 'consult-goto-line)
          (global-set-key (kbd "M-g M-g") 'consult-goto-line)
          (global-set-key (kbd "M-y") 'consult-yank-pop)

          ;; Tree-sitter configuration
          (setq treesit-font-lock-level 4)  ;; Use maximum decoration level

          ;; Try to install Nix grammar if missing
          (when (and (fboundp 'treesit-install-language-grammar)
                     (not (treesit-language-available-p 'nix)))
            (message "Tree-sitter grammar for Nix not found, attempting installation...")
            (condition-case err
                (treesit-install-language-grammar 'nix)
              (error (message "Failed to install Nix grammar: %s" err))))

          ;; Enable major mode remapping to tree-sitter modes where available
          (setq major-mode-remap-alist
                '((c-mode . c-ts-mode)
                  (python-mode . python-ts-mode)
                  (sh-mode . bash-ts-mode)
                  (yaml-mode . yaml-ts-mode)))

          ;; Nix mode setup with fallback mechanism
          (if (and (require 'nix-ts-mode nil t)
                   (treesit-language-available-p 'nix))
              (progn
                (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
                (message "Using nix-ts-mode for .nix files"))
            (when (require 'nix-mode nil t)
              (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
              (message "Falling back to regular nix-mode for .nix files")))

          ;; Custom Nix formatting function
          (defun nixpkgs-fmt-buffer ()
            "Format the current buffer with nixpkgs-fmt if it is available."
            (interactive)
            (when (eq major-mode 'nix-ts-mode)
              (let ((output-buffer (get-buffer-create "*nixpkgs-fmt*")))
                (if (zerop (call-process-region (point-min) (point-max) 
                                              "nixpkgs-fmt" nil output-buffer nil))
                    (let ((p (point)))
                      (delete-region (point-min) (point-max))
                      (insert-buffer-substring output-buffer)
                      (goto-char p))
                  (message "nixpkgs-fmt failed"))
                (kill-buffer output-buffer))))

          ;; Configure Eglot for LSP support in Nix files
          (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs
                       '(nix-ts-mode . ("nil" "--stdio"))))

          ;; Dashboard configuration
          (require 'dashboard)
          (require 'page-break-lines)
          (page-break-lines-mode)

          ;; Dashboard settings
          (setq dashboard-banner-logo-title "Bivrost Theme Testing Environment")
          (setq dashboard-startup-banner 'logo)  ;; Use default Emacs logo
          (setq dashboard-center-content t)
  
          ;; Try to load icons if available
          (when (require 'all-the-icons nil 'noerror)
            (setq dashboard-set-heading-icons t)
            (setq dashboard-set-file-icons t))

          (setq dashboard-items '((recents  . 5)
                                 (bookmarks . 3)
                                 (registers . 3)))

          ;; Enable Dashboard 
          (dashboard-setup-startup-hook)

          ;; Add the current directory to load path
          (add-to-list 'custom-theme-load-path default-directory)
          (load-theme 'bivrost t)

          ;; Some useful settings for theme testing
          (menu-bar-mode 1)
          (tool-bar-mode -1)
          (scroll-bar-mode -1)
          (setq inhibit-startup-screen t)

          ;; Open the dashboard in the current buffer
          (dashboard-refresh-buffer)

          ;; Create some sample buffers for theme testing
          (with-current-buffer (get-buffer-create "*Theme Sample*")
            (erase-buffer)
            (insert ";; This is a sample buffer for theme testing\n\n")
            (insert "(defun sample-function (arg)\n  \"Docstring example.\"\n  (message \"Testing %s\" arg))\n\n")
            (insert "/* CSS Sample */\n.class {\n  color: red;\n}\n\n")
            (insert "# Python Sample\ndef hello():\n    print('Hello, Bivrost!')\n\n")
            (emacs-lisp-mode))

          ;; Create sample Nix buffer for testing
          (progn
            (find-file-noselect "/tmp/sample.nix")
            (with-current-buffer (get-buffer "/tmp/sample.nix")
              (erase-buffer)
              (insert "# A sample Nix file for theme testing\n\n")
              (insert "{ pkgs ? import <nixpkgs> {} }:\n\n")
              (insert "pkgs.stdenv.mkDerivation {\n")
              (insert "  pname = \"theme-test\";\n")
              (insert "  version = \"0.1.0\";\n\n")
              (insert "  src = ./src;\n\n")
              (insert "  buildInputs = with pkgs; [\n")
              (insert "    emacs\n")
              (insert "    ripgrep\n")
              (insert "    fd\n")
              (insert "  ];\n\n")
              (insert "  meta = {\n")
              (insert "    description = \"A test derivation for theme development\";\n")
              (insert "    license = pkgs.lib.licenses.mit;\n")
              (insert "  };\n")
              (insert "}\n")
              (nix-ts-mode)))

          ;; Setup some test buffers with syntax highlighting
          (progn 
            (find-file-noselect "/tmp/sample.py")
            (find-file-noselect "/tmp/sample.el")
            (find-file-noselect "/tmp/sample.org")
            (message "Bivrost theme testing environment loaded!"))

          ;; Function to reload the theme without restarting
          (defun reload-theme ()
            "Reload the Bivrost theme."
            (interactive)
            (disable-theme 'bivrost)
            (load-file (expand-file-name "bivrost-theme.el" default-directory))
            (load-theme 'bivrost t)
            (message "Bivrost theme reloaded!"))

          ;; Function to switch to dashboard
          (defun show-dashboard ()
            "Switch to the dashboard buffer."
            (interactive)
            (dashboard-refresh-buffer))
    
          ;; Function to test Corfu completion
          (defun test-corfu-completion ()
            "Open a buffer for testing Corfu completion."
            (interactive)
            (switch-to-buffer (get-buffer-create "*Corfu Test*"))
            (erase-buffer)
            (emacs-lisp-mode)
            (insert ";; Type here to test Corfu completion\n\n")
            (insert "(defun test-function ()\n  (mess")
            (message "Type to see Corfu completion (try completing 'message')"))

          ;; Function to test Tree-sitter highlighting
          (defun test-treesit ()
            "Open test files with tree-sitter modes for theme testing."
            (interactive)
            (find-file "/tmp/sample.nix")
            (message "Testing Tree-sitter Nix highlighting"))

          ;; Additional keybindings
          (global-set-key (kbd "C-c r") 'reload-theme)
          (global-set-key (kbd "C-c d") 'show-dashboard)
          (global-set-key (kbd "C-c c") 'test-corfu-completion)
          (global-set-key (kbd "C-c t") 'test-treesit)
  
          ;; Define a function to cycle through Evil states for testing
          (defun cycle-evil-states ()
            "Cycle through various evil states for theme testing."
            (interactive)
            (let ((states '(normal insert visual replace operator motion emacs))
                  (current-state evil-state)
                  next-state)
              (setq next-state (or (cadr (member current-state states))
                                  (car states)))
              (case next-state
                (normal (evil-normal-state))
                (insert (evil-insert-state))
                (visual (evil-visual-state))
                (replace (evil-replace-state))
                (operator (evil-operator-state))
                (motion (evil-motion-state))
                (emacs (evil-emacs-state)))
              (message "Switched to %s state" next-state)))
  
          ;; Bind the cycle function to a key
          (global-set-key (kbd "C-c s") 'cycle-evil-states)
        '';

        # Create a truly isolated user-dir for this Emacs instance
        isolatedEmacsDir = pkgs.runCommand "isolated-emacs-dir" { } ''
          mkdir -p $out/{etc,var/lib/emacs}
          touch $out/etc/site-start.el
        '';

        # Build a custom emacs with the required packages
        customEmacs = pkgs.emacs.pkgs.withPackages (epkgs: [
          epkgs.autothemer
          epkgs.consult
          epkgs.vertico
          epkgs.orderless
          epkgs.marginalia
          epkgs.dashboard
          epkgs.page-break-lines
          epkgs.all-the-icons
          epkgs.corfu
          epkgs.cape
          epkgs.kind-icon
          epkgs.evil
          epkgs.evil-collection
          epkgs.evil-commentary
          epkgs.treesit-grammars.with-all-grammars
          epkgs.nix-ts-mode
          epkgs.nix-mode
          epkgs.eglot
          epkgs.nix-ts-mode
          epkgs.flymake-diagnostic-at-point
        ]);

      in
      {
        # A standalone emacs for theme testing
        packages.emacs-theme-tester = pkgs.writeShellScriptBin "emacs-theme-tester" ''
          cd $PWD  # Ensure we're in the project directory
          
          # Create temporary directories for truly isolated Emacs
          export TMPDIR=$(mktemp -d)
          export HOME=$TMPDIR
          mkdir -p $HOME/.emacs.d
          
          # Set environment variables to ensure isolation
          export EMACSLOADPATH=""
          export EMACSDOCPATH=""
          
          # Run Emacs with isolation flags
          ${customEmacs}/bin/emacs \
            --no-init-file \
            --no-site-file \
            --no-splash \
            --directory ${isolatedEmacsDir}/etc \
            --eval '(setq user-emacs-directory "'"$HOME/.emacs.d"'")' \
            --load ${themeTestInit} \
            "$@"
            
          # Clean up temp directory
          rm -rf $TMPDIR
        '';

        # Convenience app that launches the theme testing environment
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.emacs-theme-tester}/bin/emacs-theme-tester";
        };

        # Development shell with theme development tools
        devShell = pkgs.mkShell {
          buildInputs = [
            customEmacs
            self.packages.${system}.emacs-theme-tester
            pkgs.nil
            pkgs.nixpkgs-fmt
          ];

          shellHook = ''
            echo "Bivrost theme development environment activated!"
            echo "Run 'emacs-theme-tester' to test your theme."
          '';
        };
      });
}
