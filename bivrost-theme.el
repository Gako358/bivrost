;;; bivrost-theme.el --- Minimal, dark theme -*- lexical-binding: t -*-

;; Author: MerrinX <gako358@outlook.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30"))
;; Keywords: faces
;; Homepage: https://github.com/gako358/bivrost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;;; Code:

(deftheme bivrost
  "bivrost theme.")

(defgroup bivrost nil
  "Bivrost theme properties."
  :group 'faces)

(eval-and-compile
  (defconst bivrost-theme-colors-alist
    '(;; Basic
      (bg-main . "#272727")        ;; Primary background color - deep charcoal
      (fg-main . "#cdcecf")        ;; Primary foreground color - soft white with slight blue tint
      (bg-active . "#29394f")      ;; Active window/element background - deep navy blue
      (bg-inactive . "#212e3f")    ;; Inactive window/element background - muted slate blue
      (black . "#393b44")          ;; True black alternative - smoky charcoal
      (black-alt . "#131a24")      ;; Secondary black - inkwell blue-black
      (gray . "#71839b")           ;; Primary gray - steel blue-gray
      (gray-light . "#aeafb0")     ;; Light gray - cloudy silver
      (gray-silver . "#738091")    ;; Silver gray - muted slate
      (gray-dark . "#39506d")      ;; Dark gray - deep slate blue
      (purple . "#9d79d6")         ;; Purple - lavender violet
      (blue . "#719cd6")           ;; Primary blue - sky cerulean
      (blue-alt . "#63cdcf")       ;; Alternative blue - turquoise
      (blue-light . "#2b3b51")     ;; Light blue - midnight navy
      (green . "#81b29a")          ;; Primary green - sage
      (green-mint . "#a3ccb5")     ;; Mint green - soft pastels
      (green-light . "#94c4af")    ;; Light green - pale jade
      (green-faint . "#5a7d6d")    ;; Faint green - forest mist
      (yellow-dark . "#b59d61")    ;; Dark yellow - antique gold
      (red . "#c94f6d")            ;; Primary red - cranberry
      (red-faint . "#e0a3b3")      ;; Faint red - dusty rose
      (orange . "#f4a261"))))      ;; Orange - warm peach

(defmacro bivrost-with-color-variables (&rest body)
  (declare (indent 0))
  `(let (,@(mapcar (lambda (cons)
		     (list (car cons) (cdr cons)))
		   bivrost-theme-colors-alist))
     ,@body))

(defface bivrost-critical nil
  "Critical face for information that requires immediate action."
  :group nil)

(defface bivrost-critical-i nil
  "Critical face inversed."
  :group nil)

(defface bivrost-bold
  '((t (:bold t :foreground "#000")))
  "Bold face."
  :group nil)

(defface bivrost-strong nil
  "Strong face for information of a structural nature."
  :group nil)

(defface bivrost-strong-i nil
  "Strong face inversed."
  :group nil)

(defface bivrost-keyword nil
  "Default keyword face."
  :group nil)

(defface bivrost-type nil
  "Default type face."
  :group nil)

(defface bivrost-verbatim nil
  "Face used for things like strings."
  :group nil)

(defface bivrost-faded nil
  "Faded face for less important information."
  :group nil)

(defface bivrost-faded-i nil
  "Faded face inversed." :group nil)

(defface bivrost-subtle nil
  "Subtle face is used to suggest a physical area on the screen."
  :group nil)

(defface bivrost-subtle-i nil
  "Subtle face inversed." :group nil)

(defface bivrost-default nil
  "Default face." :group nil)

(defface bivrost-default-i nil
  "Default face inversed." :group nil)

(defface bivrost-highlight nil
  "Default highlight face."
  :group nil)

(defface bivrost-warning nil
  "Warning face."
  :group nil)

(defface bivrost-error nil
  "Error face."
  :group nil)

(defface bivrost-note nil
  "Note face."
  :group nil)

(defface bivrost-block nil
  "Default block face."
  :group nil)

(defface bivrost-button nil
  "Default button face."
  :group nil)

(defface bivrost-button-hover nil
  "Hover button face."
  :group nil)

(defface bivrost-button-pressed nil
  "Pressed button face."
  :group nil)

(defface bivrost-border nil
  "Border face."
  :group nil)

(defface bivrost-bar nil
  "Face used for active mode-line and tab-bar"
  :group nil)

(defface bivrost-bar-inactive nil
  "Face used for inactive mode-line and tab-bar"
  :group nil)

(defface bivrost-link nil
  "Face used for links."
  :group nil)

(defface bivrost-paren-face
  '((t (:foreground "grey70")))
  "Face used to dim parentheses."
  :group nil)

(defun bivrost--font-lock-add-paren ()
  "Make Lisp parentheses faded."
  (font-lock-add-keywords nil '(("(\\|)" . 'bivrost-paren-face))))

(defvar bivrost-after-load-hook nil
  "Hook run after theme has loaded.")

(defcustom bivrost-use-more-bold nil
  "Use more bold constructs."
  :type 'boolean :group 'bivrost)

(defcustom bivrost-faded-lisp-parens-modes
  '(emacs-lisp-mode
    lisp-mode
    scheme-mode
    racket-mode)
  "List of modes for which faded parentheses should be enabled."
  :type '(symbol) :group 'bivrost)

(defcustom bivrost-use-more-fading nil
  "Use more fading."
  :type 'boolean :group 'bivrost)

(defun bivrost--set-faded-lisp-parens (symbol value)
  "Bivrost :set function for `bivrost-use-faded-lisp-parens'.
Takes care of adding or removing hooks when the
`bivrost-use-faded-lisp-parens' variable is customized."
  (let ((hooks (mapcar (lambda (mode) (intern (format "%s-hook" mode)))
		       bivrost-faded-lisp-parens-modes)))
    (if value
	(progn
	  (dolist (hook hooks)
	    (add-hook hook #'bivrost--font-lock-add-paren)))
      (dolist (hook hooks)
	(remove-hook hook #'bivrost--font-lock-add-paren))))
  (setq bivrost-use-faded-lisp-parens value))

(defcustom bivrost-use-faded-lisp-parens t
  "Use faded parenthesis in Lisp modes."
  :type 'boolean :group 'bivrost
  :initialize #'custom-initialize-reset
  :set #'bivrost--set-faded-lisp-parens)

(defface bivrost-heading-1 nil
  "Face for headings."
  :group nil)

(defun bivrost ()
  "Load bivrost theme."
  (interactive)

  (when bivrost-use-faded-lisp-parens
    (add-hook 'lisp-data-mode-hook #'bivrost--font-lock-add-paren)
    (add-hook 'scheme-mode-hook #'bivrost--font-lock-add-paren))

  (load-theme 'bivrost t)
  (run-hooks 'bivrost-after-load-hook))

;;(make-obsolete 'bivrost 'load-theme "0.1")

;; --- Faces ---------------------------------------------------------
(bivrost-with-color-variables
 (let ((bivrost-heading-1-height (if bivrost-use-more-bold 1.0 1.1))
       (faded-color (if bivrost-use-more-fading gray-silver gray-dark)))
   (custom-theme-set-faces
    'bivrost

    ;; --- Base ---------------------------------------------------------
    `(cursor ((t (:foreground ,bg-main :background ,fg-main))))

    `(default ((t (:background ,bg-main :foreground ,fg-main))))

    `(highlight ((t (:background ,gray))))

    `(bivrost-subtle ((t (:background ,gray-light))))

    `(bivrost-subtle-i ((t (:foreground ,gray-light))))

    `(bivrost-faded ((t (:foreground ,faded-color))))
    `(bivrost-faded-i ((t (:foreground ,bg-main :background ,faded-color))))

    `(bivrost-default ((t (:foreground ,fg-main))))

    `(bivrost-default-i ((t (:foreground ,bg-main :background ,fg-main))))

    `(bivrost-keyword ((t (:foreground ,purple))))
    `(bivrost-type ((t (:foreground ,green))))
    `(bivrost-verbatim ((t (:foreground ,yellow-dark))))

    `(bivrost-strong ((t ,(when bivrost-use-more-bold '(:weight semibold)))))
    `(bivrost-strong-i ((t (:foreground ,bg-main :background ,fg-main :weight bold))))

    `(bivrost-warning ((t (:foreground ,orange))))
    `(bivrost-note ((t (:foreground ,green-light))))
    `(bivrost-error ((t (:foreground ,red))))
    `(bivrost-critical ((t (:foreground ,bg-main :background ,red))))
    `(bivrost-critical-i ((t (:foreground ,red))))
    `(bivrost-link ((t (:foreground ,blue :underline t))))

    `(bivrost-heading-1 ((t (:inherit bivrost-strong :height ,bivrost-heading-1-height))))
    `(bivrost-block ((t (:background ,bg-active :foreground ,fg-main :extend t))))

    `(bivrost-border ((t (:foreground ,gray-light :box (:color ,gray-silver :line-width 1)))))

    `(bivrost-bar ((t (:foreground ,fg-main :inherit bivrost-border))))
    `(bivrost-bar-inactive ((t (:foreground "#535c65" :background ,bg-inactive :inherit bivrost-border))))

    `(bivrost-button ((t (:box (:color ,gray-silver) :background ,bg-inactive))))
    `(bivrost-button-pressed ((t (:box (:color ,gray-dark) :background ,bg-inactive))))
    `(bivrost-button-hover ((t (:inherit bivrost-button :background ,bg-inactive))))

    ;; --- Header & mode line -------------------------------------------
    `(mode-line ((t (:inherit bivrost-bar))))
    `(mode-line-inactive ((t (:inherit bivrost-bar-inactive))))
    `(mode-line-buffer-id ((t (:weight regular :background nil))))
    `(mode-line-emphasis ((t (:weight regular :background nil))))
    `(header-line ((t (:inherit bivrost-bar :box nil))))

    ;; --- Structural ---------------------------------------------------
    '(bold ((t (:inherit bivrost-strong :weight semibold))))
    '(italic ((t (:slant italic))))
    '(italic ((t (:inherit bivrost-faded))))
    '(bold-italic ((t (:inherit bivrost-strong))))
    '(region ((t (:inherit highlight))))
    '(fringe ((t (:inherit bivrost-faded))))
    '(hl-line ((t (:inherit bivrost-subtle))))
    '(link ((t (:inherit bivrost-link))))

    ;; --- Semantic -----------------------------------------------------
    '(shadow ((t (:inherit bivrost-faded))))
    '(success ((t (:inherit bivrost-keyword))))
    '(warning ((t (:inherit bivrost-warning))))
    '(error ((t (:inherit bivrost-critical))))
    '(match ((t (:inherit ,bivrost-bold))))
    `(preview-face ((t (:inherit bivrost-subtle))))

    ;; --- General ------------------------------------------------------
    '(buffer-menu-buffer ((t (:inherit bivrost-strong))))
    '(minibuffer-prompt ((t (:inherit bivrost-strong))))
    `(isearch ((t (:inherit (bivrost-strong highlight)))))
    '(isearch-fail ((t (:inherit bivrost-faded))))
    `(isearch-group-1 ((t (:foreground ,bg-main :background ,purple))))
    '(show-paren-match ((t (:weight bold :foreground "#AB47BC"))))
    '(show-paren-mismatch ((t (:inherit bivrost-critical))))
    '(lazy-highlight ((t (:inherit bivrost-subtle))))
    '(trailing-whitespace ((t (:inherit bivrost-subtle))))
    '(secondary-selection ((t (:inherit bivrost-subtle))))
    '(completions-annotations ((t (:inherit bivrost-faded))))
    '(completions-common-part ((t (:inherit bivrost-bold))))
    '(completions-first-difference ((t (:inherit nil))))
    '(tooltip ((t (:inherit bivrost-subtle))))
    '(read-multiple-choice-face ((t (:inherit bivrost-strong))))
    '(nobreak-hyphen ((t (:inherit bivrost-strong))))
    '(nobreak-space ((t (:inherit bivrost-strong))))
    '(help-argument-name ((t (:inherit bivrost-verbatim))))
    '(help-key-binding ((t :inherit bivrost-verbatim)))
    '(tabulated-list-fake-header ((t (:inherit bivrost-strong))))
    '(tool-bar ((t (:inherit bivrost-faded-i))))

    ;; --- Pulse --------------------------------------------------------
    `(pulse-highlight-face ((t (:inherit highlight))))
    `(pulse-highlight-start-face ((t (:inherit highlight))))

    ;; --- TTY faces ----------------------------------------------------
    '(tty-menu-disabled-face ((t (:inherit bivrost-faded-i))))
    '(tty-menu-enabled-face ((t (:inherit bivrost-default-i))))
    '(tty-menu-selected-face ((t (:inherit bivrost-keyword-i))))

    ;; --- RE-builder ----------------------------------------------------
    `(reb-match-1 ((t :inherit highlight)))

    ;; --- Ansi faces ----------------------------------------------------
    `(ansi-color-red ((t :foreground ,red)))
    `(ansi-color-green ((t :foreground "#263237")))
    `(ansi-color-blue ((t :foreground ,blue)))
    `(ansi-color-bright-green ((t :foreground ,green)))
    `(ansi-color-yellow ((t :foreground ,yellow-dark)))
    `(ansi-color-bold ((t :inherit bivrost-bold)))
    `(ansi-color-cyan ((t :foreground ,gray-dark)))

    ;; --- whitespace-mode ----------------------------------------------------
    `(whitespace-space ((t (:inherit bivrost-default))))
    `(whitespace-empty ((t (:inherit bivrost-default :foreground ,orange))))
    `(whitespace-newline ((t (:inherit bivrost-faded))))

    ;; --- Eshell ----------------------------------------------------
    '(eshell-prompt ((t (:inherit bivrost-default))))

    ;; --- ERC ----------------------------------------------------
    '(erc-prompt-face ((t (:inhert bivrost-default))))
    '(erc-timestamp-face ((t (:inhert bivrost-faded))))
    '(erc-notice-face ((t (:inherit bivrost-keyword))))
    '(erc-current-nick-face ((t (:inherit bivrost-strong))))
    '(erc-error-face ((t (:inherit bivrost-critical-i))))

    ;; --- Dictionary ----------------------------------------------------
    `(dictionary-word-definition-face ((t (:inherit (bivrost-default fixed-pitch)))))
    `(dictionary-reference-face ((t (:inherit (bivrost-keyword fixed-pitch)))))

    ;; --- Windows divider ----------------------------------------------
    `(window-divider ((t (:foreground ,bg-main))))
    '(window-divider-first-pixel ((t (:inherit window-divider))))
    '(window-divider-last-pixel ((t (:inherit window-divider))))
    `(vertical-border ((t (:foreground ,gray-silver))))

    ;; --- Tab bar ------------------------------------------------------
    '(tab-bar ((t (:inherit bivrost-bar))))
    `(tab-bar-tab ((t (:inherit default :box (:line-width 1 :color ,gray-silver)))))
    '(tab-bar-tab-inactive ((t (:inherit bivrost-faded))))
    '(tab-line ((t (:inherit default))))

    ;; --- Speedbar ------------------------------------------------------
    `(speedbar-selected-face ((t (:inherit bivrost-keyword))))
    `(speedbar-file-face ((t (:inherit bivrost-default))))
    `(speedbar-directory-face ((t (:inherit (bivrost-default bivrost-bold)))))
    `(speedbar-highlight-face ((t (:inherit bivrost-button-hover :box nil))))
    `(speedbar-tag-face ((t (:inherit bivrost-default))))
    `(speedbar-button-face ((t (:inherit bivrost-button))))

    ;; --- Line numbers -------------------------------------------------
    '(line-number ((t (:inherit bivrost-faded))))
    '(line-number-current-line ((t (:inherit default))))
    `(line-number-major-tick ((t (:inherit bivrost-faded))))
    '(line-number-minor-tick ((t (:inherit bivrost-faded))))

    ;; --- Font lock ----------------------------------------------------
    '(font-lock-comment-face ((t (:inherit bivrost-faded))))
    '(font-lock-doc-face ((t (:inherit bivrost-faded))))
    `(font-lock-string-face ((t (:inherit bivrost-verbatim))))
    '(font-lock-constant-face ((t (:inherit bivrost-strong))))
    `(font-lock-warning-face ((t (:inherit bivrost-warning))))
    '(font-lock-function-name-face ((t (:inherit bivrost-strong))))
    `(font-lock-variable-name-face ((t (:inherit bivrost-default))))
    '(font-lock-builtin-face ((t (:inherit bivrost-keyword))))
    '(font-lock-type-face ((t (:inherit bivrost-type))))
    '(font-lock-keyword-face ((t (:inherit bivrost-keyword))))

    '(shr-h2 ((t :inherit bivrost-bold)))

    ;; --- Popper -------------------------------------------------------
    `(popper-echo-area-buried ((t (:inherit bivrost-default))))
    `(popper-echo-dispatch-hint ((t (:inherit bivrost-subtle))))

    ;; --- Custom edit --------------------------------------------------
    '(widget-field ((t (:inherit bivrost-subtle))))
    '(widget-button ((t (:inherit bivrost-strong))))
    '(widget-single-line-field ((t (:inherit bivrost-subtle))))
    '(custom-group-subtitle ((t (:inherit bivrost-strong))))
    '(custom-group-tag ((t (:inherit bivrost-strong))))
    '(custom-group-tag-1 ((t (:inherit bivrost-strong))))
    '(custom-comment ((t (:inherit bivrost-faded))))
    '(custom-comment-tag ((t (:inherit bivrost-faded))))
    '(custom-changed ((t (:inherit bivrost-keyword))))
    '(custom-modified ((t (:inherit bivrost-keyword))))
    '(custom-face-tag ((t (:inherit bivrost-strong))))
    '(custom-variable-tag ((t (:inherit bivrost-strong))))
    '(custom-invalid ((t (:inherit bivrost-strong))))
    '(custom-visibility ((t (:inherit bivrost-keyword))))
    '(custom-state ((t (:inherit bivrost-keyword))))
    '(custom-link ((t (:inherit bivrost-keyword))))
    '(custom-variable-obsolete ((t (:inherit bivrost-faded))))

    ;; --- Corfu  --------------------------------------------------------
    `(corfu-current ((t (:inherit highlight))))

    ;; --- Vertico  --------------------------------------------------------
    `(vertico-current ((t (:inherit highlight))))

    ;; --- Buttons ------------------------------------------------------
    `(custom-button ((t (:inherit bivrost-button))))

    `(custom-button-mouse ((t (:inherit bivrost-button-hover))))

    `(custom-button-pressed ((t (:inherit bivrost-button-pressed))))

    ;; --- Packages -----------------------------------------------------
    '(package-description ((t (:inherit bivrost-default))))
    '(package-help-section-name ((t (:inherit bivrost-default))))
    '(package-name ((t (:inherit bivrost-keyword))))
    '(package-status-avail-obso ((t (:inherit bivrost-faded))))
    '(package-status-available ((t (:inherit bivrost-default))))
    '(package-status-built-in ((t (:inherit bivrost-keyword))))
    '(package-status-dependency ((t (:inherit bivrost-keyword))))
    '(package-status-disabled ((t (:inherit bivrost-faded))))
    '(package-status-external ((t (:inherit bivrost-default))))
    '(package-status-held ((t (:inherit bivrost-default))))
    '(package-status-incompat ((t (:inherit bivrost-faded))))
    '(package-status-installed ((t (:inherit bivrost-keyword))))
    '(package-status-new ((t (:inherit bivrost-default))))
    '(package-status-unsigned ((t (:inherit bivrost-default))))

    ;; --- Info ---------------------------------------------------------
    '(info-node ((t (:inherit bivrost-strong))))
    '(info-menu-header ((t (:inherit bivrost-strong))))
    '(info-header-node ((t (:inherit bivrost-default))))
    '(info-index-match ((t (:inherit bivrost-keyword))))
    '(info-menu-star ((t (:inherit bivrost-default))))
    '(Info-quoted ((t (:inherit bivrost-keyword))))
    '(info-title-1 ((t (:inherit bivrost-strong))))
    '(info-title-2 ((t (:inherit bivrost-strong))))
    '(info-title-3 ((t (:inherit bivrost-strong))))
    '(info-title-4 ((t (:inherit bivrost-strong))))

    ;; --- Helpful ------------------------------------------------------
    '(helpful-heading ((t (:inherit bivrost-strong))))

    ;; --- EPA ----------------------------------------------------------
    '(epa-field-body ((t (:inherit bivrost-default))))
    '(epa-field-name ((t (:inherit bivrost-strong))))
    '(epa-mark ((t (:inherit bivrost-keyword))))
    '(epa-string ((t (:inherit bivrost-strong))))
    '(epa-validity-disabled ((t (:inherit bivrost-faded))))
    '(epa-validity-high ((t (:inherit bivrost-strong))))
    '(epa-validity-medium ((t (:inherit bivrost-default))))
    '(epa-validity-low ((t (:inherit bivrost-faded))))

    ;; --- Dired --------------------------------------------------------
    `(dired-header ((t (:foreground "#463c65" :inherit bivrost-bold))))

    '(dired-directory ((t (:inherit (bivrost-bold)))))
    `(dired-symlink ((t (:slant italic))))
    '(dired-marked ((t (:inherit bivrost-keyword))))
    `(dired-flagged ((t (:inherit bivrost-critical-i))))
    `(dired-broken-symlink ((t (:slant italic :strike-through ,red))))

    ;; --- Diredfl ------------------------------------------------------
    `(diredfl-dir-heading ((t (:inherit bivrost-keyword))))
    `(diredfl-file-name ((t (:inhert bivrost-default))))
    `(diredfl-write-priv ((t (:inhert bivrost-default))))
    `(diredfl-read-priv ((t (:inhert bivrost-default))))
    `(diredfl-exec-priv ((t (:inherit bivrost-keyword))))
    `(diredfl-no-priv ((t (:inherit bivrost-faded))))
    `(diredfl-dir-priv ((t (:inherit (bivrost-bold bivrost-strong)))))
    `(diredfl-date-time ((t (:inherit bivrost-verbatim))))
    `(diredfl-number ((t (:foreground ,fg-main))))
    `(diredfl-file-suffix ((t (:inherit bivrost-keyword))))
    `(diredfl-dir-name ((t (:inherit bivrost-bold))))
    `(diredfl-deletion-file-name ((t (:background ,bg-inactive))))
    `(diredfl-deletion ((t (:inherit (bivrost-critical-i bivrost-bold)))))
    `(diredfl-ignored-file-name ((t (:inherit bivrost-faded))))
    `(diredfl-flag-mark-line ((t (:background ,bg-inactive))))
    `(diredfl-flag-mark ((t (:background ,bg-inactive))))
    `(diredfl-symlink ((t (:slant italic))))
    `(diredfl-rare-priv ((t (:inherit bivrost-default :slant italic))))
    `(diredfl-compressed-file-name ((t (:inherit bivrost-default))))
    `(diredfl-compressed-extensions ((t (:inherit bivrost-keyword))))
    `(diredfl-compressed-file-suffix ((t (:inherit bivrost-type))))
					; TODO: I don't know what these are..
    `(diredfl-link-priv ((t (:foreground ,orange))))
					;`(diredfl-other-priv ((t ())))
    `(diredfl-tagged-autofile-name ((t (:background "#c6dad3"))))

    ;; --- Eglot --------------------------------------------------------
    `(eglot-mode-line ((t (:foreground ,fg-main))))
    `(eglot-mode-line-none-face ((t (:foreground ,fg-main))))
    '(eglot-highlight-symbol-face ((t (:inherit underline))))

    ;; --- Eww ----------------------------------------------------
    `(eww-form-submit ((t (:box (:style released-button) :background ,bg-inactive))))
    `(shr-link ((t (:foreground ,blue))))

    ;; --- Keycast ------------------------------------------------------
    `(keycast-key ((t :inherit nil :bold t)))
    `(keycast-command ((t :inherit bivrost-default)))

    ;; --- Popup --------------------------------------------------------
    '(popup-face ((t (:inherit highlight))))
    '(popup-isearch-match ((t (:inherit bivrost-strong))))
    '(popup-menu-face ((t (:inherit bivrost-subtle))))
    '(popup-menu-mouse-face ((t (:inherit bivrost-faded-i))))
    '(popup-menu-selection-face ((t (:inherit bivrost-keyword-i))))
    '(popup-menu-summary-face ((t (:inherit bivrost-faded))))
    '(popup-scroll-bar-background-face ((t (:inherit bivrost-subtle))))
    '(popup-scroll-bar-foreground-face ((t (:inherit bivrost-subtle))))
    '(popup-summary-face ((t (:inherit bivrost-faded))))
    '(popup-tip-face ((t (:inherit bivrost-strong-i))))

    ;; --- Diff ---------------------------------------------------------
    `(diff-header ((t (:inherit bivrost-bold))))
    '(diff-file-header ((t (:inherit bivrost-strong))))
    '(diff-context ((t (:inherit bivrost-default))))
    '(diff-removed ((t (:background "#ffb7b6"))))
    '(diff-changed ((t (:inherit bivrost-strong))))
    `(diff-added ((t (:background ,green-mint))))
    '(diff-refine-added ((t (:inherit (bivrost-keyword bivrost-strong)))))
    '(diff-refine-changed ((t (:inherit bivrost-strong))))
    '(diff-refine-removed ((t (:inherit bivrost-faded :strike-through t))))

    ;; --- Message ------------------------------------------------------
    '(message-cited-text-1 ((t (:inherit bivrost-faded))))
    '(message-cited-text-2 ((t (:inherit bivrost-faded))))
    '(message-cited-text-3 ((t (:inherit bivrost-faded))))
    '(message-cited-text-4 ((t (:inherit bivrost-faded))))
    '(message-cited-text ((t (:inherit bivrost-faded))))
    '(message-header-cc ((t (:inherit bivrost-default))))
    '(message-header-name ((t (:inherit bivrost-strong))))
    '(message-header-newsgroups ((t (:inherit bivrost-default))))
    '(message-header-other ((t (:inherit bivrost-default))))
    '(message-header-subject ((t (:inherit bivrost-keyword))))
    '(message-header-to ((t (:inherit bivrost-keyword))))
    '(message-header-xheader ((t (:inherit bivrost-default))))
    '(message-mml ((t (:inherit bivrost-strong))))
    '(message-separator ((t (:inherit bivrost-faded))))

    ;; --- Outline ------------------------------------------------------
    '(outline-1 ((t (:inherit bivrost-strong))))
    '(outline-2 ((t (:inherit bivrost-strong))))
    '(outline-3 ((t (:inherit bivrost-strong))))
    '(outline-4 ((t (:inherit bivrost-strong))))
    '(outline-5 ((t (:inherit bivrost-strong))))
    '(outline-6 ((t (:inherit bivrost-strong))))
    '(outline-7 ((t (:inherit bivrost-strong))))
    '(outline-8 ((t (:inherit bivrost-strong))))

    ;; --- Orderless ------------------------------------------------------
    '(orderless-match-face-0 ((t (:inherit bivrost-bold))))
    '(orderless-match-face-1 ((t (:inherit bivrost-bold))))
    '(orderless-match-face-2 ((t (:inherit bivrost-bold))))
    '(orderless-match-face-3 ((t (:inherit bivrost-bold))))

    ;; --- Flyspell ----------------------------------------------------
    '(flyspell-duplicate ((t (:inherit bivrost-warning))))
    `(flyspell-incorrect ((t (:inherit bivrost-strong :underline (:style wave :color ,red)))))

    ;; --- Flymake ----------------------------------------------------
    `(flymake-error ((t (:underline (:style wave :color ,red)))))
    `(flymake-warning ((t (:underline (:style wave :color ,orange)))))
    `(flymake-note ((t (:underline (:style wave :color ,green-light)))))
    `(compilation-error ((t (:inherit bivrost-error))))
    `(compilation-warning ((t (:foreground ,orange))))
    `(compilation-mode-line-run ((t (:inherit bivrost-foreground))))

    ;; --- Flycheck ----------------------------------------------------
    `(flycheck-error ((t (:underline (:style wave :color ,red)))))
    `(flycheck-warning ((t (:underline (:style wave :color ,orange)))))
    `(flycheck-info ((t (:underline (:style wave :color ,green-light)))))

    ;; --- Org agenda ---------------------------------------------------
    '(org-agenda-calendar-event ((t (:inherit bivrost-default))))
    '(org-agenda-calendar-sexp ((t (:inherit bivrost-keyword))))
    '(org-agenda-clocking ((t (:inherit bivrost-faded))))
    '(org-agenda-column-dateline ((t (:inherit bivrost-faded))))
    '(org-agenda-current-time ((t (:inherit bivrost-strong))))
    '(org-agenda-date ((t (:inherit bivrost-default))))
    '(org-agenda-date-today ((t (:inherit (bivrost-keyword bivrost-strong)))))
    '(org-agenda-date-weekend ((t (:inherit bivrost-critical-i))))
    '(org-agenda-diary ((t (:inherit bivrost-faded))))
    '(org-agenda-dimmed-todo-face ((t (:inherit bivrost-faded))))
    '(org-agenda-done ((t (:inherit bivrost-faded))))
    '(org-agenda-filter-category ((t (:inherit bivrost-faded))))
    '(org-agenda-filter-effort ((t (:inherit bivrost-faded))))
    '(org-agenda-filter-regexp ((t (:inherit bivrost-faded))))
    '(org-agenda-filter-tags ((t (:inherit bivrost-faded))))
    '(org-agenda-property-face ((t (:inherit bivrost-faded))))
    '(org-agenda-restriction-lock ((t (:inherit bivrost-faded))))
    '(org-agenda-structure ((t (:inherit bivrost-bold))))
    `(org-dispatcher-highlight ((t (:inherit bivrost-keyword :bold t))))

    ;; --- Org ----------------------------------------------------------
    '(org-archived ((t (:inherit bivrost-faded))))
    '(org-block ((t (:inherit (bivrost-block fixed-pitch)))))

    `(org-block-begin-line ((t (:inherit (bivrost-faded fixed-pitch)
					 :overline ,gray-silver
					 :background ,gray-light
					 :height 0.9
					 :extend t))))

    `(org-block-end-line ((t (:inherit fixed-pitch
				       :foreground ,gray-silver
				       :background ,gray-light
				       :height 0.9
				       :extend t))))

    '(org-checkbox ((t (:inherit (bivrost-default fixed-pitch)))))
    '(org-checkbox-statistics-done ((t (:inherit (bivrost-faded fixed-pitch)))))
    '(org-checkbox-statistics-todo ((t (:inherit (bivrost-default fixed-pitch)))))
    '(org-clock-overlay ((t (:inherit bivrost-faded))))
    '(org-code ((t (:inherit (fixed-pitch bivrost-block)))))
    '(org-column ((t (:inherit bivrost-faded))))
    '(org-column-title ((t (:inherit bivrost-faded))))
    '(org-date ((t (:inherit bivrost-faded))))
    '(org-date-selected ((t (:inherit bivrost-faded))))
    '(org-default ((t (:inherit bivrost-faded))))
    '(org-document-info ((t (:inherit bivrost-faded))))
    '(org-document-info-keyword ((t (:inherit (bivrost-faded fixed-pitch)))))
    '(org-document-title ((t (:inherit bivrost-strong :height 1.8 :weight semibold))))
    '(org-done ((t (:foreground "#585c60"))))
    '(org-drawer ((t (:inherit (bivrost-faded fixed-pitch)))))
    '(org-ellipsis ((t (:inherit bivrost-faded))))
    '(org-footnote ((t (:inherit bivrost-faded))))
    '(org-formula ((t (:inherit bivrost-faded))))
    '(org-headline-done ((t (:foreground "#585c60"))))
    '(org-headline-todo ((t (:inherit bivrost-default))))
    '(org-hide ((t (:inherit bivrost-subtle-i))))
    '(org-indent ((t (:inherit bivrost-subtle-i))))
    `(org-latex-and-related ((t (:inherit (bivrost-default) :background ,bg-main))))
    `(org-level-1 ((t (:inherit bivrost-heading-1))))
    `(org-level-2 ((t (:inherit bivrost-heading-1))))
    `(org-level-3 ((t (:inherit bivrost-heading-1))))
    `(org-level-4 ((t (:inherit bivrost-heading-1))))
    `(org-level-5 ((t (:inherit bivrost-heading-1))))
    `(org-level-6 ((t (:inherit bivrost-strong))))
    `(org-level-7 ((t (:inherit bivrost-strong))))
    `(org-level-8 ((t (:inherit bivrost-strong))))
    `(org-link ((t (:inherit link))))
    '(org-list-dt ((t (:inherit bivrost-faded))))
    '(org-macro ((t (:inherit bivrost-faded))))
    '(org-meta-line ((t (:inherit (bivrost-faded fixed-pitch) :height 0.9))))
    '(org-mode-line-clock ((t (:inherit bivrost-faded))))
    '(org-mode-line-clock-overrun ((t (:inherit bivrost-faded))))
    '(org-priority ((t (:inherit bivrost-faded))))
    '(org-property-value ((t (:inherit (bivrost-faded fixed-pitch)))))
    '(org-quote ((t (:inherit bivrost-default))))
    '(org-scheduled ((t (:inherit bivrost-faded))))
    '(org-scheduled-previously ((t (:inherit bivrost-faded))))
    '(org-scheduled-today ((t (:inherit bivrost-faded))))
    '(org-sexp-date ((t (:inherit bivrost-faded))))
    '(org-special-keyword ((t (:inherit (bivrost-faded fixed-pitch)))))
    '(org-table ((t (:inherit (bivrost-default fixed-pitch)))))
    '(org-tag ((t (:inherit bivrost-strong))))
    '(org-tag-group ((t (:inherit bivrost-faded))))
    '(org-target ((t (:inherit bivrost-faded))))
    '(org-time-grid ((t (:inherit bivrost-faded))))
    '(org-todo ((t (:inherit (bivrost-keyword bivrost-strong)))))
    '(org-upcoming-deadline ((t (:inherit bivrost-default))))
    '(org-verbatim ((t (:inherit (bivrost-verbatim fixed-pitch)))))
    '(org-verse ((t (:inherit bivrost-faded))))
    '(org-warning ((t (:inherit bivrost-strong))))

    ;; --- Org modern ---------------------------------------------------
    `(org-modern-date-active ((t (:inherit org-modern-done :background ,bg-inactive))))
    `(org-modern-statistics ((t (:inherit org-modern-done :background ,bg-inactive))))
    `(org-modern-priority ((t (:inherit org-modern-done :background ,bg-inactive))))
    `(org-modern-label ((t (:box (:color ,bg-main :line-width (0 . -1))))))

    ;; --- Mu4e ---------------------------------------------------------
    '(mu4e-attach-number-face ((t (:inherit bivrost-strong))))
    '(mu4e-cited-1-face ((t (:inherit bivrost-faded))))
    '(mu4e-cited-2-face ((t (:inherit bivrost-faded))))
    '(mu4e-cited-3-face ((t (:inherit bivrost-faded))))
    '(mu4e-cited-4-face ((t (:inherit bivrost-faded))))
    '(mu4e-cited-5-face ((t (:inherit bivrost-faded))))
    '(mu4e-cited-6-face ((t (:inherit bivrost-faded))))
    '(mu4e-cited-7-face ((t (:inherit bivrost-faded))))
    '(mu4e-compose-header-face ((t (:inherit bivrost-faded))))
    '(mu4e-compose-separator-face ((t (:inherit bivrost-faded))))
    '(mu4e-contact-face ((t (:inherit bivrost-keyword))))
    '(mu4e-context-face ((t (:inherit bivrost-faded))))
    '(mu4e-draft-face ((t (:inherit bivrost-faded))))
    '(mu4e-flagged-face ((t (:inherit bivrost-strong))))
    '(mu4e-footer-face ((t (:inherit bivrost-faded))))
    '(mu4e-forwarded-face ((t (:inherit bivrost-default))))
    '(mu4e-header-face ((t (:inherit bivrost-default))))
    '(mu4e-header-highlight-face ((t (:inherit highlight))))
    '(mu4e-header-key-face ((t (:inherit bivrost-strong))))
    '(mu4e-header-marks-face ((t (:inherit bivrost-faded))))
    '(mu4e-header-title-face ((t (:inherit bivrost-strong))))
    '(mu4e-header-value-face ((t (:inherit bivrost-default))))
    '(mu4e-highlight-face ((t (:inherit bivrost-strong))))
    '(mu4e-link-face ((t (:inherit bivrost-keyword))))
    '(mu4e-modeline-face ((t (:inherit bivrost-faded))))
    '(mu4e-moved-face ((t (:inherit bivrost-faded))))
    '(mu4e-ok-face ((t (:inherit bivrost-faded))))
    '(mu4e-region-code ((t (:inherit bivrost-faded))))
    '(mu4e-replied-face ((t (:inherit bivrost-default))))
    '(mu4e-special-header-value-face ((t (:inherit bivrost-default))))
    '(mu4e-system-face ((t (:inherit bivrost-faded))))
    '(mu4e-title-face ((t (:inherit bivrost-strong))))
    '(mu4e-trashed-face ((t (:inherit bivrost-faded))))
    '(mu4e-unread-face ((t (:inherit bivrost-strong))))
    '(mu4e-url-number-face ((t (:inherit bivrost-faded))))
    '(mu4e-view-body-face ((t (:inherit bivrost-default))))
    '(mu4e-warning-face ((t (:inherit bivrost-strong))))

    ;; --- Ivy --------------------------------------------------------
    `(ivy-minibuffer-match-face-1 ((t (:inherit bivrost-strong))))
    `(ivy-minibuffer-match-face-2 ((t (:inherit bivrost-strong))))
    `(ivy-minibuffer-match-face-3 ((t (:inherit bivrost-strong))))
    `(ivy-minibuffer-match-face-4 ((t (:inherit bivrost-strong))))

    ;; --- Rainbow delimeters ------------------------------------------
    '(rainbow-delimiters-depth-1-face ((t (:foreground "#b9bbbc"))))
    '(rainbow-delimiters-depth-2-face ((t (:foreground "#a2a4a6"))))
    '(rainbow-delimiters-depth-3-face ((t (:foreground "#8b8e90"))))
    '(rainbow-delimiters-depth-4-face ((t (:foreground "#737779"))))
    '(rainbow-delimiters-depth-5-face ((t (:foreground "#5c6063"))))
    '(rainbow-delimiters-depth-6-face ((t (:foreground "#45494d"))))
    '(rainbow-delimiters-depth-7-face ((t (:foreground "#2d3336"))))
    '(rainbow-delimiters-depth-8-face ((t (:inherit bivrost-strong))))
    '(rainbow-delimiters-depth-9-face ((t (:inherit bivrost-strong))))
    '(rainbow-delimiters-depth-10-face ((t (:inherit bivrost-strong))))
    '(rainbow-delimiters-depth-11-face ((t (:inherit bivrost-strong))))
    '(rainbow-delimiters-depth-12-face ((t (:inherit bivrost-strong))))

    ;; --- Deft --------------------------------------------------------
    '(deft-filter-string-error-face ((t (:inherit bivrost-strong))))
    '(deft-filter-string-face ((t (:inherit bivrost-default))))
    '(deft-header-face ((t (:inherit bivrost-keyword))))
    '(deft-separator-face ((t (:inherit bivrost-faded))))
    '(deft-summary-face ((t (:inherit bivrost-faded))))
    '(deft-time-face ((t (:inherit bivrost-keyword))))
    '(deft-title-face ((t (:inherit bivrost-strong))))

    ;; --- Restructured text -------------------------------------------
    '(rst-adornment ((t (:inherit bivrost-faded))))
    '(rst-block ((t (:inherit bivrost-default))))
    '(rst-comment ((t (:inherit bivrost-faded))))
    '(rst-definition ((t (:inherit bivrost-keyword))))
    '(rst-directive ((t (:inherit bivrost-keyword))))
    '(rst-emphasis1 ((t (:inherit bivrost-faded))))
    '(rst-emphasis2 ((t (:inherit bivrost-strong))))
    '(rst-external ((t (:inherit bivrost-keyword))))
    '(rst-level-1 ((t (:inherit bivrost-strong))))
    '(rst-level-2 ((t (:inherit bivrost-strong))))
    '(rst-level-3 ((t (:inherit bivrost-strong))))
    '(rst-level-4 ((t (:inherit bivrost-strong))))
    '(rst-level-5 ((t (:inherit bivrost-strong))))
    '(rst-level-6 ((t (:inherit bivrost-strong))))
    '(rst-literal ((t (:inherit bivrost-keyword))))
    '(rst-reference ((t (:inherit bivrost-keyword))))
    '(rst-transition ((t (:inherit bivrost-default))))

    ;; --- Markdown ----------------------------------------------------
    '(markdown-blockquote-face ((t (:inherit bivrost-default))))
    '(markdown-bold-face ((t (:inherit bivrost-strong))))
    `(markdown-code-face ((t (:inherit (fixed-pitch bivrost-block)))))
    '(markdown-comment-face ((t (:inherit bivrost-faded))))
    '(markdown-footnote-marker-face ((t (:inherit bivrost-default))))
    '(markdown-footnote-text-face ((t (:inherit bivrost-default))))
    '(markdown-gfm-checkbox-face ((t (:inherit bivrost-default))))
    '(markdown-header-delimiter-face ((t (:inherit bivrost-faded))))
    '(markdown-header-face ((t (:inherit (bivrost-strong bivrost-heading-1)))))
    '(markdown-header-face-1 ((t (:inherit (bivrost-strong bivrost-heading-1)))))
    '(markdown-header-face-2 ((t (:inherit (bivrost-strong bivrost-heading-1)))))
    '(markdown-header-face-3 ((t (:inherit (bivrost-strong bivrost-heading-1)))))
    '(markdown-header-face-4 ((t (:inherit (bivrost-strong bivrost-heading-1)))))
    '(markdown-header-face-5 ((t (:inherit (bivrost-strong bivrost-heading-1)))))
    '(markdown-header-face-6 ((t (:inherit (bivrost-strong bivrost-heading-1)))))
    '(markdown-header-rule-face ((t (:inherit bivrost-default))))
    '(markdown-highlight-face ((t (:inherit bivrost-default))))
    '(markdown-hr-face ((t (:inherit bivrost-default))))
    '(markdown-html-attr-name-face ((t (:inherit bivrost-default))))
    '(markdown-html-attr-value-face ((t (:inherit bivrost-default))))
    '(markdown-html-entity-face ((t (:inherit bivrost-default))))
    '(markdown-html-tag-delimiter-face ((t (:inherit bivrost-default))))
    '(markdown-html-tag-name-face ((t (:inherit bivrost-default))))
    '(markdown-inline-code-face ((t (:inherit (fixed-pitch bivrost-strong)))))
    '(markdown-italic-face ((t (:inherit bivrost-faded))))
    '(markdown-language-info-face ((t (:inherit bivrost-default))))
    '(markdown-language-keyword-face ((t (:inherit bivrost-faded))))
    '(markdown-line-break-face ((t (:inherit bivrost-default))))
    '(markdown-link-face ((t (:inherit bivrost-keyword))))
    '(markdown-link-title-face ((t (:inherit bivrost-default))))
    '(markdown-list-face ((t (:inherit bivrost-default))))
    '(markdown-markup-face ((t (:inherit bivrost-faded))))
    '(markdown-math-face ((t (:inherit bivrost-default))))
    '(markdown-metadata-key-face ((t (:inherit bivrost-faded))))
    '(markdown-metadata-value-face ((t (:inherit bivrost-faded))))
    '(markdown-missing-link-face ((t (:inherit bivrost-default))))
    '(markdown-plain-url-face ((t (:inherit bivrost-default))))
    `(markdown-pre-face ((t (:inherit bivrost-subtle :extend t :inherit fixed-pitch))))
    '(markdown-reference-face ((t (:inherit bivrost-keyword))))
    '(markdown-strike-through-face ((t (:inherit bivrost-faded))))
    '(markdown-table-face ((t (:inherit bivrost-default))))
    '(markdown-url-face ((t (:inherit bivrost-keyword))))

    ;; --- Terminal ----------------------------------------------------
    '(term-bold ((t (:inherit bivrost-strong))))
    '(term-color-black ((t (:inherit default))))
    '(term-color-blue ((t (:foreground "#122340" :background "#122340"))))
    '(term-color-cyan ((t (:inherit default))))
    '(term-color-green ((t (:foreground "#124023" :background "#124023"))))
    '(term-color-magenta ((t (:foreground "#463c65" :background "#463c65"))))
    '(term-color-red ((t (:foreground "#5d0000" :background "#5d0000 "))))
    '(term-color-yellow ((t (:foreground "#54433a" :background "#54433a"))))

    ;; --- Haskell ----------------------------------------------------
    `(haskell-constructor-face ((t (:inherit bivrost-type))))
    `(haskell-pragma-face ((t (:inherit font-lock-comment-face))))
    `(haskell-operator-face ((t (:inherit bivrost-default))))

    ;; --- Nix ----------------------------------------------------
    `(nix-attribute-face ((t (:inherit bivrost-default))))

    ;; --- Sh ----------------------------------------------------
    `(sh-quoted-exec ((t (:inherit bivrost-default))))

    ;; --- LaTeX ----------------------------------------------------
    `(font-latex-math-face ((t (:inherit (bivrost-default fixed-pitch)))))
    `(font-latex-bold-face ((t (:inherit bold))))
    `(font-latex-warning-face ((t (:inherit (bivrost-note fixed-pitch)))))
    `(font-latex-script-char-face ((t (:inherit bivrost-default))))
    `(font-latex-sectioning-2-face ((t (:inherit bold :height 1.4))))

    ;; --- Geiser ----------------------------------------------------
    `(geiser-font-lock-autodoc-current-arg ((t :inherit bivrost-verbatim)))
    `(geiser-font-lock-autodoc-identifier ((t :inherit bivrost-keyword)))

    ;; --- Racket ----------------------------------------------------
    `(racket-keyword-argument-face ((t (:inherit bivrost-keyword)))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(run-hooks 'bivrost-after-load-hook)

(provide 'bivrost)
(provide-theme 'bivrost)

;;; bivrost-theme.el ends here
