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

;; Add missing customization options
(defcustom bivrost-italic t
  "If nil, disable the italic style."
  :group 'bivrost)

(defcustom bivrost-bold t
  "If nil, disable the bold style."
  :group 'bivrost)

(defcustom bivrost-underline t
  "If nil, disable the underline style."
  :group 'bivrost)

(defcustom bivrost-underline-wave t
  "When t, use the wave underline style to highlight warnings and error."
  :group 'bivrost)

(defcustom bivrost-hl-line-colored nil
  "When t, will display colored hl-line style instead of dim gray."
  :group 'bivrost)

(defcustom bivrost-linum-hl-line-style nil
  "When t, enable same style for hl-line and line number faces."
  :group 'bivrost)

(defcustom bivrost-italic-comments nil
  "If t, enable italic style in comments."
  :group 'bivrost)

(defcustom bivrost-comments-style 'normal
  "Sets the style of commentaries: normal (default), alt (colored), or contrast (distinguished)."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Colored" alt)
                 (const :tag "Contrast" contrast))
  :group 'bivrost)

(defcustom bivrost-git-gutter-solid nil
  "If t, display solid line to highlight git-gutter changes in fringe."
  :group 'bivrost)

(defcustom bivrost-distinct-fringe nil
  "Enable distinct background for fringe and line numbers."
  :group 'bivrost)

(defcustom bivrost-distinct-company-scrollbar nil
  "Enable distinct colors for company popup scrollbar."
  :group 'bivrost)

(defcustom bivrost-distinct-parentheses nil
  "Enable distinct colors for parentheses (i.e. rainbow delimiters package)."
  :group 'bivrost)

(defcustom bivrost-distinct-verbatim nil
  "Use distinct background color for verbatim face (org-mode) instead of colorful text."
  :group 'bivrost)

(defcustom bivrost-org-scale-headings t
  "If not-nil, scale heading size in org-mode."
  :group 'bivrost)

(defcustom bivrost-distinct-metakeys t
  "If not-nil, enable distinct color for metadata key (e.g. metakeys in org-mode).
Otherwise inherit from comments."
  :group 'bivrost)

(eval-and-compile
  (defconst bivrost-theme-colors-alist
    '(;; Basic
      (bg-main . "#272727")        ;; Primary background color - deep charcoal
      (fg-main . "#cdcecf")        ;; Primary foreground color - soft white with slight blue tint
      (bg-active . "#29394f")      ;; Active window/element background - deep navy blue
      (bg-inactive . "#212e3f")    ;; Inactive window/element background - muted slate blue
      (black . "#393b44")          ;; True black alternative - smoky charcoal
      (black2 . "#2e2e33")         ;; Slightly lighter variant of bg-main for modeline
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

;; Mode-line configuration options
(defcustom bivrost-modeline-padded nil
  "Add padding to the modeline.
Can be an integer or boolean. When non-nil, adds a padding to the modeline.
When integer, use that integer as horizontal padding. When true add default
padding, which is 4 pixels."
  :type '(choice (boolean :tag "Add default padding")
                 (integer :tag "Add custom padding"))
  :group 'bivrost)

(defcustom bivrost-modeline-border nil
  "When non-nil, add a border to the modeline."
  :type 'boolean
  :group 'bivrost)

(defmacro bivrost-with-color-variables (&rest body)
  (declare (indent 0))
  `(let (,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   bivrost-theme-colors-alist))
     ,@body))

;; Update existing faces with proper default values
(defface bivrost-critical
  '((t (:foreground "#c94f6d" :weight bold)))
  "Critical face for information that requires immediate action."
  :group 'bivrost)

(defface bivrost-critical-i
  '((t (:background "#c94f6d" :foreground "#272727")))
  "Critical face inversed."
  :group 'bivrost)

(defface bivrost-bold
  '((t (:weight bold)))
  "Bold face."
  :group 'bivrost)

(defface bivrost-strong
  '((t (:foreground "#cdcecf" :weight bold)))
  "Strong face for information of a structural nature."
  :group 'bivrost)

(defface bivrost-strong-i
  '((t (:background "#cdcecf" :foreground "#272727" :weight bold)))
  "Strong face inversed."
  :group 'bivrost)

(defface bivrost-keyword
  '((t (:foreground "#9d79d6")))
  "Default keyword face."
  :group 'bivrost)

(defface bivrost-type
  '((t (:foreground "#b59d61")))
  "Default type face."
  :group 'bivrost)

(defface bivrost-verbatim
  '((t (:foreground "#81b29a")))
  "Face used for things like strings."
  :group 'bivrost)

(defface bivrost-faded
  '((t (:foreground "#71839b")))
  "Faded face for less important information."
  :group 'bivrost)

(defface bivrost-faded-i
  '((t (:background "#71839b" :foreground "#272727")))
  "Faded face inversed."
  :group 'bivrost)

(defface bivrost-subtle
  '((t (:background "#2e2e33")))
  "Subtle face is used to suggest a physical area on the screen."
  :group 'bivrost)

(defface bivrost-subtle-i
  '((t (:foreground "#2e2e33")))
  "Subtle face inversed."
  :group 'bivrost)

(defface bivrost-default
  '((t (:foreground "#cdcecf")))
  "Default face."
  :group 'bivrost)

(defface bivrost-default-i
  '((t (:background "#cdcecf" :foreground "#272727")))
  "Default face inversed."
  :group 'bivrost)

(defface bivrost-highlight
  '((t (:background "#3d3d3d")))
  "Default highlight face."
  :group 'bivrost)

(defface bivrost-warning
  '((t (:foreground "#f4a261")))
  "Warning face."
  :group 'bivrost)

(defface bivrost-error
  '((t (:foreground "#c94f6d")))
  "Error face."
  :group 'bivrost)

(defface bivrost-note
  '((t (:foreground "#63cdcf")))
  "Note face."
  :group 'bivrost)

(defface bivrost-block
  '((t (:background "#29394f")))
  "Default block face."
  :group 'bivrost)

(defface bivrost-button
  '((t (:foreground "#cdcecf" :background "#39506d" :box (:line-width 1 :style released-button))))
  "Default button face."
  :group 'bivrost)

(defface bivrost-button-hover
  '((t (:foreground "#cdcecf" :background "#4a617e" :box (:line-width 1 :style released-button))))
  "Hover button face."
  :group 'bivrost)

(defface bivrost-button-pressed
  '((t (:foreground "#cdcecf" :background "#29394f" :box (:line-width 1 :style pressed-button))))
  "Pressed button face."
  :group 'bivrost)

(defface bivrost-border
  '((t (:foreground "#131a24")))
  "Border face."
  :group 'bivrost)

(defface bivrost-bar
  '((t (:foreground "#cdcecf" :background "#2e2e33")))
  "Face used for active mode-line and tab-bar"
  :group 'bivrost)

(defface bivrost-bar-inactive
  '((t (:foreground "#71839b" :background "#393b44")))
  "Face used for inactive mode-line and tab-bar"
  :group 'bivrost)

(defface bivrost-link
  '((t (:foreground "#719cd6" :underline t)))
  "Face used for links."
  :group 'bivrost)

(defface bivrost-paren-face
  '((t (:foreground "grey70")))
  "Face used to dim parentheses."
  :group 'bivrost)

;; Add missing faces
(defface bivrost-boolean
  '((t (:foreground "#63cdcf")))
  "Face to highlight boolean values"
  :group 'bivrost)

(defface bivrost-heading-1
  '((t (:foreground "#b59d61" :weight bold :height 1.3)))
  "Face for level 1 headings."
  :group 'bivrost)

(defface bivrost-heading-2
  '((t (:foreground "#b59d61" :weight bold :height 1.2)))
  "Face for level 2 headings."
  :group 'bivrost)

(defface bivrost-heading-3
  '((t (:foreground "#b59d61" :weight bold :height 1.1)))
  "Face for level 3 headings."
  :group 'bivrost)

(defface bivrost-heading-4
  '((t (:foreground "#b59d61" :weight bold)))
  "Face for level 4 headings."
  :group 'bivrost)

;; Mode-line specific faces
(defface bivrost-modeline-evil-normal
  '((t (:foreground "#272727" :background "#81b29a" :weight bold)))
  "Face for Evil normal state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-insert
  '((t (:foreground "#272727" :background "#719cd6" :weight bold)))
  "Face for Evil insert state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-visual
  '((t (:foreground "#272727" :background "#9d79d6" :weight bold)))
  "Face for Evil visual state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-replace
  '((t (:foreground "#272727" :background "#c94f6d" :weight bold)))
  "Face for Evil replace state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-motion
  '((t (:foreground "#272727" :background "#63cdcf" :weight bold)))
  "Face for Evil motion state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-operator
  '((t (:foreground "#272727" :background "#b59d61" :weight bold)))
  "Face for Evil operator state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-emacs
  '((t (:foreground "#272727" :background "#f4a261" :weight bold)))
  "Face for Evil emacs state in mode-line."
  :group 'bivrost)

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

(defun bivrost ()
  "Load bivrost theme."
  (interactive)

  (when bivrost-use-faded-lisp-parens
    (add-hook 'lisp-data-mode-hook #'bivrost--font-lock-add-paren)
    (add-hook 'scheme-mode-hook #'bivrost--font-lock-add-paren))

  (load-theme 'bivrost t)
  (run-hooks 'bivrost-after-load-hook))

;; --- Faces ---------------------------------------------------------
(bivrost-with-color-variables
 (let ((bivrost-heading-1-height (if bivrost-use-more-bold 1.0 1.1))
       (faded-color (if bivrost-use-more-fading gray-silver gray-dark)))
   (custom-theme-set-faces
    'bivrost

    ;; --- Base ---------------------------------------------------------
    ;; Keeping these core faces as they define your theme's base appearance
    `(default ((t (:background ,bg-main :foreground ,fg-main))))
    `(cursor ((t (:foreground ,bg-main :background ,fg-main))))
    
    ;; Bivrost-specific faces (keeping these as they're unique to your theme)
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
    `(bivrost-heading-1 ((t (:inherit bivrost-strong :height ,bivrost-heading-1-height))))
    `(bivrost-block ((t (:background ,bg-active :foreground ,fg-main :extend t))))
    `(bivrost-border ((t (:foreground ,gray-light :box (:color ,gray-silver :line-width 1)))))
    `(bivrost-bar ((t (:foreground ,fg-main :box (:color ,gray-silver :line-width 1)))))
    `(bivrost-bar-inactive ((t (:foreground "#535c65" :background ,bg-inactive :box (:color ,gray-silver :line-width 1)))))
    `(bivrost-button ((t (:box (:color ,gray-silver) :background ,bg-inactive))))
    `(bivrost-button-pressed ((t (:box (:color ,gray-dark) :background ,bg-inactive))))
    `(bivrost-button-hover ((t (:background ,bg-inactive :box (:color ,gray-silver)))))
    
    ;; Font-lock faces from Kaolin theme (replacing your basic ones)
    `(font-lock-bracket-face           ((t (:foreground ,purple))))
    `(font-lock-builtin-face           ((t (:foreground ,blue-alt))))
    `(font-lock-comment-delimiter-face ((t (:background unspecified :foreground ,gray :italic t))))
    `(font-lock-comment-face           ((t (:background unspecified :foreground ,gray :italic t))))
    `(font-lock-constant-face          ((t (:foreground ,yellow-dark))))
    `(font-lock-doc-face               ((t (:foreground ,green-faint))))
    `(font-lock-function-name-face     ((t (:foreground ,blue))))
    `(font-lock-keyword-face           ((t (:foreground ,purple))))
    `(font-lock-negation-char-face     ((t (:foreground ,red))))
    `(font-lock-number-face            ((t (:foreground ,green-light))))
    `(font-lock-operator-face          ((t (:foreground ,blue))))
    `(font-lock-preprocessor-face      ((t (:foreground ,gray-silver))))
    `(font-lock-reference-face         ((t (:foreground ,yellow-dark))))
    `(font-lock-string-face            ((t (:foreground ,yellow-dark))))
    `(font-lock-type-face              ((t (:foreground ,green))))
    `(font-lock-variable-name-face     ((t (:foreground ,gray-light))))
    `(font-lock-warning-face           ((t (:background unspecified :foreground ,orange))))
    `(font-lock-regexp-grouping-construct ((t (:foreground ,green-light))))
    `(font-lock-regexp-grouping-backslash ((t (:foreground ,green-light))))

    ;; General UI faces (replacing overlapping ones)
    `(highlight ((t (:background ,gray))))
    `(warning ((t (:foreground ,orange))))
    `(bivrost-warning ((t (:foreground ,orange)))) ;; Keeping this for consistency
    `(error ((t (:foreground ,red))))
    `(bivrost-error ((t (:foreground ,red)))) ;; Keeping this for consistency
    `(bivrost-critical ((t (:foreground ,bg-main :background ,red))))
    `(bivrost-critical-i ((t (:foreground ,red))))
    `(bivrost-note ((t (:foreground ,green-light))))
    `(shadow ((t (:foreground ,gray))))
    `(file-name-shadow ((t (:foreground ,gray))))
    `(region ((t (:background ,blue-light :foreground ,fg-main))))
    `(secondary-selection ((t (:background ,gray-dark :foreground ,fg-main))))
    `(vertical-border ((t (:foreground ,black-alt))))
    `(window-divider ((t (:foreground ,black-alt))))
    `(minibuffer-prompt ((t (:background unspecified :foreground ,blue :bold ,(if bivrost-use-more-bold t nil)))))
    `(bold ((t (:bold ,(if bivrost-use-more-bold t nil)))))
    `(italic ((t (:italic t))))
    `(default-italic ((t (:italic t))))
    `(bold-italic ((t (:bold ,(if bivrost-use-more-bold t nil) :italic t))))
    `(link ((t (:foreground ,blue :underline t))))
    `(bivrost-link ((t (:foreground ,blue :underline t)))) ;; Keeping this for consistency
    `(link-visited ((t (:foreground ,blue :underline nil))))
    `(success ((t (:background unspecified :foreground ,green))))
    `(escape-glyph ((t (:background unspecified :foreground ,blue-alt))))
    `(trailing-whitespace ((t (:background ,red))))
    `(fill-column-indicator ((t (:foreground ,gray-dark))))

    ;; Emacs UI faces
    `(package-name ((t (:foreground ,blue :underline nil))))
    `(button ((t (:foreground ,blue :underline t))))
    `(custom-button ((t (:background unspecified :foreground ,blue 
				     :box (:line-width 1 :color ,blue :style nil) :height 0.9))))
    `(custom-button-mouse ((t (:background unspecified :foreground ,blue-alt 
					   :box (:line-width 1 :color ,blue-alt :style nil)))))
    `(custom-button-pressed ((t (:background unspecified :foreground ,blue-alt 
					     :box (:line-width 1 :color ,black-alt :style nil)))))
    `(custom-button-unraised ((t (:background unspecified :foreground ,blue 
					      :box (:line-width 1 :color ,blue :style nil) :height 0.9))))
    `(custom-button-pressed-unraised ((t (:background unspecified :foreground ,blue-alt 
						      :box (:line-width 1 :color ,black-alt :style nil)))))
    `(custom-group-tag ((t (:foreground ,blue :height 1.2 :weight bold))))
    `(custom-group-subtitle ((t (:foreground ,blue :height 1.0 :weight bold))))
    `(custom-variable-button ((t (:foreground ,blue :underline t))))
    `(custom-comment ((t (:background ,gray-dark :foreground ,fg-main))))
    `(custom-comment-tag ((t (:foreground ,gray))))
    `(custom-documentation ((t (:foreground ,fg-main))))
    `(custom-visibility ((t (:background unspecified :foreground ,blue :underline t))))
    `(custom-state ((t (:background unspecified :foreground ,yellow-dark))))
    `(custom-changed ((t (:background unspecified :foreground ,blue))))
    `(custom-set ((t (:background unspecified :foreground ,green))))
    `(custom-themed ((t (:background unspecified :foreground ,green))))
    `(custom-invalid ((t (:background unspecified :foreground ,red))))
    `(custom-variable-tag ((t (:foreground ,gray-light))))
    `(custom-variable-obsolete ((t (:foreground ,gray))))
    `(widget-documentation ((t (:background unspecified :foreground ,gray-light))))
    `(widget-button-pressed ((t (:background unspecified :foreground ,blue-alt))))
    `(widget-field ((t (:background ,bg-active :foreground ,gray-light 
				    :box (:line-width 2 :color ,gray-dark :style nil)))))
    `(widget-single-line-field ((t (:background ,bg-active :foreground ,gray-light 
						:box (:line-width 2 :color ,gray-dark :style nil)))))

    ;; Dashboard
    `(dashboard-heading   ((t (:foreground ,blue))))
    `(dashboard-navigator ((t (:foreground ,gray-silver))))
    `(dashboard-footer    ((t (:foreground ,yellow-dark))))

    ;; Compilation
    `(compilation-column-number  ((t (:foreground ,gray-light))))
    `(compilation-line-number    ((t (:foreground ,green-light))))
    `(compilation-info           ((t (:foreground ,green))))
    `(compilation-warning        ((t (:foreground ,orange))))
    `(compilation-error          ((t (:foreground ,red :weight bold))))
    `(compilation-mode-line-exit ((t (:foreground ,green))))
    `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))

    ;; Dired
    `(dired-header     ((t (:foreground ,blue :weight bold))))
    `(dired-directory  ((t (:foreground ,purple))))
    `(dired-ignored    ((t (:foreground ,gray))))
    `(dired-flagged    ((t (:foreground ,red))))
    `(dired-mark       ((t (:foreground ,green-light :weight bold))))
    `(dired-marked     ((t (:foreground ,blue :weight bold))))
    `(dired-perm-write ((t (:foreground ,fg-main :underline t))))
    `(dired-symlink    ((t (:foreground ,blue))))
    `(dired-warning    ((t (:foreground ,orange))))

    ;; dired-plus
    `(diredp-dir-name               ((t (:foreground ,purple :weight bold :strike-through nil))))
    `(diredp-dir-heading            ((t (:foreground ,blue :weight bold :strike-through nil))))
    `(diredp-file-name              ((t (:foreground ,fg-main :strike-through nil))))
    `(diredp-file-suffix            ((t (:foreground ,yellow-dark))))
    `(diredp-ignored-file-name      ((t (:foreground ,gray))))
    `(diredp-omit-file-name         ((t (:foreground ,gray))))
    `(diredp-compressed-file-suffix ((t (:foreground ,gray))))
    `(diredp-symlink                ((t (:foreground ,blue))))
    `(diredp-read-priv              ((t (:foreground ,green))))
    `(diredp-write-priv             ((t (:foreground ,blue))))
    `(diredp-exec-priv              ((t (:foreground ,red))))
    `(diredp-executable-tag         ((t (:foreground ,red))))
    `(diredp-rare-priv              ((t (:foreground ,red :weight bold))))
    `(diredp-dir-priv               ((t (:foreground ,purple :weight bold))))
    `(diredp-other-priv             ((t (:foreground ,orange))))
    `(diredp-no-priv                ((t (:foreground ,gray))))
    `(diredp-number                 ((t (:foreground ,green-light))))
    `(diredp-date-time              ((t (:foreground ,gray-silver))))
    `(diredp-flag-mark              ((t (:background ,gray-dark :foreground ,blue))))
    `(diredp-flag-mark-line         ((t (:background ,gray-dark))))
    `(diredp-deletion               ((t (:background unspecified :foreground ,red :underline t))))
    `(diredp-deletion-file-name     ((t (:background unspecified :foreground ,red :underline t))))
    `(diredp-autofile-name          ((t (:foreground ,green-light :underline t))))

    ;; diredfl
    `(diredfl-autofile-name          ((t (:foreground ,green-light :underline t))))
    `(diredfl-compressed-file-name   ((t (:foreground ,gray))))
    `(diredfl-compressed-file-suffix ((t (:foreground ,gray))))
    `(diredfl-date-time              ((t (:foreground ,gray-silver))))
    `(diredfl-deletion               ((t (:background unspecified :foreground ,red :underline t))))
    `(diredfl-deletion-file-name     ((t (:background unspecified :foreground ,red :underline t))))
    `(diredfl-dir-heading            ((t (:foreground ,blue :weight bold :strike-through nil))))
    `(diredfl-dir-name               ((t (:foreground ,purple :weight bold :strike-through nil))))
    `(diredfl-dir-priv               ((t (:foreground ,purple :weight bold))))
    `(diredfl-exec-priv              ((t (:foreground ,red))))
    `(diredfl-executable-tag         ((t (:foreground ,red))))
    `(diredfl-file-name              ((t (:foreground ,fg-main :strike-through nil))))
    `(diredfl-file-suffix            ((t (:foreground ,yellow-dark))))
    `(diredfl-flag-mark              ((t (:background ,blue-light :foreground ,blue))))
    `(diredfl-flag-mark-line         ((t (:background ,blue-light))))
    `(diredfl-ignored-file-name      ((t (:foreground ,gray))))
    `(diredfl-link-priv              ((t (:foreground ,blue))))
    `(diredfl-no-priv                ((t (:foreground ,gray))))
    `(diredfl-number                 ((t (:foreground ,green-light))))
    `(diredfl-other-priv             ((t (:foreground ,orange))))
    `(diredfl-rare-priv              ((t (:foreground ,red :weight bold))))
    `(diredfl-read-priv              ((t (:foreground ,green))))
    `(diredfl-symlink                ((t (:foreground ,blue))))
    `(diredfl-tagged-autofile-name   ((t (:foreground ,green-light :underline t))))
    `(diredfl-write-priv             ((t (:foreground ,blue))))

    ;; Highlighting
    `(highlight                ((t (:background ,gray-dark :foreground ,fg-main))))
    `(lazy-highlight           ((t (:background ,gray-dark :foreground ,blue))))
    `(hl-line                  ((t (:background ,blue-light))))
    `(highlight-numbers-number ((t (:foreground ,green-light))))
    `(highlight-quoted-quote   ((t (:foreground ,blue-alt))))
    `(highlight-quoted-symbol  ((t (:foreground ,purple))))
    `(highlight-symbol-face    ((t (:background ,gray-dark))))

    ;; hi-lock
    `(hi-black-hb ((t (:weight bold))))
    `(hi-aquamarine ((t (:foreground ,bg-main :background ,blue-alt))))
    `(hi-blue ((t (:foreground ,bg-main :background ,blue))))
    `(hi-blue-b ((t (:foreground ,blue :weight bold))))
    `(hi-green ((t (:foreground ,bg-main :background ,green))))
    `(hi-green-b ((t (:foreground ,green :weight bold))))
    `(hi-pink ((t (:foreground ,bg-main :background ,purple))))
    `(hi-red-b ((t (:foreground ,red :weight bold))))
    `(hi-yellow ((t (:foreground ,bg-main :background ,yellow-dark))))
    `(hi-salmon ((t (:foreground ,bg-main :background ,blue))))

    ;; Highlight indent guides
    `(highlight-indent-guides-odd-face        ((t (:background ,gray-dark))))
    `(highlight-indent-guides-even-face       ((t (:background ,gray-dark))))
    `(highlight-indent-guides-character-face  ((t (:foreground ,gray-dark))))

    ;; Indent-guide
    `(indent-guide-face ((t (:foreground ,gray-dark))))

    ;; Highlighting indentation
    `(highlight-indentation-face                ((t (:background ,bg-active))))
    `(highlight-indentation-current-column-face ((t (:background ,gray-dark))))

    ;; Native line numbers
    `(line-number ((t (:background ,bg-main :foreground ,gray-dark))))
    `(line-number-current-line ((t (:background ,bg-main :foreground ,blue 
						,@(when bivrost-use-more-bold '(:weight bold))))))

    ;; Which-function-mode
    `(which-func ((t (:foreground ,yellow-dark))))

    ;; Which-key
    `(which-key-key-face                   ((t (:foreground ,purple ,@(when bivrost-use-more-bold '(:weight bold))))))
    `(which-key-group-description-face     ((t (:foreground ,blue))))
    `(which-key-local-map-description-face ((t (:foreground ,blue-alt))))
    `(which-key-command-description-face   ((t (:foreground ,green))))

    ;; Ruler-mode
    `(ruler-mode-default        ((t (:background ,bg-active :foreground ,gray-silver))))
    `(ruler-mode-column-number  ((t (:foreground ,blue))))
    `(ruler-mode-current-column ((t (:foreground ,orange))))
    `(ruler-mode-fill-column    ((t (:foreground ,red))))
    `(ruler-mode-comment-column ((t (:foreground ,blue-alt))))
    `(ruler-mode-fringes        ((t (:foreground ,green))))
    `(ruler-mode-pad            ((t (:foreground ,blue))))
    `(ruler-mode-tab-stop       ((t (:foreground ,purple))))
    `(ruler-mode-goal-column    ((t (:foreground ,red))))

    ;; Message faces
    `(message-cited-text           ((t (:foreground ,gray))))
    `(message-header-subject       ((t (:inherit message-header-other :weight bold :foreground ,purple))))
    `(message-header-to            ((t (:inherit message-header-other :weight bold :foreground ,yellow-dark))))
    `(message-header-cc            ((t (:inherit message-header-to))))
    `(message-header-name          ((t (:foreground ,blue))))
    `(message-header-newsgroups    ((t (:foreground ,green :slant normal))))
    `(message-header-other         ((t (:foreground ,fg-main :background unspecified :weight normal))))
    `(message-mml                  ((t (:foreground ,gray))))
    `(message-separator            ((t (:foreground ,gray))))

    ;; debbugs
    `(debbugs-gnu-done      ((t (:foreground ,gray))))
    `(debbugs-gnu-forwarded ((t (:foreground ,yellow-dark))))
    `(debbugs-gnu-handled   ((t (:foreground ,green))))
    `(debbugs-gnu-new       ((t (:foreground ,red))))
    `(debbugs-gnu-pending   ((t (:foreground ,blue-alt))))
    `(debbugs-gnu-stale-1   ((t (:foreground ,purple))))
    `(debbugs-gnu-stale-2   ((t (:foreground ,blue))))
    `(debbugs-gnu-stale-3   ((t (:foreground ,green-mint))))
    `(debbugs-gnu-stale-4   ((t (:foreground ,green-light))))
    `(debbugs-gnu-stale-5   ((t (:foreground ,green-faint))))
    `(debbugs-gnu-tagged    ((t (:foreground ,orange))))

    ;; Modeline
    `(mode-line           ((t (:background ,(if bivrost-modeline-padded black2 bg-main) :foreground ,gray-light :bold nil
					   :box (:line-width ,(if bivrost-modeline-padded 
								  (if (integerp bivrost-modeline-padded) 
								      bivrost-modeline-padded 4) 2)
							     :color ,(if (and (not bivrost-modeline-padded) bivrost-modeline-border) 
									 black-alt (if bivrost-modeline-padded black2 bg-main))
							     :style nil)))))

    `(mode-line-active    ((t (:inherit mode-line))))
    `(mode-line-inactive  ((t (:background ,black :foreground ,gray-dark :bold nil
					   :box (:line-width ,(if bivrost-modeline-padded 
								  (if (integerp bivrost-modeline-padded) 
								      bivrost-modeline-padded 4) 2)
							     :color ,(if (and (not bivrost-modeline-padded) bivrost-modeline-border) 
									 black-alt black)
							     :style nil)))))
    `(mode-line-buffer-id ((t (:background unspecified :foreground ,fg-main :bold nil))))
    `(mode-line-highlight ((t (:foreground ,green :box nil :bold nil))))
    `(mode-line-emphasis  ((t (:foreground ,green))))

    ;; Powerline
    `(powerline-active0   ((t (:background ,black2 :foreground ,gray-light))))
    `(powerline-active1   ((t (:background ,black2 :foreground ,fg-main))))
    `(powerline-active2   ((t (:background ,(if bivrost-modeline-padded black2 bg-main) :foreground ,fg-main))))
    `(powerline-inactive0 ((t (:inherit mode-line-inactive))))
    `(powerline-inactive1 ((t (:inherit mode-line-inactive))))
    `(powerline-inactive2 ((t (:inherit mode-line-inactive))))

    ;; Spaceline
    `(spaceline-highlight-face ((t (:background ,black2 :foreground ,green :bold nil))))

    ;; Highlight TODOs
    `(fic-face         ((t (:background unspecified :foreground ,red :bold ,bivrost-bold))))
    `(fic-author-face  ((t (:background unspecified :foreground ,red :bold ,bivrost-bold))))
    `(hl-todo          ((t (:background unspecified :foreground ,red :bold ,bivrost-bold))))

    ;; Additional completion
    `(ac-completion-face    ((t (:foreground ,purple :underline ,bivrost-underline))))
    `(icomplete-first-match ((t (:inherit bivrost-match))))
    `(icompletep-determined ((t (:foreground ,blue-alt))))

    ;; info faces
    `(Info-quoted      ((t (:foreground ,blue-alt))))
    `(info-quoted-name ((t (:foreground ,blue-alt))))
    `(info-string      ((t (:foreground ,green))))
    `(info-menu-star   ((t (:foreground ,red))))
    `(info-index-match ((t (:inherit bivrost-match))))
    `(info-node        ((t (:foreground ,blue))))
    `(info-menu-header ((t (:foreground ,purple :weight ,(if bivrost-bold 'bold 'normal) :height 1.1))))
    `(info-title-1     ((t (:foreground ,yellow-dark :weight ,(if bivrost-bold 'bold 'normal) :height 1.3))))
    `(info-title-2     ((t (:foreground ,yellow-dark :weight ,(if bivrost-bold 'bold 'normal) :height 1.2))))
    `(info-title-3     ((t (:foreground ,yellow-dark :weight ,(if bivrost-bold 'bold 'normal) :height 1.1))))
    `(info-title-4     ((t (:foreground ,yellow-dark :weight ,(if bivrost-bold 'bold 'normal)))))

    ;; corfu
    `(corfu-background ((t (:background ,black2))))
    `(corfu-echo       ((t (:foreground ,gray-light))))
    `(corfu-current    ((t (:background ,bg-active :foreground ,fg-main))))
    `(corfu-border     ((t (:background ,black))))

    ;; all-the-icons
    `(all-the-icons-red      ((t (:foreground ,red))))
    `(all-the-icons-red-alt  ((t (:foreground ,red))))
    `(all-the-icons-lred     ((t (:foreground ,red-faint))))
    `(all-the-icons-dred     ((t (:foreground ,red))))
    `(all-the-icons-green    ((t (:foreground ,green))))
    `(all-the-icons-lgreen   ((t (:foreground ,green-light))))
    `(all-the-icons-dgreen   ((t (:foreground ,green-faint))))
    `(all-the-icons-yellow   ((t (:foreground ,yellow-dark))))
    `(all-the-icons-lyellow  ((t (:foreground ,yellow-dark))))
    `(all-the-icons-dyellow  ((t (:foreground ,orange))))
    `(all-the-icons-orange   ((t (:foreground ,orange))))
    `(all-the-icons-lorange  ((t (:foreground ,orange))))
    `(all-the-icons-dorange  ((t (:foreground ,orange))))
    `(all-the-icons-blue     ((t (:foreground ,blue))))
    `(all-the-icons-blue-alt ((t (:foreground ,blue-alt))))
    `(all-the-icons-lblue    ((t (:foreground ,blue))))
    `(all-the-icons-dblue    ((t (:foreground ,blue-light))))
    `(all-the-icons-maroon   ((t (:foreground ,red))))
    `(all-the-icons-lmaroon  ((t (:foreground ,red-faint))))
    `(all-the-icons-dmaroon  ((t (:foreground ,red))))
    `(all-the-icons-purple   ((t (:foreground ,purple))))
    `(all-the-icons-lpurple  ((t (:foreground ,purple))))
    `(all-the-icons-dpurple  ((t (:foreground ,purple))))
    `(all-the-icons-cyan     ((t (:foreground ,blue-alt))))
    `(all-the-icons-cyan-alt ((t (:foreground ,blue-alt))))
    `(all-the-icons-lcyan    ((t (:foreground ,blue-alt))))
    `(all-the-icons-dcyan    ((t (:foreground ,blue-alt))))
    `(all-the-icons-pink     ((t (:foreground ,red))))
    `(all-the-icons-lpink    ((t (:foreground ,red-faint))))
    `(all-the-icons-dpink    ((t (:foreground ,red))))
    `(all-the-icons-silver   ((t (:foreground ,gray))))
    `(all-the-icons-lsilver  ((t (:foreground ,gray-light))))
    `(all-the-icons-dsilver  ((t (:foreground ,gray-silver))))

    ;; Magit
    ;; Magit sections
    `(magit-section-highlight         ((t (:background ,blue-light))))
    `(magit-section-heading           ((t (:foreground ,blue :bold ,bivrost-bold))))
    `(magit-section-heading-selection ((t (:foreground ,purple :bold ,bivrost-bold))))
    `(magit-item-highlight            ((t (:background ,gray-dark))))
    `(magit-blame-heading             ((t (:background ,gray-dark :foreground ,gray-light))))

    ;; Magit branches
    `(magit-branch                     ((t (:foreground ,blue-alt))))
    `(magit-branch-local               ((t (:foreground ,blue-alt))))
    `(magit-branch-remote              ((t (:foreground ,green-mint))))
    `(magit-hunk-heading               ((t (:background ,gray-dark))))
    `(magit-hunk-heading-highlight     ((t (:background ,gray-dark))))
    `(magit-diff-hunk-heading          ((t (:background ,gray-dark))))
    `(magit-diff-hunk-heading-highlight ((t (:background ,black-alt :foreground ,blue))))
    
    `(magit-diff-file-heading          ((t (:foreground ,fg-main :bold ,bivrost-bold))))
    `(magit-diff-file-heading-highlight ((t (:background ,gray-dark :bold ,bivrost-bold))))
    
    ;; Diff faces
    `(magit-diff-base               ((t (:background ,yellow-dark :foreground ,gray-light))))
    `(magit-diff-base-highlight     ((t (:background ,yellow-dark :foreground ,fg-main))))
    `(magit-diff-context            ((t (:background ,bg-main :foreground ,gray))))
    `(magit-diff-context-highlight  ((t (:background ,blue-light :foreground ,gray-light))))
    `(magit-diff-added              ((t (:background ,bg-main :foreground ,green))))
    `(magit-diff-added-highlight    ((t (:background ,green-faint :foreground ,fg-main))))
    `(magit-diff-removed            ((t (:background ,bg-main :foreground ,red))))
    `(magit-diff-removed-highlight  ((t (:background ,red :foreground ,fg-main))))
    `(magit-diffstat-added          ((t (:foreground ,green))))
    `(magit-diffstat-removed        ((t (:foreground ,red))))
    
    ;; Other magit faces
    `(magit-tag                    ((t (:foreground ,orange))))
    `(magit-hash                   ((t (:inherit 'magit-tag))))
    `(magit-dimmed                 ((t (:inherit 'shadow))))
    `(magit-log-author             ((t (:foreground ,blue))))
    `(magit-log-date               ((t (:foreground ,gray-light))))
    `(magit-log-graph              ((t (:foreground ,green-mint))))

    `(magit-process-ok             ((t (:foreground ,green :bold ,bivrost-bold))))
    `(magit-process-ng             ((t (:foreground ,red :bold ,bivrost-bold))))

    ;; Reflog specific faces
    `(magit-reflog-amend           ((t (:foreground ,purple))))
    `(magit-reflog-checkout        ((t (:foreground ,blue))))
    `(magit-reflog-cherry-pick     ((t (:foreground ,green))))
    `(magit-reflog-commit          ((t (:foreground ,green))))
    `(magit-reflog-merge           ((t (:foreground ,green))))
    `(magit-reflog-rebase          ((t (:foreground ,purple))))
    `(magit-reflog-remote          ((t (:foreground ,blue-alt))))
    `(magit-reflog-reset           ((t (:foreground ,red :bold ,bivrost-bold))))
    `(magit-reflog-other           ((t (:foreground ,blue-alt))))
    `(magit-refname                ((t (:foreground ,gray-light))))

    ;; Sequence faces
    `(magit-sequence-head          ((t (:foreground ,blue))))
    `(magit-sequence-drop          ((t (:foreground ,red))))
    `(magit-sequence-part          ((t (:foreground ,yellow-dark))))
    `(magit-sequence-stop          ((t (:foreground ,green))))

    ;; Special states
    `(magit-cherry-equivalent      ((t (:foreground ,purple))))
    `(magit-cherry-unmatched       ((t (:foreground ,blue-alt))))
    `(magit-bisect-good            ((t (:foreground ,green-mint))))
    `(magit-bisect-bad             ((t (:foreground ,red))))
    `(magit-bisect-skip            ((t (:foreground ,green-light))))
    `(magit-signature-good         ((t (:foreground ,green))))
    `(magit-signature-bad          ((t (:foreground ,red))))
    `(magit-signature-untrusted    ((t (:foreground ,blue-alt))))

    `(magit-popup-key              ((t (:foreground ,blue))))

    ;; Magit Transient (replaced the old popup)
    `(transient-heading            ((t (:foreground ,blue))))
    `(transient-key                ((t (:foreground ,blue-alt))))
    `(transient-argument           ((t (:foreground ,purple))))
    `(transient-enabled-suffix     ((t (:background ,green :foreground ,bg-main))))
    `(transient-disabled-suffix    ((t (:background ,red :foreground ,bg-main))))

    ;; Flymake
    `(flymake-note    ((t (:underline ,(if bivrost-underline 
					   `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,green)
					 nil)))))
    `(flymake-warning ((t (:underline ,(if bivrost-underline 
					   `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,yellow-dark)
					 nil)))))
    `(flymake-error   ((t (:underline ,(if bivrost-underline 
					   `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,red)
					 nil)))))

    ;; Flycheck
    `(flycheck-info           ((t (:underline ,(if bivrost-underline 
						   `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,green)
						 nil)))))
    `(flycheck-warning        ((t (:underline ,(if bivrost-underline 
						   `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,yellow-dark)
						 nil)))))
    `(flycheck-error          ((t (:underline ,(if bivrost-underline 
						   `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,red)
						 nil)))))
    `(flycheck-fringe-error   ((t (:foreground ,red))))
    `(flycheck-fringe-warning ((t (:foreground ,yellow-dark))))
    `(flycheck-fringe-info    ((t (:foreground ,green))))

    ;; Flycheck posframe
    `(flycheck-posframe-face            ((t (:inherit 'default))))
    `(flycheck-posframe-background-face ((t (:background ,black-alt))))
    `(flycheck-posframe-info-face       ((t (:inherit 'flycheck-posframe-face :foreground ,fg-main))))
    `(flycheck-posframe-warning-face    ((t (:inherit 'flycheck-posframe-face :foreground ,yellow-dark))))
    `(flycheck-posframe-error-face      ((t (:inherit 'flycheck-posframe-face :foreground ,red))))

    ;; Flyspell
    `(flyspell-duplicate ((t (:underline ,(if bivrost-underline 
                                              `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,yellow-dark)
                                            nil)))))
    `(flyspell-incorrect ((t (:underline ,(if bivrost-underline 
                                              `(:style ,(if bivrost-underline-wave 'wave 'line) :color ,red)
                                            nil)))))

    ;; Hydra
    `(hydra-face-red      ((t (:foreground ,red))))
    `(hydra-face-teal     ((t (:foreground ,blue-alt))))
    `(hydra-face-blue     ((t (:foreground ,blue))))
    `(hydra-face-pink     ((t (:foreground ,red-faint))))
    `(hydra-face-amaranth ((t (:foreground ,purple))))

    ;; Hydra-posframe
    `(hydra-posframe-face        ((t (:background ,blue-light :foreground ,fg-main))))
    `(hydra-posframe-border-face ((t (:background ,blue-light))))

    ;; Ido
    `(ido-indicator ((t (:foreground ,blue-alt))))
    `(ido-first-match ((t (:foreground ,yellow-dark :weight ,(if bivrost-bold 'bold 'normal)))))
    `(ido-only-match ((t (:foreground ,blue-alt))))
    `(ido-subdir ((t (:foreground ,purple))))

    ;; Gnus
    `(gnus-header-content ((t (:foreground ,purple))))
    `(gnus-header-from ((t (:foreground ,blue))))
    `(gnus-header-name ((t (:foreground ,yellow-dark))))
    `(gnus-header-subject ((t (:foreground ,green :weight ,(if bivrost-bold 'bold 'normal)))))

    ;; ffap
    `(ffap ((t (:foreground ,gray-light))))

    ;; Js-mode
    `(js2-private-function-call    ((t (:foreground ,blue-alt))))
    `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,green))))
    `(js2-jsdoc-html-tag-name      ((t (:foreground ,purple))))
    `(js2-external-variable        ((t (:foreground ,yellow-dark))))
    `(js2-function-param           ((t (:foreground ,blue-alt))))
    `(js2-error                    ((t (:underline (:color ,red :style ,(if bivrost-underline-wave 'wave bivrost-underline))))))
    `(js2-function-call            ((t (:foreground ,green))))
    `(js2-object-property          ((t (:foreground ,blue-alt))))
    `(js2-jsdoc-value              ((t (:foreground ,green))))
    `(js2-private-member           ((t (:foreground ,gray))))

    ;; JS3
    `(js3-function-param-face      ((t (:foreground ,purple))))
    `(js3-instance-member-face     ((t (:foreground ,blue-alt))))
    `(js3-external-variable-face   ((t (:foreground ,blue))))
    `(js3-jsdoc-tag-face           ((t (:foreground ,purple))))
    `(js3-warning-face             ((t (:underline ,purple))))
    `(js3-error-face               ((t (:underline ,red))))

    ;; Rst-mode
    `(rst-adornment ((t (:foreground ,gray))))
    `(rst-block     ((t (:foreground ,green))))
    `(rst-level-1   ((t (:foreground ,purple))))
    `(rst-level-2   ((t (:foreground ,blue))))
    `(rst-level-3   ((t (:foreground ,blue-alt))))
    `(rst-level-4   ((t (:foreground ,blue-alt))))
    `(rst-level-5   ((t (:foreground ,yellow-dark))))
    `(rst-level-6   ((t (:foreground ,purple))))

    ;; csv-mode
    `(csv-separator-face ((t (:inherit escape-glyph))))

    ;; Undo-tree
    `(undo-tree-visualizer-active-branch-face ((t (:foreground ,green :weight ,(if bivrost-bold 'bold 'normal)))))
    `(undo-tree-visualizer-current-face       ((t (:foreground ,yellow-dark))))
    `(undo-tree-visualizer-default-face       ((t (:foreground ,gray-light))))
    `(undo-tree-visualizer-unmodified-face    ((t (:foreground ,green-mint))))
    `(undo-tree-visualizer-register-face      ((t (:foreground ,yellow-dark))))

    ;; Vundo
    `(vundo-default ((t (:foreground ,fg-main))))
    `(vundo-highlight ((t (:foreground ,yellow-dark :weight bold))))

    ;; Rainbow Delimiters
    `(rainbow-delimiters-mismatched-face ((t (:background ,red :foreground ,red))))
    `(rainbow-delimiters-unmatched-face ((t (:inherit 'rainbow-delimiters-mismatched-face))))
    `(rainbow-delimiters-base-face    ((t (:foreground ,(if bivrost-distinct-parentheses purple gray)))))
    `(rainbow-delimiters-depth-1-face ((t (:foreground ,(if bivrost-distinct-parentheses purple gray)))))
    `(rainbow-delimiters-depth-2-face ((t (:foreground ,(if bivrost-distinct-parentheses blue gray)))))
    `(rainbow-delimiters-depth-3-face ((t (:foreground ,(if bivrost-distinct-parentheses green gray)))))
    `(rainbow-delimiters-depth-4-face ((t (:foreground ,(if bivrost-distinct-parentheses yellow-dark gray)))))
    `(rainbow-delimiters-depth-5-face ((t (:foreground ,(if bivrost-distinct-parentheses red gray)))))
    `(rainbow-delimiters-depth-6-face ((t (:foreground ,(if bivrost-distinct-parentheses blue-alt gray)))))
    `(rainbow-delimiters-depth-7-face ((t (:foreground ,(if bivrost-distinct-parentheses green-mint gray)))))
    `(rainbow-delimiters-depth-8-face ((t (:foreground ,(if bivrost-distinct-parentheses orange gray)))))
    `(rainbow-delimiters-depth-9-face ((t (:foreground ,(if bivrost-distinct-parentheses red-faint gray)))))

    ;; Diff
    `(diff-context           ((t (:foreground ,gray))))
    `(diff-header            ((t (:background ,blue-light))))
    `(diff-function          ((t (:background ,blue-light :foreground ,green))))
    `(diff-nonexistent       ((t (:foreground ,red))))
    `(diff-hunk-header       ((t (:background ,blue-light))))
    `(diff-file-header       ((t (:background unspecified :foreground ,purple))))
    `(diff-added             ((t (:background ,green-faint :foreground ,fg-main))))
    `(diff-changed           ((t (:background ,blue :foreground ,fg-main))))
    `(diff-removed           ((t (:background ,red :foreground ,fg-main))))
    `(diff-refine-added      ((t (:background ,green :foreground ,fg-main))))
    `(diff-refine-changed    ((t (:background ,blue-alt :foreground ,fg-main))))
    `(diff-refine-removed    ((t (:background ,red-faint :foreground ,fg-main))))
    `(diff-indicator-added   ((t (:background ,green-faint :foreground ,fg-main))))
    `(diff-indicator-changed ((t (:background ,blue :foreground ,fg-main))))
    `(diff-indicator-removed ((t (:background ,red :foreground ,fg-main))))

    ;; smerge
    `(smerge-base    ((t (:background ,black))))
    `(smerge-upper   ((t (:background ,green-faint))))
    `(smerge-lower   ((t (:background ,red))))
    `(smerge-markers ((t (:background ,gray :foreground ,bg-main))))

    ;; Ediff
    `(ediff-current-diff-Ancestor ((t (:background ,blue :foreground ,gray-light))))
    `(ediff-current-diff-A ((t (:background ,red :foreground ,gray-light))))
    `(ediff-current-diff-B ((t (:background ,green :foreground ,gray-light))))
    `(ediff-current-diff-C ((t (:background ,blue-alt :foreground ,gray-light))))

    `(ediff-even-diff-Ancestor ((t (:background ,gray-dark))))
    `(ediff-even-diff-A ((t (:background ,gray-dark))))
    `(ediff-even-diff-B ((t (:background ,gray-dark))))
    `(ediff-even-diff-C ((t (:background ,gray-dark))))

    `(ediff-fine-diff-Ancestor ((t (:background ,blue :weight ,(if bivrost-bold 'bold 'normal) :foreground ,fg-main))))
    `(ediff-fine-diff-A ((t (:background ,red-faint :weight ,(if bivrost-bold 'bold 'normal) :foreground ,fg-main))))
    `(ediff-fine-diff-B ((t (:background ,green-mint :weight ,(if bivrost-bold 'bold 'normal) :foreground ,fg-main))))
    `(ediff-fine-diff-C ((t (:background ,blue-alt :weight ,(if bivrost-bold 'bold 'normal) :foreground ,fg-main))))

    `(ediff-odd-diff-Ancestor ((t (:background ,blue-light))))
    `(ediff-odd-diff-A ((t (:background ,blue-light))))
    `(ediff-odd-diff-B ((t (:background ,blue-light))))
    `(ediff-odd-diff-C ((t (:background ,blue-light))))

    ;; calendar.el
    `(diary ((t (:foreground ,yellow-dark))))
    `(holiday ((t (:foreground ,bg-main :background ,red))))
    `(calendar-today ((t (:background ,blue-light :foreground ,purple))))

    ;; Imenu list
    `(imenu-list-entry-face   ((t (:inherit font-lock-keyword-name-face))))
    `(imenu-list-entry-face-0 ((t (:inherit font-lock-keyword-face :height 1.0))))
    `(imenu-list-entry-face-1 ((t (:inherit font-lock-function-name-face))))
    `(imenu-list-entry-face-2 ((t (:inherit font-lock-string-face))))
    `(imenu-list-entry-face-3 ((t (:inherit font-lock-type-face))))
    `(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight ,(if bivrost-bold 'bold 'normal)))))
    `(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight ,(if bivrost-bold 'bold 'normal)))))
    `(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight ,(if bivrost-bold 'bold 'normal)))))
    `(imenu-list-entry-subalist-face-3 ((t (:inherit imenu-list-entry-face-3 :weight ,(if bivrost-bold 'bold 'normal)))))

    ;; Git gutter
    `(git-gutter:unchanged  ((t (:background ,bg-main :foreground unspecified))))
    `(git-gutter:added      ((t (:background ,bg-main :foreground ,green
					     :bold ,(if bivrost-bold t nil)))))
    `(git-gutter:modified   ((t (:background ,bg-main :foreground ,blue
					     :bold ,(if bivrost-bold t nil)))))
    `(git-gutter:deleted    ((t (:background ,bg-main :foreground ,red
					     :bold ,(if bivrost-bold t nil)))))

    ;; Diff-hl
    `(diff-hl-insert        ((t (:background ,green-faint))))
    `(diff-hl-change        ((t (:background ,blue))))
    `(diff-hl-delete        ((t (:background ,red))))
    `(diff-hl-margin-insert ((t (:background ,green :foreground ,bg-main :slant normal))))
    `(diff-hl-margin-change ((t (:background ,blue :foreground ,bg-main :slant normal))))
    `(diff-hl-margin-delete ((t (:background ,red :foreground ,bg-main :slant normal))))

    ;; Popup
    `(popup-face                ((t (:background ,black-alt :foreground ,fg-main 
						 :bold ,(if bivrost-bold t nil)))))
    `(popup-menu-selection-face ((t (:background ,gray-dark :foreground ,fg-main 
						 :bold ,(if bivrost-bold t nil)))))
    `(popup-tip-face            ((t (:background ,gray-dark :foreground ,fg-main 
						 :bold ,(if bivrost-bold t nil)))))

    ;; Frog-menu
    `(frog-menu-posframe-background-face ((t (:background ,black-alt))))
    `(frog-menu-border                   ((t (:background ,gray-dark))))
    `(frog-menu-prompt-face              ((t (:foreground ,purple))))
    `(frog-menu-action-keybinding-face   ((t (:foreground ,blue-alt :bold ,(if bivrost-bold t nil)))))

    ;; Terminal
    ;; Ansi-color faces (built-in Emacs 28.1+)
    `(ansi-color-black          ((t (:foreground ,black :background ,black))))
    `(ansi-color-red            ((t (:foreground ,red :background ,red))))
    `(ansi-color-green          ((t (:foreground ,green :background ,green))))
    `(ansi-color-yellow         ((t (:foreground ,yellow-dark :background ,yellow-dark))))
    `(ansi-color-blue           ((t (:foreground ,blue :background ,blue))))
    `(ansi-color-magenta        ((t (:foreground ,purple :background ,purple))))
    `(ansi-color-cyan           ((t (:foreground ,blue-alt :background ,blue-alt))))
    `(ansi-color-white          ((t (:foreground ,fg-main :background ,fg-main))))

    `(term               ((t (:background ,bg-main :foreground ,fg-main))))
    `(term-color-black   ((t (:foreground ,black))))
    `(term-color-red     ((t (:foreground ,red))))
    `(term-color-green   ((t (:foreground ,green))))
    `(term-color-yellow  ((t (:foreground ,yellow-dark))))
    `(term-color-blue    ((t (:foreground ,blue))))
    `(term-color-magenta ((t (:foreground ,purple))))
    `(term-color-cyan    ((t (:foreground ,blue-alt))))
    `(term-color-white   ((t (:foreground ,fg-main))))
    `(term-underline     ((t (:inherit underline))))

    ;; EShell
    `(eshell-prompt        ((t (:foreground ,purple :bold ,(if bivrost-bold t nil)))))
    `(eshell-ls-directory  ((t (:foreground ,blue :bold ,(if bivrost-bold t nil)))))
    `(eshell-ls-symlink    ((t (:foreground ,blue-alt :bold ,(if bivrost-bold t nil)))))
    `(eshell-ls-executable ((t (:foreground ,green :bold ,(if bivrost-bold t nil)))))
    `(eshell-ls-archive    ((t (:foreground ,orange))))
    `(eshell-ls-backup     ((t (:foreground ,purple))))
    `(eshell-ls-clutter    ((t (:foreground ,gray))))
    `(eshell-ls-missing    ((t (:background ,black2 :foreground ,red))))
    `(eshell-ls-product    ((t (:foreground ,yellow-dark))))
    `(eshell-ls-readonly   ((t (:foreground ,red-faint))))
    `(eshell-ls-special    ((t (:foreground ,blue-light))))
    `(eshell-ls-unreadable ((t (:inherit shadow))))

    ;; Whitespace mode
    `(whitespace-empty            ((t (:background ,black2))))
    `(whitespace-line             ((t (:background ,bg-main :foreground ,blue-alt))))
    `(whitespace-indentation      ((t (:background ,blue-light :foreground ,blue-light))))
    `(whitespace-tab              ((t (:background ,black-alt :foreground ,gray))))
    `(whitespace-space            ((t (:background ,black-alt :foreground ,gray))))
    `(whitespace-newline          ((t (:inherit whitespace-space))))
    `(whitespace-space-before-tab ((t (:background ,blue :foreground ,black-alt))))
    `(whitespace-space-after-tab  ((t (:background ,blue :foreground ,black-alt))))
    `(whitespace-trailing         ((t (:inherit trailing-whitespace :foreground ,black-alt))))
    `(whitespace-big-indent       ((t (:background ,red :foreground ,black-alt))))

    ;; Org-mode
    `(org-todo                      ((t (:foreground ,red :bold ,(if bivrost-bold t nil)))))
    `(org-done                      ((t (:foreground ,green  :bold ,(if bivrost-bold t nil)))))
    `(org-headline-done             ((t (:foreground ,gray  :bold nil))))
    `(org-ellipsis                  ((t (:foreground ,purple :underline nil))))
    `(org-date                      ((t (:foreground ,purple :underline ,(if bivrost-underline t nil)))))
    `(org-date-selected             ((t (:background ,black2 :foreground ,blue :weight bold))))
    `(org-link                      ((t (:inherit link))))
    `(org-code                      ((t (:foreground ,orange))))
    `(org-verbatim                  ((t (:background ,(if bivrost-distinct-verbatim black-alt bg-main) :foreground ,green))))
    `(org-hide                      ((t (:foreground ,bg-main))))
    `(org-drawer                    ((t (:foreground ,blue))))
    `(org-special-keyword           ((t (:foreground ,orange))))
    `(org-table                     ((t (:background ,black-alt :foreground ,gray-light))))
    `(org-formula                   ((t (:background unspecified :foreground ,yellow-dark))))
    `(org-warning                   ((t (:foreground ,orange :underline ,(if bivrost-underline t nil)))))
    `(org-tag                       ((t (:foreground ,orange))))
    `(org-checkbox                  ((t (:foreground ,blue))))

    `(org-document-info-keyword     ((t (:foreground ,(if bivrost-distinct-metakeys blue-alt gray)))))
    `(org-meta-line                 ((t (:inherit org-document-info-keyword))))
    `(org-block                     ((t (:background ,black-alt :foreground ,gray-light))))
    `(org-block-begin-line          ((t (:background ,black-alt :foreground ,gray :height 0.9))))
    `(org-block-end-line            ((t (:inherit org-block-begin-line))))
    `(org-list-dt                   ((t (:inherit org-checkbox))))
    `(org-document-title            ((t (:foreground ,purple :bold ,(if bivrost-bold t nil) :height 1.1))))
    `(org-document-info             ((t (:foreground ,purple))))
    `(org-footnote                  ((t (:foreground ,gray-light :underline ,(if bivrost-underline t nil)))))
    `(org-quote                     ((t (:background ,black-alt :foreground ,gray-light :italic ,(if bivrost-italic t nil) :extend t))))
    `(org-verse                     ((t (:foreground ,gray-light :italic ,(if bivrost-italic t nil)))))

    `(org-level-1            ((t (:foreground ,purple :bold ,(if bivrost-bold t nil) :height ,(if bivrost-org-scale-headings 1.2 1.0)))))
    `(org-level-2            ((t (:foreground ,blue  :bold nil :height ,(if bivrost-org-scale-headings 1.1 1.0)))))
    `(org-level-3            ((t (:foreground ,blue-alt :bold nil :height ,(if bivrost-org-scale-headings 1.05 1.0)))))
    `(org-level-4            ((t (:foreground ,yellow-dark :bold nil))))

    ;; org-agenda
    `(org-agenda-dimmed-todo-face ((t (:foreground ,gray))))
    `(org-agenda-date             ((t (:foreground ,fg-main))))
    `(org-agenda-date-today       ((t (:foreground ,orange :bold ,(if bivrost-bold t nil)))))
    `(org-agenda-date-weekend     ((t (:foreground ,orange))))
    `(org-agenda-done             ((t (:foreground ,green))))
    `(org-agenda-structure        ((t (:foreground ,purple))))
    `(org-agenda-clocking         ((t (:background ,blue-light :foreground ,fg-main))))
    `(org-scheduled               ((t (:foreground ,fg-main))))
    `(org-scheduled-today         ((t (:foreground ,blue :bold ,(if bivrost-bold t nil)))))
    `(org-sexp-date               ((t (:foreground ,gray-light))))
    `(org-time-grid               ((t (:foreground ,gray))))

    ;; org-habit
    `(org-habit-clear-face          ((t (:background ,blue))))
    `(org-habit-clear-future-face   ((t (:background ,black2))))
    `(org-habit-ready-face          ((t (:background ,green))))
    `(org-habit-ready-future-face   ((t (:background ,green-faint))))
    `(org-habit-alert-face          ((t (:background ,blue))))
    `(org-habit-alert-future-face   ((t (:background ,blue-light))))
    `(org-habit-overdue-face        ((t (:background ,red))))
    `(org-habit-overdue-future-face ((t (:background ,red-faint))))

    ;; Web-mode
    `(css-selector                   ((t (:inherit font-lock-builtin-face))))
    `(web-mode-css-selector-face     ((t (:inherit font-lock-builtin-face))))
    `(web-mode-type-face             ((t (:inherit font-lock-type-face))))
    `(web-mode-html-tag-face         ((t (:inherit font-lock-keyword-face))))
    `(web-mode-html-tag-bracket-face ((t (:inherit web-mode-html-tag-face))))
    `(web-mode-html-attr-name-face   ((t (:inherit font-lock-function-name-face))))
    `(web-mode-html-attr-value-face  ((t (:inherit font-lock-string-face))))
    `(web-mode-builtin-face          ((t (:inherit font-lock-builtin-face))))
    `(web-mode-keyword-face          ((t (:inherit font-lock-builtin-face))))
    `(web-mode-constant-face         ((t (:inherit font-lock-constant-face))))
    `(web-mode-comment-face          ((t (:inherit font-lock-comment-face))))
    `(web-mode-doctype-face          ((t (:inherit font-lock-preprocessor-face))))
    `(web-mode-function-name-face    ((t (:inherit font-lock-function-name-face))))
    `(web-mode-string-face           ((t (:inherit font-lock-string-face))))
    `(web-mode-warning-face          ((t (:inherit font-lock-warning-face))))

    ;; Alert
    `(alert-high-face     ((t (:inherit bold :foreground ,red))))
    `(alert-low-face      ((t (:inherit bold :foreground ,gray-light))))
    `(alert-moderate-face ((t (:inherit bold :foreground ,orange))))
    `(alert-trivial-face  ((t (:inherit bold :foreground ,gray-light))))
    `(alert-urgent-face   ((t (:inherit bold :foreground ,red))))

    ;; Evil ex
    `(evil-ex-info                   ((t (:foreground ,orange))))
    `(evil-ex-substitute-matches     ((t (:background unspecified :foreground ,red :underline ,bivrost-underline))))
    `(evil-ex-substitute-replacement ((t (:background unspecified :foreground ,green-mint))))
    `(evil-ex-lazy-highlight         ((t (:inherit lazy-highlight))))

    ;; Evil-goggles
    `(evil-goggles-default-face      ((t (:background ,blue-light))))

    ;; Evil-snipe
    `(evil-snipe-first-match-face    ((t (:foreground ,blue))))
    `(evil-snipe-matches-face        ((t (:foreground ,blue-alt))))

    ;; evil-mc
    `(evil-mc-cursor-default-face    ((t (:background ,blue-alt :foreground ,bg-main :inverse-video nil))))
    `(evil-mc-region-face            ((t (:inherit region))))
    `(evil-mc-cursor-bar-face        ((t (:background ,blue-alt :foreground ,bg-main :height 1))))
    `(evil-mc-cursor-hbar-face       ((t (:underline (:color ,blue)))))

    ;; Clojure/Cider
    `(cider-deprecated-face                   ((t (:underline (:style ,(if bivrost-underline-wave 'wave 'line) :color ,orange)))))
    `(cider-docview-emphasis-face             ((t (:inherit italic :foreground ,gray-light))))
    `(cider-docview-literal-face              ((t (:foreground ,yellow-dark))))
    `(cider-docview-strong-face               ((t (:inherit ,(if bivrost-bold 'bold 'normal) :foreground ,gray-light))))
    `(cider-docview-table-border-face         ((t (:foreground ,black2))))
    `(cider-enlightened-face                  ((t (:background ,black2 :foreground ,gray-light :box (:color ,yellow-dark :line-width -1)))))
    `(cider-enlightened-local-face            ((t (:foreground ,blue))))
    `(cider-error-highlight-face              ((t (:foreground ,red :underline (:style ,(if bivrost-underline-wave 'wave 'line) :color ,red)))))
    `(cider-fragile-button-face               ((t (:background unspecified :foreground ,yellow-dark :box (:line-width 1 :color ,gray-dark :style nil)))))
    `(cider-fringe-good-face                  ((t (:foreground ,green))))
    `(cider-instrumented-face                 ((t (:background ,black2 :foreground ,gray-light :box (:color ,red :line-width -1)))))
    `(cider-reader-conditional-face           ((t (:foreground ,gray-light))))
    `(cider-repl-input-face                   ((t (:inherit ,(if bivrost-bold 'bold 'normal)))))
    `(cider-repl-prompt-face                  ((t (:foreground ,purple))))
    `(cider-repl-stderr-face                  ((t (:foreground ,red))))
    `(cider-repl-stdout-face                  ((t (:foreground ,blue))))
    `(cider-result-overlay-face               ((t (:background ,black2 :foreground ,gray-light :box (:color ,yellow-dark :line-width -1)))))
    `(cider-stacktrace-error-class-face       ((t (:foreground ,red :inherit ,(if bivrost-bold 'bold 'normal)))))
    `(cider-stacktrace-error-message-face     ((t (:foreground ,red :inherit italic))))
    `(cider-stacktrace-face                   ((t (:background ,black2 :foreground ,fg-main))))
    `(cider-stacktrace-filter-active-face     ((t (:foreground ,green-light :underline t))))
    `(cider-stacktrace-filter-inactive-face   ((t (:foreground ,green-light))))
    `(cider-stacktrace-fn-face                ((t (:foreground ,fg-main :inherit ,(if bivrost-bold 'bold 'normal)))))
    `(cider-stacktrace-ns-face                ((t (:foreground ,gray-light :inherit italic))))
    `(cider-stacktrace-promoted-button-face   ((t (:background unspecified :foreground ,red :box (:line-width 1 :color ,gray :style nil)))))
    `(cider-stacktrace-suppressed-button-face ((t (:background unspecified :foreground ,gray-silver :box (:line-width 1 :color ,gray :style nil)))))
    `(cider-test-error-face                   ((t (:background ,orange :foreground ,bg-main))))
    `(cider-test-failure-face                 ((t (:background ,red :foreground ,bg-main))))
    `(cider-test-success-face                 ((t (:background ,green :foreground ,bg-main))))
    `(cider-traced-face                       ((t (:background ,black2 :foreground ,gray-light :box (:color ,gray :line-width -1)))))
    `(cider-warning-highlight-face            ((t (:foreground ,gray-light :underline (:style ,(if bivrost-underline-wave 'wave 'line) :color ,orange)))))

    ;; Clojure test
    `(clojure-test-failure-face               ((t (:background unspecified :inherit flymake-warning))))
    `(clojure-test-error-face                 ((t (:background unspecified :inherit flymake-error))))
    `(clojure-test-success-face               ((t (:background unspecified :inherit flymake-note))))

    ;; Erlang
    `(edts-face-warning-line      ((t (:inherit flymake-warning))))
    `(edts-face-warning-mode-line ((t (:foreground ,orange :weight ,(if bivrost-bold 'bold 'normal)))))
    `(edts-face-error-line        ((t (:inherit flymake-error))))
    `(edts-face-error-mode-line   ((t (:foreground ,red :weight ,(if bivrost-bold 'bold 'normal)))))

    ;; Markdown
    `(markdown-bold-face               ((t (:inherit ,(if bivrost-bold 'bold 'normal)))))
    `(markdown-italic-face             ((t (:inherit ,(if bivrost-italic 'italic 'normal)))))
    `(markdown-header-face             ((t (:foreground ,yellow-dark :bold ,(if bivrost-bold 'bold 'normal)))))
    `(markdown-header-delimiter-face   ((t (:inherit markdown-header-face))))
    `(markdown-metadata-key-face       ((t (:foreground ,(if bivrost-distinct-metakeys purple gray)))))
    `(markdown-metadata-value-face     ((t (:foreground ,green-faint))))
    `(markdown-markup-face             ((t (:foreground ,blue))))
    `(markdown-list-face               ((t (:foreground ,blue))))
    `(markdown-pre-face                ((t (:foreground ,yellow-dark))))
    `(markdown-code-face               ((t (:background ,black2))))
    `(markdown-inline-code-face        ((t (:inherit (markdown-code-face markdown-pre-face)))))
    `(markdown-link-face               ((t (:inherit link))))
    `(markdown-url-face                ((t (:foreground ,yellow-dark))))
    `(markdown-blockquote-face         ((t (:foreground ,gray-silver))))
    `(markdown-reference-face          ((t (:foreground ,green-faint))))
    `(markdown-language-keyword-face   ((t (:foreground ,green))))
    `(markdown-html-tag-name-face      ((t (:inherit font-lock-keyword-face))))
    `(markdown-html-tag-delimiter-face ((t (:inherit web-mode-html-tag-face))))
    `(markdown-html-entity-face        ((t (:inherit font-lock-variable-name-face))))
    `(markdown-html-attr-name-face     ((t (:inherit font-lock-function-name-face))))
    `(markdown-html-attr-value-face    ((t (:inherit font-lock-string-face))))

    ;; Shell script
    `(sh-escaped-newline ((t (:inherit font-lock-string-face))))
    `(sh-heredoc         ((t (:foreground ,green-faint))))
    `(sh-quoted-exec     ((t (:foreground ,green-light))))

    ;; Wgrep
    `(wgrep-face        ((t (:background ,black2 :foreground ,blue))))
    `(wgrep-delete-face ((t (:background ,red-faint :foreground ,black2))))
    `(wgrep-done-face   ((t (:foreground ,green))))
    `(wgrep-file-face   ((t (:foreground ,gray))))
    `(wgrep-reject-face ((t (:inherit error))))

    ;; xref
    `(xref-file-header ((t (:foreground ,yellow-dark))))
    `(xref-line-number ((t (:inherit compilation-line-number))))
    `(xref-match       ((t (:foreground ,blue :underline ,(if bivrost-underline t nil)))))

    ;; tldr
    `(tldr-title          ((t (:foreground ,yellow-dark :weight ,(if bivrost-bold 'bold 'normal)))))
    `(tldr-introduction   ((t (:foreground ,gray))))
    `(tldr-description    ((t (:foreground ,yellow-dark))))
    `(tldr-command-itself ((t (:background ,blue :foreground ,bg-main))))
    `(tldr-code-block     ((t (:background ,black2 :foreground ,blue))))

    ;; antlr-mode
    `(antlr-keyword  ((t (:inherit font-lock-keyword-face))))
    `(antlr-syntax   ((t (:inherit font-lock-constant-face))))
    `(antlr-ruledef  ((t (:inherit font-lock-function-name-face))))
    `(antlr-ruleref  ((t (:inherit font-lock-type-face))))
    `(antlr-tokendef ((t (:inherit font-lock-function-name-face))))
    `(antlr-tokenref ((t (:inherit font-lock-type-face))))
    `(antlr-literal  ((t (:inherit font-lock-string-face))))

    ;; Anzu mode
    `(anzu-mode-line          ((t (:foreground ,green-light))))
    `(anzu-mode-line-no-match ((t (:foreground ,red))))
    `(anzu-replace-highlight  ((t (:inherit lazy-highlight))))
    `(anzu-replace-to         ((t (:inherit isearch))))
    `(anzu-match-1            ((t (:foreground ,blue))))
    `(anzu-match-2            ((t (:foreground ,blue-alt))))
    `(anzu-match-3            ((t (:foreground ,green-mint))))

    ;; Tabbar (classic tabbar package)
    `(tabbar-default ((t (:background ,bg-main :foreground ,bg-main :height 1.0))))
    `(tabbar-highlight ((t (:background ,blue-light :foreground ,fg-main :distant-foreground ,bg-main))))
    `(tabbar-button ((t (:foreground ,fg-main :background ,bg-main))))
    `(tabbar-button-highlight ((t (:foreground ,bg-main :background ,gray-light))))
    `(tabbar-modified ((t (:inherit tabbar-default :foreground ,blue-alt :weight bold))))
    `(tabbar-unselected ((t (:inherit tabbar-default :foreground ,gray))))
    `(tabbar-unselected-modified ((t (:inherit tabbar-modified))))
    `(tabbar-selected ((t (:inherit tabbar-default :foreground ,fg-main :background ,bg-active :weight bold))))
    `(tabbar-selected-modified ((t (:inherit tabbar-selected :foreground ,green))))

    ;; Built-in tab-bar mode (Emacs 27+)
    `(tab-bar ((t (:background ,black-alt :foreground ,fg-main))))
    `(tab-bar-tab ((t (:background ,bg-main :foreground ,fg-main))))
    `(tab-bar-tab-inactive ((t (:background ,bg-main :foreground ,gray))))

    ;; Built-in tab-line (Emacs 27+)
    `(tab-line ((t (:background ,black-alt :foreground ,fg-main))))
    `(tab-line-tab ((t (:background ,bg-main :foreground ,fg-main))))
    `(tab-line-tab-inactive ((t (:background ,bg-main :foreground ,gray))))
    `(tab-line-close-highlight ((t (:foreground ,red))))

    ;; Additional tab faces
    `(tab-bar-tab-group-current ((t (:background ,bg-main :foreground ,blue))))
    `(tab-bar-tab-group-inactive ((t (:background ,black2 :foreground ,gray))))
    `(tab-bar-tab-ungrouped ((t (:background ,black2 :foreground ,gray-silver))))
    `(tab-line-tab-current ((t (:background ,bg-active :foreground ,fg-main :weight bold))))
    `(tab-line-highlight ((t (:background ,blue-light :foreground ,fg-main))))

    ;; --- Completion interfaces ---------------------------------------------

    ;; Default completion
    `(completions-common-part ((t (:foreground ,blue))))
    `(completions-annotations ((t (:foreground ,gray))))

    ;; Consult
    `(consult-file ((t (:foreground ,fg-main))))
    `(consult-bookmark ((t (:foreground ,blue))))
    `(consult-imenu-prefix ((t (:foreground ,gray))))
    `(consult-preview-line ((t (:background ,bg-active))))
    `(consult-preview-cursor ((t (:background ,blue-light :foreground ,fg-main))))
    `(consult-annotation ((t (:background nil :foreground ,gray))))

    ;; Marginalia
    `(marginalia-documentation    ((t (:inherit font-lock-doc-face))))
    `(marginalia-key              ((t (:foreground ,purple))))
    `(marginalia-mode             ((t (:foreground ,yellow-dark))))
    `(marginalia-number           ((t (:foreground ,green-light))))
    `(marginalia-size             ((t (:foreground ,green-light))))
    `(marginalia-type             ((t (:foreground ,green))))
    `(marginalia-date             ((t (:foreground ,yellow-dark))))
    `(marginalia-value            ((t (:foreground ,gray-light))))
    `(marginalia-char             ((t (:foreground ,blue))))
    `(marginalia-on               ((t (:inherit success))))
    `(marginalia-file-owner       ((t (:foreground ,blue-alt))))

    `(marginalia-file-name        ((t (:foreground ,yellow-dark))))
    `(marginalia-file-priv-dir    ((t (:foreground ,purple))))
    `(marginalia-file-priv-read   ((t (:foreground ,green))))
    `(marginalia-file-priv-write  ((t (:foreground ,blue))))
    `(marginalia-file-priv-exec   ((t (:foreground ,red))))

    ;; Ivy
    `(ivy-confirm-face            ((t (:inherit success))))
    `(ivy-current-match           ((t (:background ,(if bivrost-hl-line-colored blue-light gray-dark) :foreground ,blue :bold ,(if bivrost-bold t nil)))))
    `(ivy-cursor                  ((t (:background ,gray-dark :foreground ,fg-main))))
    `(ivy-grep-info               ((t (:foreground ,yellow-dark))))
    `(ivy-highlight-face          ((t (:background unspecified :foreground ,blue))))
    `(ivy-match-required-face     ((t (:background unspecified :foreground ,red :bold nil))))
    `(ivy-modified-buffer         ((t (:foreground ,blue))))
    `(ivy-remote                  ((t (:foreground ,blue-alt))))
    `(ivy-subdir                  ((t (:foreground ,purple :bold ,(if bivrost-bold t nil)))))
    `(ivy-virtual                 ((t (:foreground ,green-mint))))
    `(ivy-minibuffer-match-face-1 ((t (:background unspecified :foreground ,green-mint))))
    `(ivy-minibuffer-match-face-2 ((t (:background unspecified :foreground ,blue :bold ,(if bivrost-bold t nil)))))
    `(ivy-minibuffer-match-face-3 ((t (:background unspecified :foreground ,blue-alt :bold ,(if bivrost-bold t nil)))))
    `(ivy-minibuffer-match-face-4 ((t (:background unspecified :foreground ,green-mint :bold ,(if bivrost-bold t nil)))))

    ;; Ivy posframe
    `(ivy-posframe               ((t (:background ,black2))))
    `(ivy-posframe-border        ((t (:background ,black-alt))))

    ;; Vertico
    `(vertico-current ((t (:background ,bg-active :foreground ,yellow-dark :bold t))))
    `(vertico-multiline ((t (:foreground ,gray))))
    `(vertico-group-title ((t (:foreground ,gray))))
    `(vertico-group-separator ((t (:foreground ,gray :strike-through t))))

    ;; Vertico-quick
    `(vertico-quick1 ((t (:background ,black2 :foreground ,blue :weight bold))))
    `(vertico-quick2 ((t (:background ,black2 :foreground ,purple :weight bold))))

    ;; Orderless
    `(orderless-match-face-0 ((t (:foreground ,yellow-dark))))
    `(orderless-match-face-1 ((t (:foreground ,blue))))
    `(orderless-match-face-2 ((t (:foreground ,purple))))
    `(orderless-match-face-3 ((t (:foreground ,green))))

    )))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(run-hooks 'bivrost-after-load-hook)

(provide 'bivrost)
(provide-theme 'bivrost)

;;; bivrost-theme.el ends here
