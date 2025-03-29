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

;; Define mode-line specific faces
(defface bivrost-modeline-evil-normal nil
  "Face for Evil normal state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-insert nil
  "Face for Evil insert state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-visual nil
  "Face for Evil visual state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-replace nil
  "Face for Evil replace state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-motion nil
  "Face for Evil motion state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-operator nil
  "Face for Evil operator state in mode-line."
  :group 'bivrost)

(defface bivrost-modeline-evil-emacs nil
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

(defface bivrost-heading-1 nil
  "Face for headings."
  :group nil)

;; Configure the theme with custom faces
(bivrost-with-color-variables
 (custom-theme-set-faces
  'bivrost

  ;; Set mode-line faces directly with colors
  `(mode-line ((t (:background ,bg-active 
			       :foreground ,gray-light
			       :box (:line-width ,(if bivrost-modeline-padded 
						      (if (integerp bivrost-modeline-padded) 
							  bivrost-modeline-padded 4) 2)
						 :color ,(if (and (not bivrost-modeline-padded) bivrost-modeline-border) 
							     black-alt bg-active)
						 :style nil)))))
  
  `(mode-line-inactive ((t (:background ,black
					:foreground ,gray-dark
					:box (:line-width ,(if bivrost-modeline-padded 
							       (if (integerp bivrost-modeline-padded) 
								   bivrost-modeline-padded 4) 2)
							  :color ,(if (and (not bivrost-modeline-padded) bivrost-modeline-border) 
								      black-alt black)
							  :style nil)))))
  
  `(mode-line-buffer-id ((t (:foreground ,fg-main :weight bold))))
  
  `(mode-line-highlight ((t (:foreground ,blue))))
  
  `(mode-line-emphasis ((t (:foreground ,blue :weight bold))))
  
  ;; Evil-mode state faces for modeline
  `(bivrost-modeline-evil-normal ((t (:foreground ,blue))))
  `(bivrost-modeline-evil-insert ((t (:foreground ,green))))
  `(bivrost-modeline-evil-visual ((t (:foreground ,purple))))
  `(bivrost-modeline-evil-replace ((t (:foreground ,red))))
  `(bivrost-modeline-evil-motion ((t (:foreground ,yellow-dark))))
  `(bivrost-modeline-evil-operator ((t (:foreground ,blue-alt))))
  `(bivrost-modeline-evil-emacs ((t (:foreground ,orange))))
  
  ;; Add more face customizations as needed
  ))

;; Optional: add compatibility with evil-mode if it's used
(defun bivrost-modeline-set-evil-faces ()
  "Set faces for evil states in the mode line."
  (when (bound-and-true-p evil-mode)
    (bivrost-with-color-variables
     (custom-set-faces
      `(evil-mode-line-normal ((t (:inherit bivrost-modeline-evil-normal))))
      `(evil-mode-line-insert ((t (:inherit bivrost-modeline-evil-insert))))
      `(evil-mode-line-visual ((t (:inherit bivrost-modeline-evil-visual))))
      `(evil-mode-line-replace ((t (:inherit bivrost-modeline-evil-replace))))
      `(evil-mode-line-motion ((t (:inherit bivrost-modeline-evil-motion))))
      `(evil-mode-line-operator ((t (:inherit bivrost-modeline-evil-operator))))
      `(evil-mode-line-emacs ((t (:inherit bivrost-modeline-evil-emacs)))))))
  nil)

(defun bivrost ()
  "Load bivrost theme."
  (interactive)

  (when bivrost-use-faded-lisp-parens
    (add-hook 'lisp-data-mode-hook #'bivrost--font-lock-add-paren)
    (add-hook 'scheme-mode-hook #'bivrost--font-lock-add-paren))

  (load-theme 'bivrost t)
  ;; Set evil-mode faces if evil-mode is available
  (when (featurep 'evil)
    (bivrost-modeline-set-evil-faces))
  (run-hooks 'bivrost-after-load-hook))

;;(make-obsolete 'bivrost 'load-theme "0.1")

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
