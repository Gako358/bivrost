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

(deftheme bivrost "An arctic, north-bluish clean and elegant theme")

(defgroup bivrost nil
  "Bivrost theme customizations.
  The theme has to be reloaded after changing anything in this group."
  :group 'faces)

;;;; Color Constants
(let ((class '((class color) (min-colors 89)))
      (bivrost-region-highlight-foreground "#2E3440")
      (bivrost-region-highlight-background "#88C0D0")
      (bivrost0 "#1a1a1a")  ; Base dark background color
      (bivrost1 "#3B4252")  ; Dark bluish color
      (bivrost2 "#303035")  ; Alternate dark background, subtle contrast from bivrost0
      (bivrost3 "#434C5E")  ; Lighter blue color, suble contrast from bivrost1
      (bivrost4 "#4C566A")  ; Grayish blue color, subtle contrast from bivrost3
      (bivrost5 "#dfdfe0")  ; Base forground, white
      (bivrost6 "#E5E9F0")  ; White bluish color
      (bivrost7 "#ECEFF4")  ; Grayish white color, subtle contrast from bivrost6
      (bivrost8 "#8FBCBB")  ; Cyan color
      (bivrost9 "#719cd6")  ; Blue color
      (bivrost10 "#81A1C1") ; Light blue color, subtle contrast from bivrost9
      (bivrost11 "#5E81AC") ; Gray blue color, subtle contrast from bivrost10
      (bivrost12 "#BF616A") ; Red color
      (bivrost13 "#D08770") ; Light orange color, subtle contrast from bivrost12
      (bivrost14 "#EBCB8B") ; Yellow color
      (bivrost15 "#A3BE8C") ; Green color
      (bivrost16 "#B48EAD") ; Lime green color, subtle contrast from bivrost15
      (bivrost-annotation "#D08770")
      (bivrost-attribute "#8FBCBB")
      (bivrost-class "#8FBCBB")
      (bivrost-comment "#616e88")
      (bivrost-escape "#D08770")
      (bivrost-method "#88C0D0")
      (bivrost-keyword "#81A1C1")
      (bivrost-numeric "#B48EAD")
      (bivrost-operator "#81A1C1")
      (bivrost-preprocessor "#5E81AC")
      (bivrost-punctuation "#D8DEE9")
      (bivrost-regexp "#EBCB8B")
      (bivrost-string "#A3BE8C")
      (bivrost-tag "#81A1C1")
      (bivrost-variable "#D8DEE9"))

;;;; +------------+
;;;; + Core Faces +
;;;; +------------+
  (custom-theme-set-faces
   'bivrost
   ;; +--- Base ---+
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:weight bold :slant italic))))
   `(default ((,class (:foreground ,bivrost5 :background ,bivrost0))))
   `(error ((,class (:foreground ,bivrost12 :weight bold))))
   `(escape-glyph ((,class (:foreground ,bivrost13))))
   `(font-lock-builtin-face ((,class (:foreground ,bivrost10))))
   `(font-lock-comment-face ((,class (:foreground ,bivrost-comment))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,bivrost-comment))))
   `(font-lock-constant-face ((,class (:foreground ,bivrost10))))
   `(font-lock-doc-face ((,class (:foreground ,bivrost-comment))))
   `(font-lock-function-name-face ((,class (:foreground ,bivrost9))))
   `(font-lock-keyword-face ((,class (:foreground ,bivrost10))))
   `(font-lock-negation-char-face ((,class (:foreground ,bivrost10))))
   `(font-lock-preprocessor-face ((,class (:foreground ,bivrost11 :weight bold))))
   `(font-lock-reference-face ((,class (:foreground ,bivrost10))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,bivrost14))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,bivrost14))))
   `(font-lock-string-face ((,class (:foreground ,bivrost15))))
   `(font-lock-type-face ((,class (:foreground ,bivrost8))))
   `(font-lock-variable-name-face ((,class (:foreground ,bivrost5))))
   `(font-lock-warning-face ((,class (:foreground ,bivrost14))))
   `(italic ((,class (:slant italic))))
   `(shadow ((,class (:foreground ,bivrost4))))
   `(underline ((,class (:underline t))))
   `(warning ((,class (:foreground ,bivrost14 :weight bold))))

   ;; +--- Syntax ---+

   ;; > C
   `(c-annotation-face ((,class (:foreground ,bivrost-annotation))))

   ;; > diff
   `(diff-added ((,class (:foreground ,bivrost15))))
   `(diff-changed ((,class (:foreground ,bivrost14))))
   `(diff-context ((,class (:inherit default))))
   `(diff-file-header ((,class (:foreground ,bivrost9))))
   `(diff-function ((,class (:foreground ,bivrost8))))
   `(diff-header ((,class (:foreground ,bivrost10 :weight bold))))
   `(diff-hunk-header ((,class (:foreground ,bivrost10 :background ,bivrost0))))
   `(diff-indicator-added ((,class (:foreground ,bivrost15))))
   `(diff-indicator-changed ((,class (:foreground ,bivrost14))))
   `(diff-indicator-removed ((,class (:foreground ,bivrost12))))
   `(diff-nonexistent ((,class (:foreground ,bivrost12))))
   `(diff-refine-added ((,class (:foreground ,bivrost15))))
   `(diff-refine-changed ((,class (:foreground ,bivrost14))))
   `(diff-refine-removed ((,class (:foreground ,bivrost12))))
   `(diff-removed ((,class (:foreground ,bivrost12))))

   ;; +--- UI ---+
   `(border ((,class (:foreground ,bivrost5))))
   `(buffer-menu-buffer ((,class (:foreground ,bivrost5 :weight bold))))
   `(button ((,class (:background ,bivrost0 :foreground ,bivrost9 :box (:line-width 2 :color ,bivrost5 :style pressed-button)))))
   `(completions-annotations ((,class (:foreground ,bivrost10))))
   `(completions-common-part ((,class (:foreground ,bivrost9 :weight bold))))
   `(completions-first-difference ((,class (:foreground ,bivrost12))))
   `(custom-button ((,class (:background ,bivrost0 :foreground ,bivrost9 :box (:line-width 2 :color ,bivrost5 :style pressed-button)))))
   `(custom-button-mouse ((,class (:background ,bivrost5 :foreground ,bivrost0 :box (:line-width 2 :color ,bivrost5 :style pressed-button)))))
   `(custom-button-pressed ((,class (:background ,bivrost7 :foreground ,bivrost0 :box (:line-width 2 :color ,bivrost5 :style pressed-button)))))
   `(custom-button-pressed-unraised ((,class (:background ,bivrost5 :foreground ,bivrost0 :box (:line-width 2 :color ,bivrost5 :style pressed-button)))))
   `(custom-button-unraised ((,class (:background ,bivrost0 :foreground ,bivrost9 :box (:line-width 2 :color ,bivrost5 :style pressed-button)))))
   `(custom-changed ((,class (:foreground ,bivrost14))))
   `(custom-comment ((,class (:foreground ,bivrost-comment))))
   `(custom-comment-tag ((,class (:foreground ,bivrost8))))
   `(custom-documentation ((,class (:foreground ,bivrost5))))
   `(custom-group-tag ((,class (:foreground ,bivrost9 :weight bold))))
   `(custom-group-tag-1 ((,class (:foreground ,bivrost9 :weight bold))))
   `(custom-invalid ((,class (:foreground ,bivrost12))))
   `(custom-modified ((,class (:foreground ,bivrost14))))
   `(custom-rogue ((,class (:foreground ,bivrost13 :background ,bivrost3))))
   `(custom-saved ((,class (:foreground ,bivrost15))))
   `(custom-set ((,class (:foreground ,bivrost9))))
   `(custom-state ((,class (:foreground ,bivrost15))))
   `(custom-themed ((,class (:foreground ,bivrost9 :background ,bivrost3))))
   `(cursor ((,class (:background ,bivrost5))))
   `(fringe ((,class (:foreground ,bivrost5 :background ,bivrost0))))
   `(file-name-shadow ((,class (:inherit shadow))))
   `(header-line ((,class (:foreground ,bivrost5 :background ,bivrost3))))
   `(help-argument-name ((,class (:foreground ,bivrost9))))
   `(highlight ((,class (:foreground ,bivrost9 :background ,bivrost3))))
   `(hl-line ((,class (:background ,bivrost1))))
   `(info-menu-star ((,class (:foreground ,bivrost10))))
   `(isearch ((,class (:foreground ,bivrost0 :background ,bivrost9))))
   `(isearch-fail ((,class (:foreground ,bivrost12))))
   `(link ((,class (:underline t))))
   `(link-visited ((,class (:underline t))))
   `(linum ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(linum-relative-current-face ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(match ((,class (:inherit isearch))))
   `(message-cited-text ((,class (:foreground ,bivrost5))))
   `(message-header-cc ((,class (:foreground ,bivrost10))))
   `(message-header-name ((,class (:foreground ,bivrost8))))
   `(message-header-newsgroup ((,class (:foreground ,bivrost15))))
   `(message-header-other ((,class (:foreground ,bivrost5))))
   `(message-header-subject ((,class (:foreground ,bivrost9))))
   `(message-header-to ((,class (:foreground ,bivrost10))))
   `(message-header-xheader ((,class (:foreground ,bivrost14))))
   `(message-mml ((,class (:foreground ,bivrost11))))
   `(message-separator ((,class (:inherit shadow))))
   `(minibuffer-prompt ((,class (:foreground ,bivrost9 :weight bold))))
   `(mm-command-output ((,class (:foreground ,bivrost9))))
   `(mode-line ((,class (:foreground ,bivrost9 :background ,bivrost2))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-highlight ((,class (:inherit highlight))))
   `(mode-line-inactive ((,class (:foreground ,bivrost5 :background ,bivrost2))))
   `(next-error ((,class (:inherit error))))
   `(nobreak-space ((,class (:foreground ,bivrost4))))
   `(outline-1 ((,class (:foreground ,bivrost9 :weight bold))))
   `(outline-2 ((,class (:inherit outline-1))))
   `(outline-3 ((,class (:inherit outline-1))))
   `(outline-4 ((,class (:inherit outline-1))))
   `(outline-5 ((,class (:inherit outline-1))))
   `(outline-6 ((,class (:inherit outline-1))))
   `(outline-7 ((,class (:inherit outline-1))))
   `(outline-8 ((,class (:inherit outline-1))))
   `(package-description ((,class (:foreground ,bivrost5))))
   `(package-help-section-name ((,class (:foreground ,bivrost9 :weight bold))))
   `(package-name ((,class (:foreground ,bivrost9))))
   `(package-status-available ((,class (:foreground ,bivrost8))))
   `(package-status-avail-obso ((,class (:foreground ,bivrost8 :slant italic))))
   `(package-status-built-in ((,class (:foreground ,bivrost10))))
   `(package-status-dependency ((,class (:foreground ,bivrost9 :slant italic))))
   `(package-status-disabled ((,class (:foreground ,bivrost4))))
   `(package-status-external ((,class (:foreground ,bivrost13 :slant italic))))
   `(package-status-held ((,class (:foreground ,bivrost5 :weight bold))))
   `(package-status-new ((,class (:foreground ,bivrost15))))
   `(package-status-incompat ((,class (:foreground ,bivrost12))))
   `(package-status-installed ((,class (:foreground ,bivrost8 :weight bold))))
   `(package-status-unsigned ((,class (:underline ,bivrost14))))
   `(query-replace ((,class (:foreground ,bivrost9 :background ,bivrost3))))
   `(region ((,class (:foreground ,bivrost-region-highlight-foreground :background ,bivrost-region-highlight-background))))
   `(scroll-bar ((,class (:background ,bivrost4))))
   `(secondary-selection ((,class (:background ,bivrost3))))
   `(show-paren-match-face ((,class (:foreground ,bivrost0 :background ,bivrost9))))
   `(show-paren-mismatch-face ((,class (:background ,bivrost12))))
   `(show-paren-match ((,class (:foreground ,bivrost0 :background ,bivrost9))))
   `(show-paren-mismatch ((,class (:background ,bivrost12))))
   `(success ((,class (:foreground ,bivrost15))))
   `(term ((,class (:foreground ,bivrost5 :background ,bivrost0))))
   `(term-color-black ((,class (:foreground ,bivrost1 :background ,bivrost1))))
   `(term-color-white ((,class (:foreground ,bivrost6 :background ,bivrost6))))
   `(term-color-cyan ((,class (:foreground ,bivrost8 :background ,bivrost8))))
   `(term-color-blue ((,class (:foreground ,bivrost9 :background ,bivrost9))))
   `(term-color-red ((,class (:foreground ,bivrost12 :background ,bivrost12))))
   `(term-color-yellow ((,class (:foreground ,bivrost14 :background ,bivrost14))))
   `(term-color-green ((,class (:foreground ,bivrost15 :background ,bivrost15))))
   `(term-color-magenta ((,class (:foreground ,bivrost16 :background ,bivrost16))))
   `(tool-bar ((,class (:foreground ,bivrost5 :background ,bivrost4))))
   `(tooltip ((,class (:foreground ,bivrost0 :background ,bivrost5))))
   `(trailing-whitespace ((,class (:foreground ,bivrost4))))
   `(tty-menu-disabled-face ((,class (:foreground ,bivrost1))))
   `(tty-menu-enabled-face ((,class (:background ,bivrost3 foreground ,bivrost5))))
   `(tty-menu-selected-face ((,class (:foreground ,bivrost9 :underline t))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,bivrost9))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,bivrost5))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,bivrost5))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,bivrost10))))
   `(vc-conflict-state ((,class (:foreground ,bivrost13))))
   `(vc-edited-state ((,class (:foreground ,bivrost14))))
   `(vc-locally-added-state ((,class (:underline ,bivrost15))))
   `(vc-locked-state ((,class (:foreground ,bivrost11))))
   `(vc-missing-state ((,class (:foreground ,bivrost12))))
   `(vc-needs-update-state ((,class (:foreground ,bivrost13))))
   `(vc-removed-state ((,class (:foreground ,bivrost12))))
   `(vc-state-base ((,class (:foreground ,bivrost5))))
   `(vc-up-to-date-state ((,class (:foreground ,bivrost9))))
   `(vertical-border ((,class (:foreground ,bivrost3))))
   `(which-func ((,class (:foreground ,bivrost9))))
   `(whitespace-big-indent ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-empty ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-hspace ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-indentation ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-line ((,class (:background ,bivrost0))))
   `(whitespace-newline ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-space ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-space-after-tab ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-space-before-tab ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-tab ((,class (:foreground ,bivrost4 :background ,bivrost0))))
   `(whitespace-trailing ((,class (:inherit trailing-whitespace))))
   `(widget-button-pressed ((,class (:foreground ,bivrost10 :background ,bivrost1))))
   `(widget-documentation ((,class (:foreground ,bivrost5))))
   `(widget-field ((,class (:background ,bivrost2 :foreground ,bivrost5))))
   `(widget-single-line-field ((,class (:background ,bivrost3 :foreground ,bivrost5))))
   `(window-divider ((,class (:background ,bivrost4))))
   `(window-divider-first-pixel ((,class (:background ,bivrost4))))
   `(window-divider-last-pixel ((,class (:background ,bivrost4))))

    ;;;; +-----------------+
    ;;;; + Package Support +
    ;;;; +-----------------+
   ;; +--- Syntax ---+

   ;; > Java Development Environment for Emacs
   `(jdee-db-active-breakpoint-face ((,class (:background ,bivrost3 :weight bold))))
   `(jdee-bug-breakpoint-cursor ((,class (:background ,bivrost3))))
   `(jdee-db-requested-breakpoint-face ((,class (:foreground ,bivrost14 :background ,bivrost3 :weight bold))))
   `(jdee-db-spec-breakpoint-face ((,class (:foreground ,bivrost15 :background ,bivrost3 :weight bold))))
   `(jdee-font-lock-api-face ((,class (:foreground ,bivrost5))))
   `(jdee-font-lock-code-face ((,class (:slant italic))))
   `(jdee-font-lock-constant-face ((,class (:foreground ,bivrost-keyword))))
   `(jdee-font-lock-constructor-face ((,class (:foreground ,bivrost-method))))
   `(jdee-font-lock-doc-tag-face ((,class (:foreground ,bivrost8))))
   `(jdee-font-lock-link-face ((,class (:underline t))))
   `(jdee-font-lock-modifier-face ((,class (:foreground ,bivrost-keyword))))
   `(jdee-font-lock-number-face ((,class (:foreground ,bivrost-numeric))))
   `(jdee-font-lock-operator-fac ((,class (:foreground ,bivrost-operator))))
   `(jdee-font-lock-package-face ((,class (:foreground ,bivrost-class))))
   `(jdee-font-lock-pre-face ((,class (:foreground ,bivrost-comment :slant italic))))
   `(jdee-font-lock-private-face ((,class (:foreground ,bivrost-keyword))))
   `(jdee-font-lock-public-face ((,class (:foreground ,bivrost-keyword))))
   `(jdee-font-lock-variable-face ((,class (:foreground ,bivrost-variable))))

   ;; > JavaScript 2
   `(js2-function-call ((,class (:foreground ,bivrost9))))
   `(js2-private-function-call ((,class (:foreground ,bivrost9))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,bivrost7))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,bivrost10))))
   `(js2-external-variable ((,class (:foreground ,bivrost5))))
   `(js2-function-param ((,class (:foreground ,bivrost5))))
   `(js2-jsdoc-value ((,class (:foreground ,bivrost-comment))))
   `(js2-jsdoc-tag ((,class (:foreground ,bivrost8))))
   `(js2-jsdoc-type ((,class (:foreground ,bivrost8))))
   `(js2-private-member ((,class (:foreground ,bivrost5))))
   `(js2-object-property ((,class (:foreground ,bivrost5))))
   `(js2-error ((,class (:foreground ,bivrost12))))
   `(js2-warning ((,class (:foreground ,bivrost14))))
   `(js2-instance-member ((,class (:foreground ,bivrost5))))

   ;; > JavaScript 3
   `(js3-error-face ((,class (:foreground ,bivrost12))))
   `(js3-external-variable-face ((,class (:foreground ,bivrost5))))
   `(js3-function-param-face ((,class (:foreground ,bivrost5))))
   `(js3-instance-member-face ((,class (:foreground ,bivrost5))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,bivrost7))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,bivrost10))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,bivrost10))))
   `(js3-jsdoc-type-face ((,class (:foreground ,bivrost8))))
   `(js3-jsdoc-value-face ((,class (:foreground ,bivrost5))))
   `(js3-magic-paren-face ((,class (:inherit show-paren-match-face))))
   `(js3-private-function-call-face ((,class (:foreground ,bivrost9))))
   `(js3-private-member-face ((,class (:foreground ,bivrost5))))
   `(js3-warning-face ((,class (:foreground ,bivrost14))))

   ;; > Markdown
   `(markdown-blockquote-face ((,class (:foreground ,bivrost-comment))))
   `(markdown-bold-face ((,class (:inherit bold))))
   `(markdown-header-face-1 ((,class (:foreground ,bivrost9))))
   `(markdown-header-face-2 ((,class (:foreground ,bivrost9))))
   `(markdown-header-face-3 ((,class (:foreground ,bivrost9))))
   `(markdown-header-face-4 ((,class (:foreground ,bivrost9))))
   `(markdown-header-face-5 ((,class (:foreground ,bivrost9))))
   `(markdown-header-face-6 ((,class (:foreground ,bivrost9))))
   `(markdown-inline-code-face ((,class (:foreground ,bivrost8))))
   `(markdown-italic-face ((,class (:inherit italic))))
   `(markdown-link-face ((,class (:foreground ,bivrost9))))
   `(markdown-markup-face ((,class (:foreground ,bivrost10))))
   `(markdown-reference-face ((,class (:inherit markdown-link-face))))
   `(markdown-url-face ((,class (:foreground ,bivrost5 :underline t))))

   ;; > Rainbow Delimeters
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,bivrost8)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,bivrost9)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,bivrost10)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,bivrost11)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,bivrost13)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,bivrost14)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,bivrost15)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,bivrost16)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,bivrost12)))

   ;; > Web Mode
   `(web-mode-attr-tag-custom-face ((,class (:foreground ,bivrost-attribute))))
   `(web-mode-builtin-face ((,class (:foreground ,bivrost-keyword))))
   `(web-mode-comment-face ((,class (:foreground ,bivrost-comment))))
   `(web-mode-comment-keyword-face ((,class (:foreground ,bivrost-comment))))
   `(web-mode-constant-face ((,class (:foreground ,bivrost-variable))))
   `(web-mode-css-at-rule-face ((,class (:foreground ,bivrost-annotation))))
   `(web-mode-css-function-face ((,class (:foreground ,bivrost-method))))
   `(web-mode-css-property-name-face ((,class (:foreground ,bivrost-keyword))))
   `(web-mode-css-pseudo-class-face ((,class (:foreground ,bivrost-class))))
   `(web-mode-css-selector-face ((,class (:foreground ,bivrost-keyword))))
   `(web-mode-css-string-face ((,class (:foreground ,bivrost-string))))
   `(web-mode-doctype-face ((,class (:foreground ,bivrost-preprocessor))))
   `(web-mode-function-call-face ((,class (:foreground ,bivrost-method))))
   `(web-mode-function-name-face ((,class (:foreground ,bivrost-method))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,bivrost-attribute))))
   `(web-mode-html-attr-equal-face ((,class (:foreground ,bivrost-punctuation))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,bivrost-string))))
   `(web-mode-html-entity-face ((,class (:foreground ,bivrost-keyword))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,bivrost-punctuation))))
   `(web-mode-html-tag-custom-face ((,class (:foreground ,bivrost-tag))))
   `(web-mode-html-tag-face ((,class (:foreground ,bivrost-tag))))
   `(web-mode-html-tag-namespaced-face ((,class (:foreground ,bivrost-keyword))))
   `(web-mode-json-key-face ((,class (:foreground ,bivrost-class))))
   `(web-mode-json-string-face ((,class (:foreground ,bivrost-string))))
   `(web-mode-keyword-face ((,class (:foreground ,bivrost-keyword))))
   `(web-mode-preprocessor-face ((,class (:foreground ,bivrost-preprocessor))))
   `(web-mode-string-face ((,class (:foreground ,bivrost-string))))
   `(web-mode-symbol-face ((,class (:foreground ,bivrost-variable))))
   `(web-mode-type-face ((,class (:foreground ,bivrost-class))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-variable-name-face ((,class (:foreground ,bivrost-variable))))

   ;; +--- UI ---+
   ;; > Anzu
   `(anzu-mode-line ((,class (:foreground, bivrost9))))
   `(anzu-mode-line-no-match ((,class (:foreground, bivrost12))))

   ;; > Corfu
   `(corfu-default ((,class (:background,bivrost2))))

   ;; > diff-hl
   `(diff-hl-change ((,class (:background ,bivrost14))))
   `(diff-hl-insert ((,class (:background ,bivrost15))))
   `(diff-hl-delete ((,class (:background ,bivrost12))))

   ;; > Eat
   ;; Standard ANSI colors
   `(eat-term-color-0 ((,class (:foreground ,bivrost0 :background ,bivrost0))))  ; Black
   `(eat-term-color-1 ((,class (:foreground ,bivrost12 :background ,bivrost12)))) ; Red
   `(eat-term-color-2 ((,class (:foreground ,bivrost15 :background ,bivrost15)))) ; Green
   `(eat-term-color-3 ((,class (:foreground ,bivrost14 :background ,bivrost14)))) ; Yellow
   `(eat-term-color-4 ((,class (:foreground ,bivrost9 :background ,bivrost9))))   ; Blue
   `(eat-term-color-5 ((,class (:foreground ,bivrost16 :background ,bivrost16)))) ; Magenta
   `(eat-term-color-6 ((,class (:foreground ,bivrost8 :background ,bivrost8))))   ; Cyan
   `(eat-term-color-7 ((,class (:foreground ,bivrost5 :background ,bivrost5))))   ; White

   ;; Bright variants
   `(eat-term-color-8 ((,class (:foreground ,bivrost2 :background ,bivrost2))))   ; Bright Black
   `(eat-term-color-9 ((,class (:foreground ,bivrost13 :background ,bivrost13)))) ; Bright Red
   `(eat-term-color-10 ((,class (:foreground ,bivrost15 :background ,bivrost15)))) ; Bright Green
   `(eat-term-color-11 ((,class (:foreground ,bivrost14 :background ,bivrost14)))) ; Bright Yellow
   `(eat-term-color-12 ((,class (:foreground ,bivrost10 :background ,bivrost10)))) ; Bright Blue
   `(eat-term-color-13 ((,class (:foreground ,bivrost16 :background ,bivrost16)))) ; Bright Magenta
   `(eat-term-color-14 ((,class (:foreground ,bivrost8 :background ,bivrost8))))   ; Bright Cyan
   `(eat-term-color-15 ((,class (:foreground ,bivrost7 :background ,bivrost7))))   ; Bright White

   ;; Terminal attributes
   `(eat-term-bold ((,class (:weight bold))))
   `(eat-term-italic ((,class (:slant italic))))
   `(eat-term-faint ((,class (:weight light))))

   ;; > Eglot
   `(eglot-diagnostic-tag-deprecated-face ((,class (:strike-through t :foreground ,bivrost13))))
   `(eglot-diagnostic-tag-unnecessary-face ((,class (:foreground ,bivrost4 :slant italic))))
   `(eglot-highlight-symbol-face ((,class (:background ,bivrost2 :weight bold))))
   `(eglot-inlay-hint-face ((,class (:height 0.9 :foreground ,bivrost4 :slant italic))))
   `(eglot-mode-line ((,class (:foreground ,bivrost9 :weight bold))))
   `(eglot-parameter-hint-face ((,class (:height 0.9 :foreground ,bivrost10 :slant italic))))
   `(eglot-type-hint-face ((,class (:height 0.9 :foreground ,bivrost8 :slant italic))))

   ;; > Eshell faces
   `(eshell-prompt ((,class (:foreground ,bivrost9 :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,bivrost13))))       ; Archives (.zip, .tar, etc.)
   `(eshell-ls-backup ((,class (:foreground ,bivrost4 :slant italic)))) ; Backup files (~, .bak, etc.)
   `(eshell-ls-clutter ((,class (:foreground ,bivrost4 :slant italic)))) ; Clutter files (.DS_Store, etc.)
   `(eshell-ls-directory ((,class (:foreground ,bivrost9 :weight bold)))) ; Directories
   `(eshell-ls-executable ((,class (:foreground ,bivrost15 :weight bold)))) ; Executable files
   `(eshell-ls-missing ((,class (:foreground ,bivrost12 :strike-through t)))) ; Missing files
   `(eshell-ls-product ((,class (:foreground ,bivrost14))))       ; Product files
   `(eshell-ls-readonly ((,class (:foreground ,bivrost14 :background ,bivrost1)))) ; Read-only files
   `(eshell-ls-special ((,class (:foreground ,bivrost16 :weight bold)))) ; Special files (devices, sockets, etc.)
   `(eshell-ls-symlink ((,class (:foreground ,bivrost8 :slant italic)))) ; Symbolic links
   `(eshell-ls-unreadable ((,class (:foreground ,bivrost4 :background ,bivrost1)))) ; Unreadable files

   ;; > Evil
   `(evil-ex-info ((,class (:foreground ,bivrost9))))
   `(evil-ex-substitute-replacement ((,class (:foreground ,bivrost10))))
   `(evil-ex-substitute-matches ((,class (:inherit isearch))))

   ;; Shell prompt annotations
   `(eat-shell-prompt-annotation-failure ((,class (:foreground ,bivrost12))))  ; Red for failure
   `(eat-shell-prompt-annotation-success ((,class (:foreground ,bivrost15))))  ; Green for success
   `(eat-shell-prompt-annotation-running ((,class (:foreground ,bivrost14))))  ; Yellow for running

   ;; > Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,bivrost12)))))
   `(flycheck-fringe-error ((,class (:foreground ,bivrost12 :weight bold))))
   `(flycheck-fringe-info ((,class (:foreground ,bivrost9 :weight bold))))
   `(flycheck-fringe-warning ((,class (:foreground ,bivrost14 :weight bold))))
   `(flycheck-info ((,class (:underline (:style wave :color ,bivrost9)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,bivrost14)))))

   ;; > Flymake
   `(flymake-error ((,class (:underline (:style wave :color ,bivrost12)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,bivrost14)))))
   `(flymake-note ((,class (:underline (:style wave :color ,bivrost15)))))
   `(flymake-error-echo ((,class (:foreground ,bivrost12 :weight bold))))
   `(flymake-warning-echo ((,class (:foreground ,bivrost14 :weight bold))))
   `(flymake-note-echo ((,class (:foreground ,bivrost15 :weight bold))))
   `(flymake-error-echo-at-eol ((,class (:foreground ,bivrost12 :background ,bivrost2 :weight bold))))
   `(flymake-warning-echo-at-eol ((,class (:foreground ,bivrost14 :background ,bivrost2 :weight bold))))
   `(flymake-note-echo-at-eol ((,class (:foreground ,bivrost15 :background ,bivrost2 :weight bold))))

   `(flymake-end-of-line-diagnostics-face ((,class (:background ,bivrost2 :foreground ,bivrost5))))
   `(flymake-eol-information-face ((,class (:background ,bivrost2 :foreground ,bivrost10))))

   ;; > Git Gutter
   `(git-gutter:modified ((,class (:foreground ,bivrost14))))
   `(git-gutter:added ((,class (:foreground ,bivrost15))))
   `(git-gutter:deleted ((,class (:foreground ,bivrost12))))

   ;; > Git Gutter Plus
   `(git-gutter+-modified ((,class (:foreground ,bivrost14))))
   `(git-gutter+-added ((,class (:foreground ,bivrost15))))
   `(git-gutter+-deleted ((,class (:foreground ,bivrost12))))

   ;; > ivy-mode
   `(ivy-current-match ((,class (:inherit region))))
   `(ivy-minibuffer-match-face-1 ((,class (:inherit default))))
   `(ivy-minibuffer-match-face-2 ((,class (:background ,bivrost8 :foreground ,bivrost0))))
   `(ivy-minibuffer-match-face-3 ((,class (:background ,bivrost9 :foreground ,bivrost0))))
   `(ivy-minibuffer-match-face-4 ((,class (:background ,bivrost10 :foreground ,bivrost0))))
   `(ivy-remote ((,class (:foreground ,bivrost15))))
   `(ivy-posframe ((,class (:background ,bivrost1))))
   `(ivy-posframe-border ((,class (:background ,bivrost1))))
   `(ivy-remote ((,class (:foreground ,bivrost15))))

   ;; > Magit
   `(magit-branch ((,class (:foreground ,bivrost8 :weight bold))))
   `(magit-diff-context-highlight ((,class (:background ,bivrost3))))
   `(magit-diff-file-header ((,class (:foreground ,bivrost9 :box (:color ,bivrost9)))))
   `(magit-diffstat-added ((,class (:foreground ,bivrost15))))
   `(magit-diffstat-removed ((,class (:foreground ,bivrost12))))
   `(magit-hash ((,class (:foreground ,bivrost9))))
   `(magit-hunk-heading ((,class (:foreground ,bivrost10))))
   `(magit-hunk-heading-highlight ((,class (:foreground ,bivrost10 :background ,bivrost3))))
   `(magit-item-highlight ((,class (:foreground ,bivrost9 :background ,bivrost3))))
   `(magit-log-author ((,class (:foreground ,bivrost8))))
   `(magit-process-ng ((,class (:foreground ,bivrost14 :weight bold))))
   `(magit-process-ok ((,class (:foreground ,bivrost15 :weight bold))))
   `(magit-section-heading ((,class (:foreground ,bivrost8 :weight bold))))
   `(magit-section-highlight ((,class (:background ,bivrost3))))

   ;; > MU4E
   `(mu4e-header-marks-face ((,class (:foreground ,bivrost10))))
   `(mu4e-title-face ((,class (:foreground ,bivrost9))))
   `(mu4e-header-key-face ((,class (:foreground ,bivrost9))))
   `(mu4e-highlight-face ((,class (:highlight))))
   `(mu4e-flagged-face ((,class (:foreground ,bivrost14))))
   `(mu4e-unread-face ((,class (:foreground ,bivrost14 :weight bold))))
   `(mu4e-link-face ((,class (:underline t))))

   ;; > Org
   `(org-level-1 ((,class (:foreground ,bivrost8 :weight extra-bold))))
   `(org-level-2 ((,class (:foreground ,bivrost9 :weight bold))))
   `(org-level-3 ((,class (:foreground ,bivrost10 :weight semi-bold))))
   `(org-level-4 ((,class (:foreground ,bivrost11 :weight normal))))
   `(org-level-5 ((,class (:inherit org-level-4))))
   `(org-level-6 ((,class (:inherit org-level-4))))
   `(org-level-7 ((,class (:inherit org-level-4))))
   `(org-level-8 ((,class (:inherit org-level-4))))
   `(org-agenda-structure ((,class (:foreground ,bivrost10))))
   `(org-agenda-date ((,class (:foreground ,bivrost9 :underline nil))))
   `(org-agenda-done ((,class (:foreground ,bivrost15))))
   `(org-agenda-dimmed-todo-face ((,class (:background ,bivrost14))))
   `(org-block ((,class (:foreground ,bivrost5))))
   `(org-block-background ((,class (:background ,bivrost0))))
   `(org-block-begin-line ((,class (:foreground ,bivrost8))))
   `(org-block-end-line ((,class (:foreground ,bivrost8))))
   `(org-checkbox ((,class (:foreground ,bivrost10))))
   `(org-checkbox-statistics-done ((,class (:foreground ,bivrost15))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,bivrost14))))
   `(org-code ((,class (:foreground ,bivrost8))))
   `(org-column ((,class (:background ,bivrost3))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,bivrost9))))
   `(org-document-info ((,class (:foreground ,bivrost5))))
   `(org-document-info-keyword ((,class (:foreground ,bivrost4 :weight bold))))
   `(org-document-title ((,class (:foreground ,bivrost9 :weight bold))))
   `(org-done ((,class (:foreground ,bivrost15 :weight bold))))
   `(org-ellipsis ((,class (:foreground ,bivrost4))))
   `(org-footnote ((,class (:foreground ,bivrost9))))
   `(org-formula ((,class (:foreground ,bivrost10))))
   `(org-hide ((,class (:foreground ,bivrost0 :background ,bivrost0))))
   `(org-link ((,class (:underline t))))
   `(org-scheduled ((,class (:foreground ,bivrost15))))
   `(org-scheduled-previously ((,class (:foreground ,bivrost14))))
   `(org-scheduled-today ((,class (:foreground ,bivrost9))))
   `(org-special-keyword ((,class (:foreground ,bivrost10))))
   `(org-table ((,class (:foreground ,bivrost10))))
   `(org-todo ((,class (:foreground ,bivrost14 :weight bold))))
   `(org-upcoming-deadline ((,class (:foreground ,bivrost13))))
   `(org-warning ((,class (:foreground ,bivrost14 :weight bold))))
   `(font-latex-bold-face ((,class (:inherit bold))))
   `(font-latex-italic-face ((,class (:slant italic))))
   `(font-latex-string-face ((,class (:foreground ,bivrost15))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,bivrost10))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,bivrost5))))
   `(ido-only-match ((,class (:foreground ,bivrost9))))
   `(org-sexp-date ((,class (:foreground ,bivrost8))))
   `(ido-first-match ((,class (:foreground ,bivrost9 :weight bold))))
   `(ido-subdir ((,class (:foreground ,bivrost10))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-agenda-date-weekend ((,class (:foreground ,bivrost10))))
   `(org-agenda-date-today ((,class (:foreground ,bivrost9 :weight bold))))
   `(org-agenda-done ((,class (:foreground ,bivrost15))))
   `(org-verbatim ((,class (:foreground ,bivrost8))))

   ;; > Powerline
   `(powerline-active1 ((,class (:foreground ,bivrost5 :background ,bivrost1))))
   `(powerline-active2 ((,class (:foreground ,bivrost5 :background ,bivrost4))))
   `(powerline-inactive1 ((,class (:background ,bivrost3))))
   `(powerline-inactive2 ((,class (:background ,bivrost3))))

   ;; > Powerline Evil
   `(powerline-evil-base-face ((,class (:foreground ,bivrost5))))
   `(powerline-evil-normal-face ((,class (:background ,bivrost9))))
   `(powerline-evil-insert-face ((,class (:foreground ,bivrost0 :background ,bivrost5))))
   `(powerline-evil-visual-face ((,class (:foreground ,bivrost0 :background ,bivrost8))))
   `(powerline-evil-replace-face ((,class (:foreground ,bivrost0 :background ,bivrost10))))

   ;; Spaceline
   `(spaceline-evil-normal ((,class (:background ,bivrost9))))
   `(spaceline-evil-insert ((,class (:foreground ,bivrost5 :background ,bivrost15))))
   `(spaceline-evil-visual ((,class (:foreground ,bivrost0 :background ,bivrost8))))
   `(spaceline-evil-replace ((,class (:foreground ,bivrost5 :background ,bivrost10))))
   `(spaceline-evil-unmodified ((,class (:foreground ,bivrost5 :background ,bivrost9))))
   `(spaceline-highlight-face ((,class (:background ,bivrost2))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bivrost)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; bivrost-theme.el ends here
