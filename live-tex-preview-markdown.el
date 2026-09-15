;;; live-tex-preview-markdown.el --- Markdown and Quarto math -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Jorge Noreña
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.6"))

;;; Commentary:
;; A frontend to the existing renderer and placement API.  Markdown-mode owns
;; code/YAML/indentation recognition.  Its math fontification regexps accept
;; currency and do not cover all four delimiters uniformly, so this frontend
;; applies its own deliberately conservative dollar policy:
;; - $$ takes precedence; runs of three or more dollars are not delimiters.
;; - Single dollars are confined to one line, with no adjacent inner whitespace.
;; - A single-dollar opener is not preceded by a word character; a closer
;;   cannot be followed by a word character.  Numeric-leading math is allowed:
;;   a valid same-line pair, not the first character, distinguishes it from currency.
;; - The first unescaped dollar must be a valid closer; never skip an invalid
;;   one to find a later equation.  Backslash delimiters avoid currency ambiguity.
;; - No fragment may cross code, metadata, or a blank paragraph boundary.
;; Existing-overlay lookup is bounded to that overlay, not the whole document.
;; No arbitrary TeX environments or Quarto/YAML configuration resolution.
;; Licensed under GPL version 3 or later, without warranty; see COPYING.

;;; Code:
(require 'live-tex-preview)
(require 'markdown-mode)
(declare-function live-tex-preview-polymode--setup "live-tex-preview-polymode")
(declare-function live-tex-preview-polymode--excluded-p "live-tex-preview-polymode")

(defgroup live-tex-preview-markdown nil
  "Math previews in Markdown and Quarto prose."
  :group 'live-tex-preview)

(defcustom live-tex-preview-markdown-preamble
  "\\documentclass{article}\n\\usepackage{amsmath,amssymb,amsfonts,xcolor}\n"
  "Default preview preamble for Markdown and Quarto."
  :type 'string :group 'live-tex-preview-markdown)

(defcustom live-tex-preview-markdown-extra-preamble ""
  "Extra preview packages and macros, also usable as a directory-local setting.
YAML header-includes and Quarto project configuration are not interpreted."
  :type 'string :group 'live-tex-preview-markdown)

(defconst live-tex-preview-markdown--openers
  (regexp-opt '("$$" "$" "\\(" "\\[")))
(defvar-local live-tex-preview-markdown--structural-edit nil)
(defvar-local live-tex-preview-markdown--validator nil)

(defun live-tex-preview-markdown--structure-p (beg end)
  "Return non-nil if BEG END contains Markdown block/code punctuation."
  (or (string-match-p "[`~\n<>-]" (buffer-substring-no-properties beg end))
      (save-excursion
        (goto-char beg)
        (string-match-p "\\`[ \t]*\\'"
                        (buffer-substring-no-properties (line-beginning-position) end)))))

(defun live-tex-preview-markdown--before-change (beg end)
  "Remember structural deletions before a change between BEG and END."
  (setq live-tex-preview-markdown--structural-edit
        (live-tex-preview-markdown--structure-p beg end)))

(defun live-tex-preview-markdown--after-change (beg end _old-length)
  "Recheck existing overlays after a structural Markdown change."
  (when (and live-tex-preview-markdown--validator
             (not (get-char-property beg 'live-tex-preview-type))
             (or live-tex-preview-markdown--structural-edit
                 (live-tex-preview-markdown--structure-p beg end)))
    (funcall live-tex-preview-markdown--validator)))

(defun live-tex-preview-markdown--validate-overlays ()
  "Remove existing previews that have become code or metadata.
This checks only existing overlays after structural edits, never scans for
new fragments.  Ordinary typing inside a formula uses bounded live lookup."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'live-tex-preview-type)
                 (not (live-tex-preview-mode--fragment-for-overlay ov)))
        (live-tex-preview-live--clearout ov)
        (run-hook-with-args 'live-tex-preview-overlay-close-functions ov)
        (delete-overlay ov)))))

(defun live-tex-preview-markdown--mode-changed ()
  "Install or remove Markdown structural-edit validation with the mode."
  (if live-tex-preview-mode
      (progn
        (setq live-tex-preview-markdown--validator
              (live-tex-preview-live--debounce #'live-tex-preview-markdown--validate-overlays
                                               live-tex-preview-update-delay))
        (add-hook 'before-change-functions #'live-tex-preview-markdown--before-change nil t)
        (add-hook 'after-change-functions #'live-tex-preview-markdown--after-change 95 t))
    (remove-hook 'before-change-functions #'live-tex-preview-markdown--before-change t)
    (remove-hook 'after-change-functions #'live-tex-preview-markdown--after-change t)
    (setq live-tex-preview-markdown--validator nil)))

(defun live-tex-preview-markdown--directory ()
  "Return the Markdown document's directory."
  (file-name-as-directory
   (if buffer-file-name (file-name-directory buffer-file-name) default-directory)))

(defun live-tex-preview-markdown--place (entries)
  "Render Markdown ENTRIES with the configured math preamble."
  (let ((directory (live-tex-preview-markdown--directory)))
    (live-tex-preview-place
     entries :preamble (concat live-tex-preview-markdown-preamble "\n"
                               live-tex-preview-markdown-extra-preamble)
     :page-width live-tex-preview-page-width :input-directory directory
     :cache-directory (and live-tex-preview-cache-directory
                           (expand-file-name live-tex-preview-cache-directory directory)))))

(defun live-tex-preview-markdown--block-p (latex)
  "Return non-nil for display LATEX."
  (or (string-prefix-p "$$" latex) (string-prefix-p "\\[" latex)))

(defun live-tex-preview-markdown--word-p (char)
  "Return non-nil for a word or underscore CHAR."
  (and char (or (eq char ?_) (eq (char-syntax char) ?w))))

(defun live-tex-preview-markdown--space-p (char)
  "Return non-nil for whitespace CHAR (or end of buffer)."
  (or (null char) (memq char '(?\s ?\t ?\n ?\r))))

(defun live-tex-preview-markdown--excluded-p (pos)
  "Return non-nil when markdown-mode recognizes non-prose at POS.
Call after syntax propertization, in the host Markdown buffer.  Code block
recognition includes tilde/backtick fences, YAML, and indented code.
Inline code uses markdown-mode's delimiter matcher, not font-lock faces."
  (save-excursion
    (save-match-data
      (or (markdown-code-block-at-pos pos)
          ;; markdown-mode marks an unfinished fence's opener but not its
          ;; body.  Treat that body as code until its closing fence is typed.
          (cl-some
           (lambda (property)
             (goto-char pos)
             (when-let ((previous (markdown-find-previous-prop property)))
               (goto-char (car previous))
               (null (cadr (markdown-get-enclosing-fenced-block-construct)))))
           (markdown-get-fenced-block-begin-properties))
          (get-text-property pos 'markdown-yaml-metadata-section)
          (markdown-inline-code-at-pos-p pos)
          (nth 4 (syntax-ppss pos))
          (and (featurep 'live-tex-preview-polymode)
               (live-tex-preview-polymode--excluded-p pos))))))

(defun live-tex-preview-markdown--prose-range-p (beg end)
  "Check that BEG to END stays in prose rather than crossing a code region.
Only the candidate fragment is inspected, including each line and backtick."
  (save-excursion
    (goto-char beg)
    (and (not (live-tex-preview-markdown--excluded-p beg))
         (not (live-tex-preview-markdown--excluded-p (1- end)))
         (not (re-search-forward "\n[ \t]*\n" end t))
         (progn
           (goto-char beg)
           (let ((valid t))
             (while (and valid (re-search-forward "^\\|`" end t))
               (let ((pos (match-beginning 0)))
                 (when (live-tex-preview-markdown--excluded-p pos) (setq valid nil)))
               (when (= (match-beginning 0) (match-end 0)) (forward-char 1)))
             valid)))))

(defun live-tex-preview-markdown--fragment-at (start &optional limit)
  "Return (BEG END LATEX) for an opener at START, bounded by LIMIT.
During live edits LIMIT is the existing overlay end.  Reject a broken closer
instead of consuming any delimiter belonging to the next equation."
  (save-excursion
    (save-match-data
      (let ((limit (min (or limit live-tex-preview--fragment-limit (point-max)) (point-max))))
        (goto-char start)
        (when (and (< start limit) (looking-at live-tex-preview-markdown--openers)
                   (not (live-tex-preview--escaped-p start)))
          (let* ((opener (match-string-no-properties 0))
                 (body (match-end 0))
                 (dollar (string-prefix-p "$" opener))
                 (single (equal opener "$"))
                 (close (cond (dollar opener) ((equal opener "\\(") "\\)") (t "\\]")))
                 finish close-start)
            (when (and (not (live-tex-preview-markdown--excluded-p start))
                       (or (not dollar)
                           (and (not (eq (char-before start) ?$))
                                (not (eq (char-after body) ?$))))
                       (or (not single)
                           (and (not (live-tex-preview-markdown--word-p (char-before start)))
                                (not (live-tex-preview-markdown--space-p (char-after body))))))
              (goto-char body)
              (when single (setq limit (min limit (line-end-position))))
              (catch 'closed
                (while (search-forward close limit t)
                  (let ((pos (- (point) (length close))))
                    (unless (live-tex-preview--escaped-p pos)
                      (setq close-start pos finish (point))
                      (throw 'closed t)))))
              (when (and finish (< body close-start)
                         (not (string-blank-p (buffer-substring-no-properties body close-start)))
                         (or (not dollar) (not (eq (char-after finish) ?$)))
                         (or (not single)
                             (and (not (live-tex-preview-markdown--space-p (char-before close-start)))
                                  (not (eq (char-before close-start) ?$))
                                  (not (live-tex-preview-markdown--word-p (char-after finish)))))
                         (live-tex-preview-markdown--prose-range-p start finish))
                (list start finish (buffer-substring-no-properties start finish))))))))))

(defun live-tex-preview-markdown--scan-region (beg end)
  "Find Markdown math between BEG and END using host syntax exclusions."
  (when (> beg end) (cl-rotatef beg end))
  (save-excursion
    (save-restriction
      ;; Parse with surrounding document context, but return only requested
      ;; candidates.  Never expose a code inner buffer's narrowed text as prose.
      (widen)
      (syntax-propertize end)
      (goto-char beg)
      (let (entries)
        (while (re-search-forward live-tex-preview-markdown--openers end t)
          (let* ((start (match-beginning 0))
                 (entry (live-tex-preview-markdown--fragment-at start end)))
            (when entry
              (push entry entries)
              (goto-char (cadr entry)))))
        (nreverse entries)))))

(defun live-tex-preview-markdown--live-fragment (start)
  "Look up a tracked fragment at START with fresh, bounded syntax context."
  (save-restriction
    (widen)
    (syntax-propertize (or live-tex-preview--fragment-limit start))
    (live-tex-preview-markdown--fragment-at start)))

(defun live-tex-preview-markdown--setup ()
  "Select the Markdown frontend without changing math fontification settings."
  (setq-local live-tex-preview-fragment-function #'live-tex-preview-markdown--live-fragment
              live-tex-preview-render-function #'live-tex-preview-markdown--place
              live-tex-preview-block-function #'live-tex-preview-markdown--block-p
              live-tex-preview-scan-function #'live-tex-preview-markdown--scan-region
              live-tex-preview-directory-function #'live-tex-preview-markdown--directory)
  (add-hook 'live-tex-preview-mode-hook #'live-tex-preview-markdown--mode-changed nil t)
  (when (bound-and-true-p polymode-mode)
    (require 'live-tex-preview-polymode)
    (live-tex-preview-polymode--setup)))

(provide 'live-tex-preview-markdown)
;;; live-tex-preview-markdown.el ends here
