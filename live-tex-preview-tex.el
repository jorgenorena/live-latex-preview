;;; live-tex-preview-tex.el --- LaTeX frontend -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.
;; Copyright (C) 2026 Jorge Noreña
;; Authors: TEC <contact@tecosaur.net>, Karthik Chikmagalur
;; Maintainer: Jorge Noreña
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, tools

;;; Commentary:
;; Derived in part from GNU Org's org-latex-preview.el, by TEC and
;; Karthik Chikmagalur; extraction source:
;; https://git.tecosaur.net/tec/org-mode.git
;; commit 1ef59f0aa02e3cff40bae68b756a29bc2001739e.
;; Adaptations and the LaTeX frontend by Jorge Noreña.
;; This package is free software under GPL version 3 or (at your option)
;; any later version, WITHOUT ANY WARRANTY.  See COPYING for details.

;;; Code:

(require 'live-tex-preview)

(defun live-tex-preview-tex--setup ()
  "Select TeX scanning and refresh master/preamble information."
  (setq-local live-tex-preview-fragment-function #'live-tex-preview--fragment-at
              live-tex-preview-render-function #'live-tex-preview--place
              live-tex-preview-block-function #'live-tex-preview--block-fragment-p
              live-tex-preview-scan-function #'live-tex-preview--scan-region
              live-tex-preview-directory-function #'live-tex-preview--main-dir)
  (live-tex-preview--reset-caches))
(defvar TeX-master)
(declare-function TeX-master-file "tex")

(defcustom live-tex-preview-environments
  '("equation" "equation*" "align" "align*" "alignat" "alignat*"
    "gather" "gather*" "multline" "multline*" "flalign" "flalign*"
    "displaymath" "eqnarray" "eqnarray*" "math" "dmath" "dmath*")
  "Math environments whose \\begin..\\end blocks should be previewed."
  :type '(repeat string)
  :group 'live-tex-preview)

(defcustom live-tex-preview-unnumber t
  "When non-nil, strip equation numbers from previews.
Numbered math environments (see `live-tex-preview-numbered-environments')
are converted to their starred, unnumbered variants before rendering — e.g.
`equation' becomes `equation*'.  Only the text sent to the LaTeX compiler is
changed; the buffer itself is never modified."
  :type 'boolean
  :group 'live-tex-preview)

(defcustom live-tex-preview-numbered-environments
  '("equation" "align" "alignat" "gather" "multline" "flalign" "eqnarray"
    "dmath")
  "Numbered math environments that have a starred unnumbered variant.
Used by `live-tex-preview-unnumber' to convert e.g. `align' to `align*'."
  :type '(repeat string)
  :group 'live-tex-preview)

(defcustom live-tex-preview-default-preamble
  "\\documentclass{article}\n\\usepackage{amsmath,amssymb,amsfonts}\n"
  "Fallback preamble used when the buffer has no \\begin{document}.
When the buffer does have one, the real document preamble (everything
before \\begin{document}) is used instead."
  :type 'string
  :group 'live-tex-preview)

(defcustom live-tex-preview-extra-preamble
  "\\usepackage{xcolor}\n"
  "Preamble lines appended to the document preamble for previews.
The preview engine emits \\color/\\pagecolor commands to match fragment
colours to the buffer, so a colour package (xcolor or color) must be
loaded.  The document being edited may not load one itself, so we ensure
it here.  Re-loading xcolor with no options when the document already
loads it is harmless."
  :type 'string
  :group 'live-tex-preview)


(defun live-tex-preview--in-comment-p (pos)
  "Non-nil if POS is inside a TeX comment.
Works in temporary preamble buffers as well as LaTeX major modes."
  (save-excursion
    (goto-char pos)
    (let ((end pos) found)
      (beginning-of-line)
      (while (and (not found) (search-forward "%" end t))
        (unless (live-tex-preview--escaped-p (1- (point))) (setq found t)))
      found)))


(defun live-tex-preview--opener-re ()
  "Regexp matching any math opener.
Capture groups: 1 = environment name, 2 = \\[, 3 = \\(, 4 = $$, 5 = $."
  (concat
   "\\\\begin{\\(?1:" (regexp-opt live-tex-preview-environments) "\\)}"
   "\\|\\(?2:" (regexp-quote "\\[") "\\)"
   "\\|\\(?3:" (regexp-quote "\\(") "\\)"
   "\\|\\(?4:\\$\\$\\)"
   "\\|\\(?5:\\$\\)"))

(defun live-tex-preview--find-inline-dollar-end ()
  "From just after an opening `$', move past the next unescaped `$'.
Return non-nil on success, leaving point just after the closing `$'."
  (live-tex-preview--find-delimiter "$"))

(defun live-tex-preview--find-delimiter (delimiter)
  "Move past DELIMITER outside comments and escaped commands."
  (let (found)
    (while (and (not found) (search-forward delimiter nil t))
      (let ((start (- (point) (length delimiter))))
        (unless (or (live-tex-preview--escaped-p start)
                    (live-tex-preview--in-comment-p start))
          (setq found t))))
    found))

(defun live-tex-preview--fragment-bounds (mb env g2 g3 g4 g5)
  "Return (MB . END) for a fragment whose opener matched at MB, or nil.
Point must be just after the opener.  ENV is the environment name (or
nil), and G2..G5 are the `match-beginning' of the \\[, \\(, $$ and $
openers respectively (see `live-tex-preview--opener-re')."
  (let ((end (cond
              (env (and (live-tex-preview--find-delimiter (concat "\\end{" env "}")) (point)))
              (g2  (and (live-tex-preview--find-delimiter "\\]") (point)))
              (g3  (and (live-tex-preview--find-delimiter "\\)") (point)))
              (g4  (and (live-tex-preview--find-delimiter "$$") (point)))
              (g5  (and (live-tex-preview--find-inline-dollar-end) (point))))))
    (and end (cons mb end))))

;;; Scanner

(defun live-tex-preview--document-body-bounds ()
  "Return the bounds of this buffer's document body, or nil.
The returned cons is (BEG . END), with BEG just after
`\\begin{document}' and END just before `\\end{document}'.  If the buffer has
a document opener but no closer yet, END is `point-max'.  Buffers without a
document environment are treated as include/standalone fragments and return
nil, so callers can scan their full requested region."
  (save-excursion
    (goto-char (point-min))
    (when (live-tex-preview--find-delimiter "\\begin{document}")
      (let ((beg (point)))
        (cons beg
              (if (live-tex-preview--find-delimiter "\\end{document}")
                  (- (point) (length "\\end{document}"))
                (point-max)))))))

(defun live-tex-preview--scan-region (beg end)
  "Return math fragments between BEG and END as a list of (BEG END VALUE).
In a complete LaTeX document, restrict the requested region to the document
body.  This prevents delimiter commands in macro definitions in the preamble
from being mistaken for math.  Buffers without `\\begin{document}' are scanned
as requested, which preserves support for included and standalone TeX files."
  (when (> beg end)
    (cl-rotatef beg end))
  (when-let ((body (live-tex-preview--document-body-bounds)))
    (setq beg (max beg (car body))
          end (min end (cdr body))))
  (let ((re (live-tex-preview--opener-re))
        (case-fold-search nil)
        entries)
    (when (< beg end)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward re end t)
          ;; Capture all match info into locals NOW.  The comment/escape
          ;; checks below call `syntax-ppss', which runs `syntax-propertize'
          ;; and clobbers the global match-data, so we must not read groups
          ;; after them.
          (let* ((mb (match-beginning 0))
                 (env (and (match-beginning 1) (match-string 1)))
                 (g2 (match-beginning 2))
                 (g3 (match-beginning 3))
                 (g4 (match-beginning 4))
                 (g5 (match-beginning 5)))
            (cond
             ;; Opener inside a comment: ignore, keep scanning after it.
             ((live-tex-preview--in-comment-p mb) nil)
             ;; Escaped \$ (or \$$): not math.
             ((live-tex-preview--escaped-p mb)
              nil)
             (t
              (let ((frag (live-tex-preview--fragment-bounds mb env g2 g3 g4 g5)))
                (when (and frag (<= (cdr frag) end))
                  (push (list (car frag) (cdr frag)
                              (live-tex-preview--unnumber
                               (buffer-substring-no-properties
                                (car frag) (cdr frag))))
                        entries)))))))))
    (nreverse entries)))

(defun live-tex-preview--fragment-at (start)
  "Return (BEG END VALUE) for the math fragment whose opener is at START.
Return nil if START is not the start of a recognisable fragment (e.g. its
delimiters were edited away).  Used to re-derive a fragment's current
extent after it has been edited in place."
  (save-excursion
    (goto-char start)
    (let ((case-fold-search nil))
      (when (looking-at (live-tex-preview--opener-re))
        (let ((mb  (match-beginning 0))
              (env (and (match-beginning 1) (match-string 1)))
              (g2  (match-beginning 2))
              (g3  (match-beginning 3))
              (g4  (match-beginning 4))
              (g5  (match-beginning 5)))
          (goto-char (match-end 0))
          (let ((frag (live-tex-preview--fragment-bounds mb env g2 g3 g4 g5)))
            (and frag
                 (list (car frag) (cdr frag)
                       (live-tex-preview--unnumber
                        (buffer-substring-no-properties (car frag) (cdr frag)))))))))))

(defun live-tex-preview--fragment-at-point ()
  "Return the math fragment containing point, or nil.
Existing preview overlays are preferred because they give exact bounds even when
the rendered image hides the source.  Otherwise scan the buffer and choose the
smallest fragment containing point."
  (or (cl-some
       (lambda (ov)
         (and (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
              (or (live-tex-preview-mode--fragment-for-overlay ov)
                  (live-tex-preview--fragment-at (overlay-start ov)))))
       (overlays-at (point)))
      (let ((pos (point))
            best)
        (dolist (frag (live-tex-preview--scan-region (point-min) (point-max)))
          (when (and (<= (nth 0 frag) pos)
                     (<= pos (nth 1 frag))
                     (or (null best)
                         (< (- (nth 1 frag) (nth 0 frag))
                            (- (nth 1 best) (nth 0 best)))))
            (setq best frag)))
        best)))

(defun live-tex-preview--block-fragment-p (value)
  "Non-nil if fragment string VALUE is display math (shown below, not inline).
Display: \\[ \\], $$ $$, and \\begin{env} environments.  Inline: $ $, \\( \\)."
  (or (string-prefix-p "\\[" value)
      (string-prefix-p "$$" value)
      (string-prefix-p "\\begin{" value)))

(defun live-tex-preview--unnumber (value)
  "Return fragment string VALUE with numbered environments starred.
When `live-tex-preview-unnumber' is non-nil, each \\begin/\\end of an
environment in `live-tex-preview-numbered-environments' gets a trailing
`*' so the preview carries no equation number.  VALUE is returned unchanged
when the option is off or no such environment is present."
  (if (not live-tex-preview-unnumber)
      value
    (let ((re (concat "\\\\\\(?:begin\\|end\\){\\(?:"
                      (regexp-opt live-tex-preview-numbered-environments)
                      "\\)}")))
      (replace-regexp-in-string
       re
       ;; M is e.g. "\\begin{equation}" or "\\end{align}"; turn the closing
       ;; "}" into "*}".  Using a function avoids replacement-string escaping.
       (lambda (m) (concat (substring m 0 -1) "*}"))
       value t t))))

;;; Main file resolution (multi-file projects) and preamble

(defvar-local live-tex-preview--main-file-cache 'unset
  "Cached result of `live-tex-preview--main-file'.")
(defvar-local live-tex-preview--preamble-cache nil
  "Cached preamble string, to avoid recomputation/disk I/O on live updates.")

(defun live-tex-preview--reset-caches ()
  "Drop the cached main file and preamble so they are recomputed."
  (setq live-tex-preview--main-file-cache 'unset
        live-tex-preview--preamble-cache nil))

(defun live-tex-preview--compute-main-file ()
  "Resolve the document's main .tex file for the current buffer.
Returns an absolute path, or nil.  Tries, in order: this buffer if it has a
\\begin{document}; a `% !TEX root = ...' magic comment; AUCTeX's `TeX-master'
file-local variable; AUCTeX's `TeX-master-file'; finally this file itself."
  (let ((dir (file-name-directory (or buffer-file-name default-directory)))
        (master (bound-and-true-p TeX-master)))
    (cond
     ((live-tex-preview--document-body-bounds)
      (and buffer-file-name (expand-file-name buffer-file-name)))
     ((save-excursion
        (goto-char (point-min))
        (re-search-forward "^%+ *!TE[Xx] root *= *\\(.+?\\) *$" (min 1024 (point-max)) t))
      (expand-file-name (string-trim (match-string 1)) dir))
     ((stringp master)
      (expand-file-name (if (string-suffix-p ".tex" master)
                            master (concat master ".tex"))
                        dir))
     ;; AUCTeX can be loaded globally and `TeX-master-file' will then invent a
     ;; "<buffer-name>.tex" master even for non-TeX buffers (for example,
     ;; "notes.org.tex").  Only consult it in an actual TeX-derived mode.
     ((and (derived-mode-p 'tex-mode)
           (fboundp 'TeX-master-file))
      (ignore-errors
        (let ((m (TeX-master-file "tex")))
          (and (stringp m) (expand-file-name m dir)))))
     (t (and buffer-file-name (expand-file-name buffer-file-name))))))

(defun live-tex-preview--main-file ()
  "Like `live-tex-preview--compute-main-file', cached per buffer."
  (if (not (eq live-tex-preview--main-file-cache 'unset))
      live-tex-preview--main-file-cache
    (setq live-tex-preview--main-file-cache
          (live-tex-preview--compute-main-file))))

(defun live-tex-preview--main-dir ()
  "Directory of the main .tex file (the project root for previews)."
  (let ((main (live-tex-preview--main-file)))
    (file-name-as-directory
     (or (and main (file-name-directory main))
         (and buffer-file-name (file-name-directory buffer-file-name))
         default-directory))))

(defun live-tex-preview--preamble-in-buffer ()
  "Return the preamble (before \\begin{document}) of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (live-tex-preview--sanitize-preamble
     (if (live-tex-preview--find-delimiter "\\begin{document}")
         (buffer-substring-no-properties (point-min) (- (point) (length "\\begin{document}")))
       live-tex-preview-default-preamble))))

(defun live-tex-preview--sanitize-preamble (preamble)
  "Return PREAMBLE without directives unsafe in generated preview files.
Org-exported LaTeX files can begin with a TeX format directive like
`%& /path/to/precompiled-format'.  If copied into the generated preview file,
that directive makes TeX load the document's precompiled format before the Org
preview engine appends preview.sty, causing page-sized previews and missing
height/depth metadata.

Org-exported files may also contain a mylatexformat dump marker:
`\\ifcsname endofdump\\endcsname\\endofdump\\fi'.  That marker is correct for
the exported document, but if Org's preview precompiler sees it before
`preview.sty' has been appended, the generated format omits the preview
environment.  Strip it from preview preambles too."
  (thread-last preamble
	       (replace-regexp-in-string "^[ \t]*%&[^\n]*\\(?:\n\\|\\'\\)" "")
	       (replace-regexp-in-string
		"^[ \t]*%+[ \t]*end precompiled preamble[ \t]*\n" "")
	       (replace-regexp-in-string
		"\\\\ifcsname[ \t]+endofdump\\\\endcsname\\\\endofdump\\\\fi" "")
	       (replace-regexp-in-string "\\\\endofdump\\>" "")))

(defun live-tex-preview--preamble ()
  "Return the LaTeX preamble for previews (cached per buffer).
For a child file in a multi-file project the preamble is read from the main
file (see `live-tex-preview--main-file'); otherwise from this buffer.
`live-tex-preview-extra-preamble' is appended."
  (or live-tex-preview--preamble-cache
      (setq live-tex-preview--preamble-cache
            (let ((main (live-tex-preview--main-file)))
              (concat
               (if (and main buffer-file-name
                        (not (file-equal-p main buffer-file-name)))
                   (with-temp-buffer
                     (insert-file-contents main)
                     (live-tex-preview--preamble-in-buffer))
                 (live-tex-preview--preamble-in-buffer))
               "\n"
               live-tex-preview-extra-preamble)))))

;;; Commands

(defun live-tex-preview--place (entries)
  "Render ENTRIES with this TeX document's preamble and cache."
  (live-tex-preview-place
   entries :preamble (live-tex-preview--preamble)
   :page-width live-tex-preview-page-width
   :input-directory (live-tex-preview--main-dir)
   :cache-directory (and live-tex-preview-cache-directory
                         (expand-file-name live-tex-preview-cache-directory
                                           (live-tex-preview--main-dir)))))

(provide 'live-tex-preview-tex)
;;; live-tex-preview-tex.el ends here
