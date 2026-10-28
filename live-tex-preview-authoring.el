;;; live-tex-preview-authoring.el --- Document images and navigation -*- lexical-binding: t; -*-
;; Generated from live-tex-preview.org. Edit the literate source, then tangle it.
;; Copyright (C) 2026 Jorge Noreña
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Optional TeX/Markdown/Quarto image previews, citations, references and
;; explicit figure producers.  Equation rendering belongs to live-tex-preview.

;;; Code:

(require 'live-tex-preview)
(require 'cl-lib)
(require 'browse-url)
(require 'subr-x)
(require 'url-util)
(require 'compile)
(require 'project)
(require 'xref)
(require 'thingatpt)

(defgroup live-tex-preview-authoring nil
  "TeX, Markdown and Quarto images, navigation and figure producers."
  :group 'tex
  :prefix "live-tex-preview-authoring-")

(defcustom live-tex-preview-authoring-figure-source-runners
  '(("py"      . "python3")
    ("jl"      . "julia")
    ("R"       . "Rscript")
    ("r"       . "Rscript")
    ("gp"      . "gnuplot")
    ("gnuplot" . "gnuplot")
    ("plt"     . "gnuplot")
    ("sh"      . "bash")
    ("wl"      . "wolframscript -file")
    ("m"       . "octave"))
  "Alist mapping a producer file's extension to the command that runs it.
Used for TeX `% !src = file.ext' and Markdown HTML producer comments."
  :type '(alist :key-type string :value-type string)
  :group 'live-tex-preview-authoring)

(defcustom live-tex-preview-authoring-graphics-extensions
  '("" ".pdf" ".png" ".jpg" ".jpeg" ".eps" ".svg" ".gif" ".tikz")
  "Extensions tried, in order, when resolving an \\includegraphics path."
  :type '(repeat string)
  :group 'live-tex-preview-authoring)

(defcustom live-tex-preview-authoring-image-directly-displayable
  '("png" "jpg" "jpeg" "gif" "svg" "tiff" "tif" "webp" "bmp")
  "Image extensions Emacs can display inline without conversion."
  :type '(repeat string)
  :group 'live-tex-preview-authoring)

(defcustom live-tex-preview-authoring-pdf-preview-dpi 150
  "Resolution used when rasterising PDF/EPS figures for inline preview."
  :type 'integer
  :group 'live-tex-preview-authoring)

(defcustom live-tex-preview-authoring-image-max-width-fraction 0.8
  "Inline image previews are scaled to at most this fraction of the window width."
  :type 'number
  :group 'live-tex-preview-authoring)

(defvar TeX-master)

;;; Document buffer ownership

(defmacro live-tex-preview-authoring--with-document (&rest body)
  "Run BODY at the caller's position in its Markdown host, if any."
  (declare (indent 0) (debug t))
  `(let* ((base (buffer-base-buffer))
          (host (live-tex-preview-document-buffer))
          (position (point)))
     (with-current-buffer host
       (save-restriction
         (when (or base (bound-and-true-p polymode-mode)) (widen))
         (goto-char (min (point-max) (max (point-min) position)))
         ,@body))))

(with-eval-after-load 'polymode
  (add-to-list 'polymode-ignore-overlays-with-these-properties 'live-tex-preview-authoring-image))

;;; Document-relative path resolution

(defun live-tex-preview-authoring--main-dir ()
  "Return the shared frontend's document directory."
  (live-tex-preview-document-directory))

(defun live-tex-preview-authoring--graphicspath-dirs (base)
  "Return absolute dirs declared by \\graphicspath, resolved against BASE."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\\\\graphicspath{\\(\\(?:{[^}]*}\\)+\\)}" nil t)
      (let ((s (match-string 1)) (start 0) dirs)
        (while (string-match "{\\([^}]*\\)}" s start)
          (push (expand-file-name (match-string 1 s) base) dirs)
          (setq start (match-end 0)))
        (nreverse dirs)))))

(defun live-tex-preview-authoring--find-existing (raw exts dirs)
  "Return the first existing DIRS/RAW+EXT combination, or nil."
  (cl-some (lambda (d)
             (cl-some (lambda (e)
                        (let ((f (expand-file-name (concat raw e) d)))
                          (and (file-exists-p f) f)))
                      exts))
           dirs))

;;; TeX file and URL targets

(defconst live-tex-preview-authoring--file-macro-re
  (concat "\\\\\\(input\\|include\\|subfile\\|includegraphics\\|includesvg"
          "\\|addbibresource\\|bibliography\\)\\*?"
          "[ \t]*\\(?:\\[[^]]*\\]\\)?[ \t]*{\\([^}]*\\)}")
  "Match a file-referencing LaTeX macro; group 1 = name, group 2 = argument.")

(defconst live-tex-preview-authoring--url-macro-re
  "\\\\url\\*?[ \t]*{\\([^}\n]+\\)}"
  "Match a LaTeX \\url macro; group 1 is the target.")

(defun live-tex-preview-authoring--macro-at-point ()
  "Return (NAME . ARG) for a file-referencing macro under point, or nil."
  (save-excursion
    (let ((p (point)) (eol (line-end-position)) result)
      (goto-char (line-beginning-position))
      (while (and (not result)
                  (re-search-forward live-tex-preview-authoring--file-macro-re eol t))
        (when (and (<= (match-beginning 0) p) (<= p (match-end 0)))
          (setq result (cons (match-string 1) (match-string 2)))))
      result)))

(defun live-tex-preview-authoring--url-at-point ()
  "Return the target of a \\url macro under point, or nil."
  (save-excursion
    (let ((p (point)) (eol (line-end-position)) result)
      (goto-char (line-beginning-position))
      (while (and (not result)
                  (re-search-forward live-tex-preview-authoring--url-macro-re eol t))
        (when (and (<= (match-beginning 0) p) (<= p (match-end 0)))
          (setq result (match-string 1))))
      result)))

(defun live-tex-preview-authoring--clean-url-target (target)
  "Return TARGET with common LaTeX escapes removed."
  (replace-regexp-in-string
   "\\\\\\([#$%&_{}]\\)" "\\1" (string-trim target) t))

(defun live-tex-preview-authoring--file-url-to-path (target)
  "Return file path for file: TARGET, or nil if TARGET is not a file URL."
  (when (string-match-p "\\`file:" target)
    (url-unhex-string
     (replace-regexp-in-string "\\`file:\\(?://\\)?" "" target))))

(defun live-tex-preview-authoring--local-url-path (target)
  "Resolve a local TARGET against the document, decoding URL paths in Markdown."
  (let* ((file-url (live-tex-preview-authoring--file-url-to-path target))
         (raw (or file-url target)))
    (unless (and (not file-url) (live-tex-preview-authoring--external-url-p target))
      (when (derived-mode-p 'markdown-mode)
        (setq raw (url-unhex-string (car (split-string raw "[?#]")))))
      (when (and raw (not (string-empty-p raw)))
        (let ((path (expand-file-name raw (live-tex-preview-authoring--main-dir))))
          (and (file-regular-p path) path))))))

(defun live-tex-preview-authoring--external-url-p (target)
  "Return non-nil when TARGET should be opened as an external URL."
  (or (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" target)
      (string-match-p "\\`www\\." target)))

(defun live-tex-preview-authoring--resolve-file (name arg)
  "Resolve the file referenced by macro NAME with argument ARG to a path."
  (let ((base (live-tex-preview-authoring--main-dir))
        (raw (string-trim arg)))
    (pcase name
      ((or "input" "include" "subfile")
       (or (live-tex-preview-authoring--find-existing raw '("" ".tex") (list base))
           (expand-file-name (if (file-name-extension raw) raw (concat raw ".tex")) base)))
      ((or "addbibresource" "bibliography")
       (or (live-tex-preview-authoring--find-existing raw '("" ".bib") (list base))
           (expand-file-name (if (file-name-extension raw) raw (concat raw ".bib")) base)))
      ("includesvg"
       (or (live-tex-preview-authoring--find-existing raw '("" ".svg") (list base))
           (expand-file-name (concat raw ".svg") base)))
      ("includegraphics"
       (or (live-tex-preview-authoring--find-existing
            raw live-tex-preview-authoring-graphics-extensions
            (cons base (live-tex-preview-authoring--graphicspath-dirs base)))
           (expand-file-name raw base))))))

;;;###autoload
(defun live-tex-preview-authoring-follow-at-point ()
  "Open the file or URL referenced at point in TeX or Markdown prose."
  (interactive)
  (live-tex-preview-authoring--with-document
    (if (derived-mode-p 'markdown-mode)
        (let* ((link (live-tex-preview-authoring--markdown-link-at-point))
               (target (and link (markdown-link-url)))
               (file (and target (live-tex-preview-authoring--local-url-path target))))
          (cond (file (find-file file))
                ((and target (live-tex-preview-authoring--external-url-p target)) (browse-url target))
                (t (user-error "Markdown link target not found: %s" (or target "none")))))
      (if-let* ((url (live-tex-preview-authoring--url-at-point)))
          (live-tex-preview-authoring-follow-url url)
        (let ((macro (live-tex-preview-authoring--macro-at-point)))
          (unless macro (user-error "No file or URL macro at point"))
          (let ((file (live-tex-preview-authoring--resolve-file (car macro) (cdr macro))))
            (if (and file (file-exists-p file)) (find-file file)
              (user-error "Target not found: %s" (or file (cdr macro))))))))))

;;;###autoload
(defun live-tex-preview-authoring-follow-url (&optional target)
  "Open TARGET from a \\url macro.
Local file paths, including file: URLs, open with `find-file'.  External URLs
open with `browse-url'."
  (interactive)
  (let ((target (live-tex-preview-authoring--clean-url-target
                 (or target
                     (live-tex-preview-authoring--url-at-point)
                     (user-error "No \\url macro at point")))))
    (cond
     ((live-tex-preview-authoring--local-url-path target)
      (find-file (live-tex-preview-authoring--local-url-path target)))
     ((string-match-p "\\`file:" target)
      (user-error "Target not found: %s" target))
     ((live-tex-preview-authoring--external-url-p target)
      (browse-url target))
     (t
      (user-error "Target not found: %s" target)))))

;;; Markdown targets and prose boundaries

(declare-function markdown-link-at-pos "markdown-mode")
(declare-function markdown-link-url "markdown-mode")
(declare-function markdown-code-block-at-pos "markdown-mode")
(declare-function markdown-inline-code-at-pos-p "markdown-mode")
(declare-function pm-innermost-span "polymode")

(defun live-tex-preview-authoring--markdown-prose-p (pos &optional comment math)
  "Use the shared Markdown prose predicate at POS with COMMENT and MATH."
  (require 'live-tex-preview-markdown)
  (live-tex-preview-markdown-prose-p pos comment math))

(defun live-tex-preview-authoring--markdown-link-at-point ()
  "Return Markdown link properties at point, only in document prose."
  (when (derived-mode-p 'markdown-mode)
    (save-match-data
      (let* ((pos (point))
             (link (markdown-link-at-pos pos))
             (beg (nth 0 link)) (end (nth 1 link)))
        (when (and beg end (<= beg pos) (<= pos end)
                   (not (save-excursion
                          (goto-char beg)
                          (= 1 (% (- beg (progn (skip-chars-backward "\\\\") (point))) 2))))
                   (live-tex-preview-authoring--markdown-prose-p beg)
                   (live-tex-preview-authoring--markdown-prose-p (1- end)))
          link)))))

(defun live-tex-preview-authoring--markdown-image-at-point ()
  "Return (BEG END FILE) for a local Markdown inline or reference image."
  (when-let* ((link (live-tex-preview-authoring--markdown-link-at-point))
              (_ (nth 6 link)))
    (let* ((target (save-excursion
                     (goto-char (car link))
                     (markdown-link-url)))
           (file (and target (live-tex-preview-authoring--local-url-path target))))
      (list (nth 0 link) (nth 1 link) file))))

(defun live-tex-preview-authoring--image-at-point ()
  "Return (BEG END FILE) for this document's image syntax at point."
  (if (derived-mode-p 'markdown-mode)
      (live-tex-preview-authoring--markdown-image-at-point)
    (live-tex-preview-authoring--includegraphics-at-point)))

;;; Producer comment lookup

(defconst live-tex-preview-authoring--markdown-producer-re
  "<!--[ \t\r\n]*!\\(src\\|run\\)[ \t]*=[ \t]*\\([^\n]+?\\)[ \t\r\n]*-->"
  "An HTML producer comment, with kind in group 1 and value in group 2.")

(defun live-tex-preview-authoring--markdown-producer-at-point ()
  "Return (BEG END KIND VALUE) for a producer HTML comment at point."
  (save-excursion
    (let ((pos (point)) result)
      (beginning-of-line)
      (when (or (looking-at-p "[ \t]*<!--")
                (search-backward "<!--" nil t))
        (when (looking-at "[ \t]*\\(?:<!--\\)")
          (skip-chars-forward " \t"))
        (when (and (looking-at live-tex-preview-authoring--markdown-producer-re)
                   (<= (match-beginning 0) pos) (<= pos (match-end 0)))
          (let ((beg (match-beginning 0)) (end (match-end 0))
                (kind (intern (match-string-no-properties 1)))
                (value (string-trim (match-string-no-properties 2))))
            (when (and (not (string-empty-p value))
                       (live-tex-preview-authoring--markdown-prose-p beg t))
              (setq result (list beg end kind value))))))
      result)))

(defun live-tex-preview-authoring--markdown-figure-source ()
  "Return (KIND . VALUE) at a producer comment or its following image.
Only whitespace may separate the comment and image.  Looking up a figure
never searches across prose, another image, or a fenced code block."
  (let ((comment (live-tex-preview-authoring--markdown-producer-at-point)))
    (unless comment
      (when-let* ((image (live-tex-preview-authoring--markdown-image-at-point)))
        (setq comment
              (save-excursion
                (goto-char (car image))
                (skip-chars-backward " \t\r\n")
                (when (and (>= (- (point) (point-min)) 3)
                           (equal (buffer-substring-no-properties (- (point) 3) (point)) "-->"))
                  (live-tex-preview-authoring--markdown-producer-at-point))))))
    (when comment (cons (nth 2 comment) (nth 3 comment)))))

;;; TeX figure producer lookup

(defun live-tex-preview-authoring--enclosing-figure ()
  "Return (BEG . END) of the `figure' environment surrounding point, or nil."
  (save-excursion
    (let ((p (point)) beg)
      (when (re-search-backward "\\\\begin{figure\\*?}" nil t)
        (setq beg (match-beginning 0))
        (when (re-search-forward "\\\\end{figure\\*?}" nil t)
          (and (<= beg p) (<= p (match-end 0))
               (cons beg (match-end 0))))))))

(defun live-tex-preview-authoring--source-comment-at-point ()
  "Return non-nil on a TeX or Markdown figure producer comment."
  (if (derived-mode-p 'markdown-mode)
      (live-tex-preview-authoring--markdown-producer-at-point)
    (save-excursion
      (beginning-of-line)
      (looking-at-p "[ \t]*%+[ \t]*!\\(?:src\\|run\\)[ \t]*="))))

(defun live-tex-preview-authoring--figure-source ()
  "Return (KIND . VALUE) from the enclosing figure's producer comment, or nil.
KIND is `src' (run VALUE by extension) or `run' (run VALUE verbatim)."
  (when-let* ((bounds (live-tex-preview-authoring--enclosing-figure)))
    (save-excursion
      (goto-char (car bounds))
      (when (re-search-forward
             "%+[ \t]*!\\(src\\|run\\)[ \t]*=[ \t]*\\(.+?\\)[ \t]*$"
             (cdr bounds) t)
        (cons (intern (match-string 1)) (string-trim (match-string 2)))))))

(defun live-tex-preview-authoring--figure-source-command ()
  "Return (COMMAND . DIR) for this document's figure producer."
  (let ((src (if (derived-mode-p 'markdown-mode)
                 (live-tex-preview-authoring--markdown-figure-source)
               (live-tex-preview-authoring--figure-source)))
        (dir (live-tex-preview-authoring--main-dir)))
    (unless src (user-error "No !src or !run producer comment for this figure"))
    (pcase-let ((`(,kind . ,value) src))
      (if (eq kind 'run)
          (cons value dir)
        (let* ((file (expand-file-name value dir))
               (extension (file-name-extension file))
               (runner (cdr (assoc extension live-tex-preview-authoring-figure-source-runners))))
          (unless (file-readable-p file) (user-error "Producer not found: %s" file))
          (unless runner (user-error "No figure runner for .%s" (or extension "?")))
          (cons (format "%s %s" runner (shell-quote-argument file)) dir))))))

;;;###autoload
(defun live-tex-preview-authoring-run-figure-source ()
  "Run the explicit !src or !run producer for the figure at point.
TeX uses a comment inside its figure environment.  Markdown and Quarto use
an HTML comment immediately before the image, or the comment at point."
  (interactive)
  (live-tex-preview-authoring--with-document
    (pcase-let ((`(,command . ,directory) (live-tex-preview-authoring--figure-source-command)))
      (let ((default-directory directory)) (compile command)))))

;;; Image conversion and cache

(defun live-tex-preview-authoring--image-cache-dir ()
  "Return (creating) the dir for rasterised figure previews."
  (let ((dir (expand-file-name ".cache/live-tex-preview-authoring-images/"
                               (live-tex-preview-authoring--main-dir))))
    (unless (file-directory-p dir) (make-directory dir t))
    dir))

(defun live-tex-preview-authoring--convert-to-png (file)
  "Rasterise a PDF/EPS FILE to a cached PNG, returning its path or nil.
Uses pdftoppm, ImageMagick, or Ghostscript, whichever is available."
  (let* ((dir (live-tex-preview-authoring--image-cache-dir))
         (mtime (float-time (file-attribute-modification-time
                             (file-attributes file))))
         (png (expand-file-name (concat (md5 (format "%s@%s@%s" file mtime live-tex-preview-authoring-pdf-preview-dpi)) ".png")
                                dir))
         (dpi (number-to-string live-tex-preview-authoring-pdf-preview-dpi)))
    (if (file-exists-p png)
        png
      (let ((rc (cond
                 ((and (equal (downcase (or (file-name-extension file) "")) "pdf")
                       (executable-find "pdftoppm"))
                  (call-process "pdftoppm" nil nil nil "-png" "-r" dpi
                                "-f" "1" "-l" "1" "-singlefile" file (file-name-sans-extension png)))
                 ((executable-find "magick")
                  (call-process "magick" nil nil nil "-density" dpi (concat file "[0]") png))
                 ((executable-find "convert")
                  (call-process "convert" nil nil nil "-density" dpi (concat file "[0]") png))
                 ((executable-find "gs")
                  (call-process "gs" nil nil nil "-q" "-dNOPAUSE" "-dBATCH"
                                "-dFirstPage=1" "-dLastPage=1" "-sDEVICE=png16m"
                                (concat "-r" dpi) (concat "-sOutputFile=" png) file))
                 (t (user-error "No PDF→PNG converter (install poppler, imagemagick, or ghostscript)")))))
        (and (integerp rc) (= rc 0) (file-exists-p png) png)))))

(defun live-tex-preview-authoring--displayable-image (file)
  "Return a displayable path for FILE, rasterising PDF/EPS if needed, else nil."
  (when (and file (file-exists-p file))
    (let ((ext (downcase (or (file-name-extension file) ""))))
      (cond
       ((member ext live-tex-preview-authoring-image-directly-displayable) file)
       ((member ext '("pdf" "eps" "ps")) (live-tex-preview-authoring--convert-to-png file))
       (t nil)))))

(defun live-tex-preview-authoring--includegraphics-at-point ()
  "Return (BEG END FILE) for the \\includegraphics macro at point, or nil.
FILE is the resolved image path."
  (save-excursion
    (let ((p (point)) (eol (line-end-position)) result)
      (goto-char (line-beginning-position))
      (while (and (not result)
                  (re-search-forward
                   "\\\\includegraphics\\*?[ \t]*\\(?:\\[[^]]*\\]\\)?[ \t]*{\\([^}]*\\)}"
                   eol t))
        (when (and (<= (match-beginning 0) p) (<= p (match-end 0)))
          (setq result (list (match-beginning 0) (match-end 0)
                             (live-tex-preview-authoring--resolve-file
                              "includegraphics" (match-string 1))))))
      result)))

(defun live-tex-preview-authoring--image-overlay-at (beg end)
  "Return an existing image-preview overlay covering BEG..END, or nil."
  (cl-find-if (lambda (o) (overlay-get o 'live-tex-preview-authoring-image))
              (overlays-in beg end)))

(defun live-tex-preview-authoring--place-image-overlay (beg end file)
  "Overlay BEG..END with an inline preview of local image FILE."
  (let ((displayable (live-tex-preview-authoring--displayable-image file)))
    (unless displayable (user-error "Cannot preview local image: %s" (or file "missing/remote")))
    (let* ((window (get-buffer-window (current-buffer) t))
           (maxpx (and window (floor (* live-tex-preview-authoring-image-max-width-fraction
                                        (window-body-width window t)))))
           (image (apply #'create-image displayable nil nil
                         (and maxpx (list :max-width maxpx))))
           (overlay (make-overlay beg end)))
      (unless image (delete-overlay overlay) (user-error "Image type unavailable: %s" displayable))
      (when (display-graphic-p) (image-flush image t))
      (overlay-put overlay 'live-tex-preview-authoring-image t)
      (overlay-put overlay 'display image)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'help-echo file)
      (overlay-put overlay 'modification-hooks
                   (list (lambda (ov after &rest _) (unless after (delete-overlay ov)))))
      overlay)))

;;; Image preview commands

;;;###autoload
(defun live-tex-preview-authoring-preview-image-at-point ()
  "Toggle an inline image at point in TeX, Markdown or Quarto prose."
  (interactive)
  (live-tex-preview-authoring--with-document
    (let ((info (live-tex-preview-authoring--image-at-point)))
      (unless info (user-error "No image at point"))
      (pcase-let ((`(,beg ,end ,file) info))
        (if-let* ((existing (live-tex-preview-authoring--image-overlay-at beg end)))
            (delete-overlay existing)
          (live-tex-preview-authoring--place-image-overlay beg end file))))))

;;;###autoload
(defun live-tex-preview-authoring-preview-images-buffer ()
  "Refresh inline previews of local images throughout the document.
Markdown inline/reference images are recognized outside code and metadata.
Missing, remote or unsupported images are counted and left as source."
  (interactive)
  (live-tex-preview-authoring--with-document
    (save-excursion
      (live-tex-preview-authoring-clear-image-previews)
      (goto-char (point-min))
      (let ((regexp (if (derived-mode-p 'markdown-mode) "!\\[" "\\\\includegraphics"))
            (count 0) (skipped 0))
        (while (re-search-forward regexp nil t)
          (let* ((start (match-beginning 0))
                 (next (point))
                 (info (save-excursion (goto-char start) (live-tex-preview-authoring--image-at-point))))
            (when info
              (pcase-let ((`(,beg ,end ,file) info))
                (condition-case nil
                    (progn (live-tex-preview-authoring--place-image-overlay beg end file) (cl-incf count))
                  (error (cl-incf skipped)))
                (setq next end)))
            (goto-char (max (1+ start) next))))
        (message "Authoring: previewed %d image(s), skipped %d" count skipped)
        count))))

;;;###autoload
(defun live-tex-preview-authoring-clear-image-previews ()
  "Remove this helper's inline image previews from the document host."
  (interactive)
  (live-tex-preview-authoring--with-document
    (remove-overlays (point-min) (point-max) 'live-tex-preview-authoring-image t)))

;;; Citations and equation references

(declare-function reftex-view-crossref "reftex-dcr")
(declare-function citar-markdown-key-at-point "citar-markdown")
(declare-function citar-markdown-citation-at-point "citar-markdown")
(declare-function citar-get-entry "citar")
(declare-function citar-open-entry "citar")
(defvar citar-bibliography)

(defconst live-tex-preview-authoring--crossref-macro-re
  (concat "\\\\\\(" (regexp-opt '("ref" "eqref" "autoref" "pageref" "nameref"
                                      "cref" "Cref" "vref" "Vref" "labelcref"))
          "\\|[[:alpha:]]*cite[[:alpha:]]*\\)\\*?"
          "[ \t\n]*\\(?:\\[[^]]*\\][ \t\n]*\\)*{\\([^}]+\\)}")
  "TeX reference or citation macro; group 2 contains the key argument.")

(defun live-tex-preview-authoring--tex-crossref-at-point ()
  "Return a position inside a reference/citation key under point, or nil."
  (save-excursion
    (let ((pos (point)) result)
      (goto-char (point-min))
      (while (and (not result)
                  (re-search-forward live-tex-preview-authoring--crossref-macro-re nil t))
        (when (and (<= (match-beginning 0) pos) (< pos (match-end 0))
                   (not (save-excursion
                          (save-match-data
                            (nth 4 (syntax-ppss (match-beginning 0)))))))
          (setq result (if (and (<= (match-beginning 2) pos) (< pos (match-end 2)))
                           pos (match-beginning 2)))))
      result)))

(defun live-tex-preview-authoring--markdown-keys-at-point ()
  "Return Pandoc keys at point, preferring an individual key to its group.
Quarto cross-references work without Citar.  Citation following imports
Citar lazily and reports an actionable error if the optional backend is absent."
  (when (and (live-tex-preview-authoring--markdown-prose-p (point))
             (not (live-tex-preview-authoring--markdown-link-at-point)))
    (save-match-data
      (let* ((simple (and (thing-at-point-looking-at
                          "-?@\\([[:alnum:]_]+\\(?:[_.:/-][[:alnum:]_]+\\)*\\)")
                         (cons (match-string-no-properties 1) (match-beginning 0))))
             (start (cdr simple))
             (valid (and simple
                         (not (memq (char-before start) '(?\\ ?_)))
                         (not (and (char-before start)
                                   (eq (char-syntax (char-before start)) ?w))))))
        (cond
         ((and valid (string-match-p
                      "\\`\\(?:eq\\|fig\\|tbl\\|sec\\|lst\\|thm\\|lem\\|cor\\|def\\|exm\\|exr\\)-"
                      (car simple)))
          (list (car simple)))
         ((require 'citar-markdown nil t)
          (let* ((key (citar-markdown-key-at-point))
                 (key-start (and key (cadr key))))
            (cond
             ((and key (not (memq (char-before key-start) '(?\\ ?_)))
                   (not (and (char-before key-start)
                             (eq (char-syntax (char-before key-start)) ?w))))
              (list (car key)))
             ((not key) (car (citar-markdown-citation-at-point))))))
         (valid (user-error "Install Citar to follow citation %s" (car simple))))))))

(defun live-tex-preview-authoring--markdown-label-position (key)
  "Find a Pandoc/Quarto label KEY in this Markdown document."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat "{#" (regexp-quote key) "\\(?:[ \t}]\\)"
                          "\\|\\\\label{" (regexp-quote key) "}"
                          "\\|^[ \t]*#|[ \t]*label:[ \t]*[\"']?"
                          (regexp-quote key) "[\"']?[ \t]*$"))
          found)
      (while (and (not found) (re-search-forward regexp nil t))
        (let ((beg (match-beginning 0)))
          ;; A Quarto cell label is intentionally inside a fenced code block.
          (when (or (save-excursion (goto-char beg) (looking-at-p "[ \t]*#|"))
                    (live-tex-preview-authoring--markdown-prose-p beg nil t))
            (setq found beg))))
      found)))

(defun live-tex-preview-authoring--markdown-reference-keys ()
  "Recognize Pandoc @keys and raw TeX reference macros in Markdown prose."
  (or (live-tex-preview-authoring--markdown-keys-at-point)
      (when (live-tex-preview-authoring--markdown-prose-p (point))
        (when-let* ((pos (live-tex-preview-authoring--tex-crossref-at-point)))
          (save-excursion
            (goto-char pos)
            (skip-chars-backward "^,{} \t\n")
            (when (looking-at "[^},[:space:]]+")
              (list (match-string-no-properties 0))))))))

(defun live-tex-preview-authoring--markdown-reference-target (key)
  "Return (BUFFER . POSITION) for label KEY, including project documents."
  (let ((current (current-buffer))
        (dir (live-tex-preview-authoring--main-dir)))
    (or (when-let* ((pos (live-tex-preview-authoring--markdown-label-position key)))
          (cons current pos))
        (let* ((project (and (string-match-p "\\`\\(?:eq\\|fig\\|tbl\\|sec\\|lst\\|thm\\|lem\\|cor\\|def\\|exm\\|exr\\)-" key)
                             (project-current nil dir)))
               (files (and project (condition-case nil (project-files project) (error nil)))))
          (catch 'found
            (dolist (file files)
              (when (and (string-match-p "\\.\\(?:qmd\\|md\\)\\'" file)
                         (not (equal file buffer-file-name)))
                (let ((buffer (find-buffer-visiting file)))
                  (with-current-buffer (or buffer (generate-new-buffer " *authoring reference*"))
                    (unwind-protect
                        (progn
                          (unless buffer
                            (insert-file-contents file)
                            (let ((markdown-mode-hook nil)) (markdown-mode)))
                          (when-let* ((pos (live-tex-preview-authoring--markdown-label-position key)))
                            (throw 'found (cons (or buffer (find-file-noselect file)) pos))))
                      (unless buffer (kill-buffer (current-buffer)))))))))))))

(defun live-tex-preview-authoring--yaml-bibliography ()
  "Read simple top-level YAML bibliography scalars or lists in this buffer.
Supports document front matter and _quarto.yml.  Complex YAML can instead
be expressed with buffer-local `citar-bibliography'."
  (save-excursion
    (goto-char (point-min))
    (let ((limit (if (looking-at-p "---[ \t]*$")
                     (save-excursion
                       (forward-line)
                       (if (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" nil t)
                           (match-beginning 0) (point-min)))
                   (if (derived-mode-p 'markdown-mode) (point-min) (point-max))))
          values)
      (when (re-search-forward "^bibliography:[ \t]*\\(.*\\)$" limit t)
        (let ((value (string-trim (match-string-no-properties 1))))
          (if (string-empty-p value)
              (progn
                (forward-line)
                (while (looking-at "[ \t]+-[ \t]+\\(.+\\)$")
                  (push (match-string-no-properties 1) values)
                  (forward-line))
                (setq values (nreverse values)))
            (setq values (if (string-prefix-p "[" value)
                             (split-string (string-trim value "\\[" "\\]") "," t)
                           (list value))))))
      (mapcar (lambda (value)
                (string-trim (string-trim value) "[\"']" "[\"']"))
              values))))

(defun live-tex-preview-authoring--markdown-bibliography ()
  "Return local YAML, Quarto project and Citar bibliography file paths."
  (let* ((dir (live-tex-preview-authoring--main-dir))
         (local (mapcar (lambda (file) (expand-file-name file dir))
                        (live-tex-preview-authoring--yaml-bibliography)))
         (root (locate-dominating-file dir "_quarto.yml"))
         (global (bound-and-true-p citar-bibliography)))
    (when root
      (setq local
            (append local
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name "_quarto.yml" root))
                      (mapcar (lambda (file) (expand-file-name file root))
                              (live-tex-preview-authoring--yaml-bibliography))))))
    (delete-dups (append local (if (stringp global) (list global) global)))))

(defun live-tex-preview-authoring--follow-markdown-reference (keys)
  "Follow a label or bibliography entry named in KEYS."
  (let* ((key (if (cdr keys) (completing-read "Reference: " keys nil t) (car keys)))
         (target (live-tex-preview-authoring--markdown-reference-target key)))
    (cond
     (target
      (xref-push-marker-stack)
      (pop-to-buffer (car target))
      (widen)
      (goto-char (cdr target)))
     ((string-prefix-p "eq-" key)
      (user-error "Equation label not found: %s" key))
     (t
      (unless (require 'citar nil t)
        (user-error "Install Citar to follow bibliography citations"))
      (let ((citar-bibliography (live-tex-preview-authoring--markdown-bibliography))
            (default-directory (live-tex-preview-authoring--main-dir)))
        (unless (citar-get-entry key)
          (user-error "Citation not found in document/project/Citar bibliographies: %s" key))
        (xref-push-marker-stack)
        (citar-open-entry key))))))

(defun live-tex-preview-authoring-follow-reference-at-point ()
  "Follow a citation or equation reference, returning non-nil if recognized."
  (interactive)
  (live-tex-preview-authoring--with-document
    (if (derived-mode-p 'markdown-mode)
        (when-let* ((keys (live-tex-preview-authoring--markdown-reference-keys)))
          (live-tex-preview-authoring--follow-markdown-reference keys)
          t)
      (when-let* ((pos (live-tex-preview-authoring--tex-crossref-at-point)))
        (goto-char pos)
        (reftex-view-crossref 2)
        t))))

;;; DWIM dispatcher

;;;###autoload
(defun live-tex-preview-authoring-dwim-at-point (&optional no-error)
  "Preview, follow a reference/link, or run an explicit producer at point.
Return non-nil after acting.  NO-ERROR allows the caller's normal RET when
nothing applies; recognized targets with real failures still signal errors."
  (interactive)
  (live-tex-preview-authoring--with-document
    (cond
     ((live-tex-preview-authoring--image-at-point) (live-tex-preview-authoring-preview-image-at-point) t)
     ((live-tex-preview-authoring-follow-reference-at-point) t)
     ((if (derived-mode-p 'markdown-mode)
          (live-tex-preview-authoring--markdown-link-at-point)
        (or (live-tex-preview-authoring--url-at-point) (live-tex-preview-authoring--macro-at-point)))
      (live-tex-preview-authoring-follow-at-point) t)
     ((live-tex-preview-authoring--source-comment-at-point) (live-tex-preview-authoring-run-figure-source) t)
     (no-error nil)
     (t (user-error "Nothing to preview, follow, or run at point")))))

(provide 'live-tex-preview-authoring)
;;; live-tex-preview-authoring.el ends here
