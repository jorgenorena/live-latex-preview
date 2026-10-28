;;; authoring-test.el --- Document authoring regressions -*- lexical-binding: t; -*-
;; Generated from live-tex-preview.org. Edit the literate source.
;; Copyright (C) 2026 Jorge Noreña
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'live-tex-preview-authoring)
(require 'markdown-mode nil t)
(require 'quarto-mode nil t)
(require 'tex-site nil t)
(require 'latex nil t)
(require 'reftex)
(require 'citar nil t)
(when (featurep 'markdown-mode) (require 'live-tex-preview-markdown))

(defmacro authoring-test--document (mode text &rest body)
  (declare (indent 2))
  `(let* ((directory (make-temp-file "live-tex-authoring-" t))
          (default-directory (file-name-as-directory directory))
          (project-find-functions nil)
          (filename (expand-file-name (if (eq ,mode 'LaTeX-mode) "main.tex" "main.qmd") directory))
          (buffer (generate-new-buffer " *authoring test*")))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer buffer
             (setq buffer-file-name filename)
             (insert ,text)
             (skip-unless (or (fboundp ,mode) (eq ,mode 'LaTeX-mode)))
             (funcall (if (and (eq ,mode 'LaTeX-mode) (not (fboundp ,mode)))
                          #'latex-mode ,mode))
             (goto-char (point-min))
             ,@body))
       (dolist (buf (buffer-list))
         (when (and (buffer-live-p buf)
                    (or (eq buf buffer)
                        (with-current-buffer buf
                          (and buffer-file-name (string-prefix-p directory buffer-file-name)))))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory directory t))))

(defun authoring-test--file (name &optional contents)
  (let ((file (expand-file-name name default-directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert (or contents "image")))
    file))

(defmacro authoring-test--images (&rest body)
  (declare (indent 0))
  `(cl-letf (((symbol-function 'create-image)
              (lambda (file &rest _) (list 'image :type 'png :file file)))
             ((symbol-function 'image-flush) #'ignore))
     ,@body))

(defun authoring-test--overlays ()
  (cl-remove-if-not (lambda (ov) (overlay-get ov 'live-tex-preview-authoring-image))
                    (overlays-in (point-min) (point-max))))

(ert-deftest authoring-markdown-local-inline-reference-and-parentheses ()
  (authoring-test--document 'markdown-mode
      "![A](<figures/a b.png>) ![B](figures/c(d).png) ![C][plot]\n\n[plot]: figures/ref.png\n"
    (dolist (file '("figures/a b.png" "figures/c(d).png" "figures/ref.png"))
      (authoring-test--file file))
    (authoring-test--images
      (should (= 3 (live-tex-preview-authoring-preview-images-buffer)))
      (should (= 3 (length (authoring-test--overlays)))))))

(ert-deftest authoring-markdown-excludes-code-yaml-comments-and-escaped-images ()
  (authoring-test--document 'markdown-mode
      "---\ntitle: '![yaml](a.png)'\n---\n\n![real](a.png)\n`![inline](a.png)`\n\n```python\n![fenced](a.png)\n```\n\n    ![indented](a.png)\n\n<!-- ![comment](a.png) -->\n\\![escaped](a.png)\n"
    (authoring-test--file "a.png")
    (authoring-test--images
      (should (= 1 (live-tex-preview-authoring-preview-images-buffer))))))

(ert-deftest authoring-tex-multiple-images-on-one-line ()
  (authoring-test--document 'LaTeX-mode
      "\\includegraphics{a} \\includegraphics[width=1cm]{b}\n"
    (authoring-test--file "a.png") (authoring-test--file "b.png")
    (authoring-test--images
      (should (= 2 (live-tex-preview-authoring-preview-images-buffer))))))

(ert-deftest authoring-refresh-and-edit-remove-stale-overlays ()
  (authoring-test--document 'markdown-mode "![real](a.png)\n"
    (authoring-test--file "a.png")
    (authoring-test--images
      (live-tex-preview-authoring-preview-images-buffer)
      (live-tex-preview-authoring-preview-images-buffer)
      (should (= 1 (length (authoring-test--overlays))))
      (goto-char 5) (insert "edit")
      (should-not (authoring-test--overlays)))))

(ert-deftest authoring-missing-and-remote-images-do-not-abort-buffer-preview ()
  (authoring-test--document 'markdown-mode
      "![remote](https://example.com/a.png) ![missing](gone.png) ![ok](a.png)\n"
    (authoring-test--file "a.png")
    (authoring-test--images
      (should (= 1 (live-tex-preview-authoring-preview-images-buffer))))))

(ert-deftest authoring-html-producer-on-comment-and-image ()
  (authoring-test--document 'markdown-mode
      "<!--\n!src = figures/my plot.sh\n-->\n\n![Plot](plot.png){#fig-plot}\n"
    (let ((source (authoring-test--file "figures/my plot.sh" "printf produced > generated.txt\n")))
      (search-forward "!src")
      (should (equal (live-tex-preview-authoring--markdown-figure-source) '(src . "figures/my plot.sh")))
      (search-forward "![Plot]")
      (should (equal (live-tex-preview-authoring--figure-source-command)
                     (cons (concat "bash " (shell-quote-argument source)) default-directory))))))

(ert-deftest authoring-producer-stops-at-intervening-prose-or-image ()
  (dolist (between '("Paragraph.\n" "![Other](other.png)\n" "```python\npass\n```\n"))
    (authoring-test--document 'markdown-mode
        (concat "<!-- !run = echo wrong -->\n" between "![Target](target.png)\n")
      (search-forward "![Target]")
      (should-error (live-tex-preview-authoring--figure-source-command) :type 'user-error))))

(ert-deftest authoring-producer-in-code-is-never-dispatched ()
  (authoring-test--document 'markdown-mode
      "```html\n<!-- !run = echo wrong -->\n```\n"
    (search-forward "!run")
    (should-not (live-tex-preview-authoring-dwim-at-point t))))

(ert-deftest authoring-explicit-producer-runs-in-document-directory ()
  (authoring-test--document 'markdown-mode
      "<!-- !src = figures/my plot.sh -->\n![Plot](plot.png)\n"
    (authoring-test--file "figures/my plot.sh" "printf produced > generated.txt\n")
    (search-forward "!src")
    (let* ((compilation-ask-about-save nil)
           (compile-command "")
           (buffer (live-tex-preview-authoring-run-figure-source))
           (process (get-buffer-process buffer))
           (deadline (+ (float-time) 5)))
      (unwind-protect
          (progn
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05))
            (should (zerop (process-exit-status process)))
            (should (file-exists-p (expand-file-name "generated.txt" default-directory))))
        (when (process-live-p process) (delete-process process))
        (kill-buffer buffer)))))

(ert-deftest authoring-tex-producer-keeps-master-relative-paths ()
  (authoring-test--document 'LaTeX-mode
      "% !TEX root = ../master.tex\n\\begin{figure}\n% !run = python figures/plot.py\n\\includegraphics{plot}\n\\end{figure}\n"
    (search-forward "!run")
    (should (equal (live-tex-preview-authoring--figure-source-command)
                   (cons "python figures/plot.py" (file-name-directory (directory-file-name default-directory)))))))

(ert-deftest authoring-markdown-ignores-tex-master-setting ()
  (authoring-test--document 'markdown-mode "% !TEX root = /wrong/master.tex\n"
    (setq-local TeX-master "/wrong/master.tex")
    (should (equal (live-tex-preview-authoring--main-dir) default-directory))))

(ert-deftest authoring-tex-citations-and-reference-macros ()
  (dolist (macro '("\\eqref{eq:a}" "\\cref{eq:a}" "\\autoref{eq:a}"
                    "\\citep[see][p. 2]{smith2024}" "\\parencite{smith2024}"))
    (authoring-test--document 'LaTeX-mode macro
      (forward-char 2)
      (let ((pos (live-tex-preview-authoring--tex-crossref-at-point)))
        (should pos)
        (should (member (buffer-substring-no-properties pos (1- (point-max)))
                        '("eq:a" "smith2024")))))))

(ert-deftest authoring-tex-dwim-calls-reftex-and-selects-destination ()
  (authoring-test--document 'LaTeX-mode "\\eqref{eq:a}\n"
    (search-forward "eq:a") (backward-char 2)
    (let (argument)
      (cl-letf (((symbol-function 'reftex-view-crossref) (lambda (arg &rest _) (setq argument arg))))
        (should (live-tex-preview-authoring-dwim-at-point t))
        (should (eq argument 2))))))

(ert-deftest authoring-markdown-equation-reference-jumps-and-can-go-back ()
  (authoring-test--document 'markdown-mode
      "See @eq-energy.\n\n$$ E = mc^2 $$ {#eq-energy}\n"
    (search-forward "@eq-energy") (backward-char 3)
    (let ((origin (point)))
      (switch-to-buffer (current-buffer))
      (should (live-tex-preview-authoring-dwim-at-point t))
      (should (looking-at-p "{#eq-energy}"))
      (xref-go-back)
      (should (= origin (point))))))

(ert-deftest authoring-markdown-citation-opens-yaml-bibliography-entry ()
  (skip-unless (require 'citar nil t))
  (authoring-test--document 'markdown-mode
      "---\nbibliography: refs.bib\n---\n\nSee [@smith2024].\n"
    (let ((bib (authoring-test--file "refs.bib" "@article{smith2024, title={A result}, author={Smith}, year={2024}}\n")))
      (save-buffer)
      (search-forward "@smith2024") (backward-char 3)
      (should (live-tex-preview-authoring-dwim-at-point t))
      (with-current-buffer (window-buffer (selected-window))
        (should (equal buffer-file-name bib))
        (bibtex-beginning-of-entry)
        (should (looking-at-p "@article{smith2024"))))))

(ert-deftest authoring-markdown-citation-group-selects-individual-key ()
  (skip-unless (require 'citar-markdown nil t))
  (authoring-test--document 'markdown-mode "See [@first; @second, p. 5].\n"
    (search-forward "@second") (backward-char 3)
    (should (equal (live-tex-preview-authoring--markdown-keys-at-point) '("second")))
    (goto-char (point-min)) (search-forward "[") (backward-char 1)
    (should (equal (live-tex-preview-authoring--markdown-keys-at-point) '("first" "second")))))

(ert-deftest authoring-markdown-email-escaped-at-and-code-are-not-citations ()
  (dolist (text '("a@smith2024" "\\@smith2024" "`@smith2024`" "<!-- @smith2024 -->"
                  "```python\nx = '@smith2024'\n```"))
    (authoring-test--document 'markdown-mode text
      (search-forward "smith2024") (backward-char 3)
      (should-not (live-tex-preview-authoring-dwim-at-point t)))))

(ert-deftest authoring-missing-equation-reference-reports-real-error ()
  (authoring-test--document 'markdown-mode "See @eq-missing.\n"
    (search-forward "eq-missing") (backward-char 2)
    (should-error (live-tex-preview-authoring-dwim-at-point t) :type 'user-error)))

(ert-deftest authoring-markdown-raw-tex-equation-references ()
  (dolist (mode '(markdown-mode poly-quarto-mode))
    (authoring-test--document mode
        "See \\eqref{eq:energy}.\n\n$$ E=mc^2 \\label{eq:energy} $$\n"
      (search-forward "eq:energy") (backward-char 3)
      (should (live-tex-preview-authoring-dwim-at-point t))
      (should (looking-at-p "\\\\label{eq:energy}")))))

(ert-deftest authoring-links-containing-at-signs-stay-links ()
  (authoring-test--document 'markdown-mode "[Profile](https://example.com/@smith2024)\n"
    (search-forward "@smith2024") (backward-char 3)
    (let (url)
      (cl-letf (((symbol-function 'browse-url) (lambda (target &rest _) (setq url target))))
        (should (live-tex-preview-authoring-dwim-at-point t))
        (should (equal url "https://example.com/@smith2024"))))))

(ert-deftest authoring-project-reference-opens-another-document ()
  (authoring-test--document 'markdown-mode "See @eq-other.\n"
    (let ((file (authoring-test--file "chapter.qmd" "$$ x $$ {#eq-other}\n")))
      (search-forward "@eq-other") (backward-char 3)
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'test-project))
                ((symbol-function 'project-files) (lambda (&rest _) (list file))))
        (should (live-tex-preview-authoring-dwim-at-point t))
        (with-current-buffer (window-buffer (selected-window))
          (should (equal buffer-file-name file))
          (should (looking-at-p "{#eq-other}")))))))

(ert-deftest authoring-tex-reftex-follows-a-real-equation-label ()
  (authoring-test--document 'LaTeX-mode
      "\\documentclass{article}\n\\begin{document}\nSee \\eqref{eq:energy}.\n\\begin{equation} E=mc^2 \\label{eq:energy} \\end{equation}\n\\end{document}\n"
    (setq-local TeX-master t)
    (save-buffer)
    (reftex-mode 1)
    (switch-to-buffer (current-buffer))
    (search-forward "eq:energy") (backward-char 3)
    (should (live-tex-preview-authoring-dwim-at-point t))
    (with-current-buffer (window-buffer (selected-window))
      (should (= 4 (line-number-at-pos))))))

(ert-deftest authoring-real-image-spec-and-toggle ()
  (skip-unless (image-type-available-p 'png))
  (authoring-test--document 'markdown-mode "![Plot](a.png)\n"
    (let ((coding-system-for-write 'binary))
      (authoring-test--file "a.png"
                            (base64-decode-string "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aD1sAAAAASUVORK5CYII=")))
    (search-forward "Plot")
    (live-tex-preview-authoring-preview-image-at-point)
    (should (eq (car (overlay-get (car (authoring-test--overlays)) 'display)) 'image))
    (live-tex-preview-authoring-preview-image-at-point)
    (should-not (authoring-test--overlays))))

(ert-deftest authoring-simple-yaml-bibliography-forms ()
  (dolist (field '("bibliography: [a.bib, 'b b.bib']" "bibliography:\n  - a.bib\n  - \"b b.bib\""))
    (authoring-test--document 'markdown-mode (concat "---\n" field "\n---\n")
      (should (equal (live-tex-preview-authoring--yaml-bibliography) '("a.bib" "b b.bib"))))))

(ert-deftest authoring-quarto-host-keeps-images-and-ignores-code ()
  (authoring-test--document 'poly-quarto-mode
      "![Host](a.png)\n\n```{python}\ntext = '![Code](a.png)'\n```\n"
    (authoring-test--file "a.png")
    (let ((host (current-buffer)) inner)
      (authoring-test--images
        (should (= 1 (live-tex-preview-authoring-preview-images-buffer)))
        (search-forward "text =")
        (pm-switch-to-buffer)
        (setq inner (current-buffer))
        (should (derived-mode-p 'python-mode))
        (should (eq (buffer-base-buffer) host))
        (should-not (live-tex-preview-authoring-dwim-at-point t))
        (live-tex-preview-authoring-clear-image-previews)
        (with-current-buffer host (should-not (authoring-test--overlays)))
        (should-not (authoring-test--overlays))
        (should (buffer-live-p inner))))))

(ert-deftest authoring-equations-use-package-markdown-and-quarto-frontend ()
  (authoring-test--document 'poly-quarto-mode
      "Inline $x$.\n\n$$ y^2 $$ {#eq-y}\n\n```{python}\nx = '$code$'\n```\n"
    (live-tex-preview--prepare-frontend)
    (should (eq live-tex-preview-scan-function #'live-tex-preview-markdown--scan-region))
    (should (equal (mapcar #'caddr (funcall live-tex-preview-scan-function 1 (point-max)))
                   '("$x$" "$$ y^2 $$")))))

(ert-deftest authoring-ordinary-prose-keeps-enter-fallback ()
  (dolist (mode '(markdown-mode LaTeX-mode))
    (authoring-test--document mode "Ordinary prose.\n"
      (should-not (live-tex-preview-authoring-dwim-at-point t)))))

(ert-deftest authoring-cross-references-work-without-citar ()
  (authoring-test--document 'markdown-mode "See @eq-energy.\n\n$$ x $$ {#eq-energy}\n"
    (search-forward "eq-energy") (backward-char 3)
    (let ((original (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest args)
                   (unless (memq feature '(citar citar-markdown))
                     (apply original feature args)))))
        (should (live-tex-preview-authoring-dwim-at-point t))
        (should (looking-at-p "{#eq-energy}"))))))

(ert-deftest authoring-missing-citation-backend-is-optional ()
  (authoring-test--document 'markdown-mode "Ordinary prose. Then @smith2024.\n"
    (let ((original (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest args)
                   (unless (memq feature '(citar citar-markdown))
                     (apply original feature args)))))
        (should-not (live-tex-preview-authoring-dwim-at-point t))
        (search-forward "smith2024") (backward-char 3)
        (should-error (live-tex-preview-authoring-dwim-at-point t) :type 'user-error)))))


(ert-deftest authoring-public-context-resolves-master-without-enabling-previews ()
  (authoring-test--document 'latex-mode "% !TEX root = ../first.tex\n"
    (should (eq (live-tex-preview-document-buffer) (current-buffer)))
    (should (equal (live-tex-preview-document-directory)
                   (file-name-directory (directory-file-name default-directory))))
    (should-not live-tex-preview-mode)
    (erase-buffer) (insert "% !TEX root = next/master.tex\n")
    (should (equal (live-tex-preview-document-directory)
                   (expand-file-name "next/" default-directory)))))

(ert-deftest authoring-pdf-conversion-first-page-and-cache-dpi ()
  (skip-unless (and (executable-find "pdflatex") (executable-find "pdftoppm")))
  (authoring-test--document 'latex-mode ""
    (let* ((source (authoring-test--file
                    "figure.tex"
                    "\\documentclass{article}\n\\begin{document}first\\newpage second\\end{document}\n"))
           (pdf (concat (file-name-sans-extension source) ".pdf")))
      (should (= 0 (call-process "pdflatex" nil nil nil "-interaction=nonstopmode" source)))
      (let ((png (live-tex-preview-authoring--convert-to-png pdf)))
        (should (file-regular-p png))
        (should (equal png (live-tex-preview-authoring--convert-to-png pdf)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally png)
          (should (equal (buffer-substring-no-properties 1 5) (unibyte-string 137 80 78 71))))
        (let ((live-tex-preview-authoring-pdf-preview-dpi 100))
          (should-not (equal png (live-tex-preview-authoring--convert-to-png pdf))))))))
