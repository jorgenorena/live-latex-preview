;;; markdown-test.el --- Markdown and Polymode tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Jorge Noreña
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'live-tex-preview-test)
(require 'live-tex-preview-markdown)

(defmacro live-tex-preview-test--markdown (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (markdown-mode)
     (insert ,text)
     (live-tex-preview--prepare-frontend)
     ,@body))

(defun live-tex-preview-test--math (text)
  (live-tex-preview-test--markdown text
				   (mapcar #'caddr (live-tex-preview-markdown--scan-region 1 (point-max)))))

(ert-deftest live-tex-preview-md-delimiters ()
  (dolist (math '("$x$" "$$x$$" "\\(x\\)" "\\[x\\]"))
    (should (equal (live-tex-preview-test--math math) (list math)))))

(ert-deftest live-tex-preview-md-dollar-policy ()
  (dolist (text '("\\$x$" "$20" "Costs $20 and $30." "$ x$" "$x $"
                  "word$x$word" "$x$2" "$$$x$$$" "$x\ny$"))
    (should-not (live-tex-preview-test--math text)))
  (should (equal (live-tex-preview-test--math "($x$), $y$. \\(20\\)")
                 '("$x$" "$y$" "\\(20\\)"))))

(ert-deftest live-tex-preview-md-numeric-leading-math ()
  (should (equal (live-tex-preview-test--math "$2x$ and $20$, $0.5$; $2 + 3$.")
                 '("$2x$" "$20$" "$0.5$" "$2 + 3$")))
  ;; An invalid currency closer must not consume the next mathematical opener.
  (should (equal (live-tex-preview-test--math "Costs $20 and $30. Then $2x$.")
                 '("$2x$")))
  (dolist (text '("$2x\n+ 1$" "$20\n\n30$" "\\$2x$" "`$2x$`"
                  "```{python}\nx = '$2x$'\n```"))
    (should-not (live-tex-preview-test--math text))))

(ert-deftest live-tex-preview-md-inline-code ()
  (dolist (text '("`$x$`" "``$x$ ` $y$``" "`multiline\n$x$`" "`\\(x\\)`"))
    (should-not (live-tex-preview-test--math text))))

(ert-deftest live-tex-preview-md-fences-and-indent ()
  (dolist (text '("```python\nx = '$x$'\n```" "~~~r\n$x$\n~~~"
                  "````text\n```\n$x$\n````" "```\n$x$"
                  "Paragraph.\n\n    $x$\n    \\(y\\)\n"))
    (should-not (live-tex-preview-test--math text))))

(ert-deftest live-tex-preview-md-multiline-and-multiple ()
  (should (equal (live-tex-preview-test--math "$a$ and $b$, \\(c\\).\n\n$$\nx^2\n$$\n\\[\ny^2\n\\]")
                 '("$a$" "$b$" "\\(c\\)" "$$\nx^2\n$$" "\\[\ny^2\n\\]"))))

(ert-deftest live-tex-preview-md-requested-example ()
  (should (equal (live-tex-preview-test--math
                  "The result is $E=mc^2$.\n\nThis costs $20.\n\n`$not_math$`\n\n```python\nx = \"$not_math$\"\n")
                 '("$E=mc^2$"))))

(ert-deftest live-tex-preview-md-no-cross-code-or-paragraph ()
  (dolist (text '("$$x\n\n```python\ny\n```\n$$" "$$x `$code$` y$$"
                  "\\[x\n\ny\\]" "<!-- $comment$ -->"))
    (should-not (live-tex-preview-test--math text))))

(ert-deftest live-tex-preview-md-no-arbitrary-environments ()
  (should-not (live-tex-preview-test--math "\\begin{equation}x\\end{equation}")))

(ert-deftest live-tex-preview-md-yaml ()
  (should (equal (live-tex-preview-test--math
                  "---\ntitle: $ignored$\nheader-includes:\n  - \\newcommand{\\Pk}{$notmath$}\n---\n$x$") '("$x$"))))

(defconst live-tex-preview-test--qmd
  "---\ntitle: $Test$\n---\n\nInline math: $E=mc^2$.\n\n$$\nH^2 = \\frac{8\\pi G}{3}\\rho\n$$\n\n```{python}\nx = \"$this is not math$\"\n```\n\n```{r}\nx <- '$r$'\n```\n\n```{julia}\nx = \"$julia$\"\n```\n\nMore prose with $\\Omega_m$.\n")

(ert-deftest live-tex-preview-qmd-prose-cells ()
  (should (equal (live-tex-preview-test--math live-tex-preview-test--qmd)
                 '("$E=mc^2$" "$$\nH^2 = \\frac{8\\pi G}{3}\\rho\n$$" "$\\Omega_m$"))))

(ert-deftest live-tex-preview-md-current-fragment-and-regions ()
  (live-tex-preview-test--markdown "A $x$ and $$y$$ then $z$."
				   (goto-char 4)
				   (should (equal (caddr (live-tex-preview--current-fragment)) "$x$"))
				   (goto-char 13)
				   (should (equal (caddr (live-tex-preview--current-fragment)) "$$y$$"))
				   (should-not (live-tex-preview-markdown--scan-region 3 4))
				   (should (equal (mapcar #'caddr (live-tex-preview-markdown--scan-region 6 3)) '("$x$")))))

(ert-deftest live-tex-preview-md-broken-delimiter-stays-local ()
  (dolist (math '("$x$" "$$x$$" "\\(x\\)" "\\[x\\]"))
    (live-tex-preview-test--markdown (concat math " later " math)
				     (let ((ov (live-tex-preview--ensure-overlay 1 (1+ (length math)))))
				       (goto-char (1- (overlay-end ov)))
				       (delete-char 1)
				       (should-not (live-tex-preview-mode--fragment-for-overlay ov))))))

(ert-deftest live-tex-preview-md-live-lookup-does-not-scan-document ()
  (live-tex-preview-test--markdown "$x$"
				   (let ((ov (live-tex-preview--ensure-overlay 1 4)))
				     (cl-letf (((symbol-function 'live-tex-preview-markdown--scan-region)
						(lambda (&rest _) (ert-fail "Full scan during live edit"))))
				       (should (equal (live-tex-preview-mode--fragment-for-overlay ov) '(1 4 "$x$")))))))

(ert-deftest live-tex-preview-md-source-becomes-code ()
  (live-tex-preview-test--markdown "$x$"
				   (let ((ov (live-tex-preview--ensure-overlay 1 4)))
				     (goto-char 1) (insert "```python\n")
				     (live-tex-preview-markdown--validate-overlays)
				     (should-not (overlay-buffer ov)))))

(ert-deftest live-tex-preview-md-render-live-and-preamble ()
  (skip-unless (executable-find "latex"))
  (let ((directory (make-temp-file "live-tex-md-" t)))
    (unwind-protect
        (live-tex-preview-test--markdown "Start $\\Pk$ and \\[x\\] end."
					 (setq default-directory (file-name-as-directory directory))
					 (setq-local live-tex-preview-markdown-extra-preamble "\\newcommand{\\Pk}{P(k)}")
					 (cl-letf (((symbol-function 'live-tex-preview--graphical-frame-p) (lambda () t)))
					   (live-tex-preview-mode 1)
					   (live-tex-preview-buffer)
					   (live-tex-preview-test--await (car live-tex-preview--jobs))
					   (goto-char 8)
					   (set-marker live-tex-preview-mode--marker (point))
					   (let ((ov (car (overlays-at (point)))))
					     (live-tex-preview-mode--open-this-overlay)
					     (should (overlay-get ov 'after-string))
					     (goto-char (1- (overlay-end ov))) (insert "+1")
					     (live-tex-preview-live--regenerate)
					     (live-tex-preview-test--await (car live-tex-preview--jobs))
					     (live-tex-preview-mode--handle-pre-cursor)
					     (goto-char (overlay-end ov))
					     (live-tex-preview-mode--handle-post-cursor)
					     (should (overlay-get ov 'display)))
					   (live-tex-preview-mode -1)))
      (delete-directory directory t))))

(ert-deftest live-tex-preview-qmd-real-polymode-ownership ()
  (skip-unless (require 'quarto-mode nil t))
  (let ((host (generate-new-buffer "test.qmd")))
    (unwind-protect
        (with-current-buffer host
          (insert live-tex-preview-test--qmd)
          (poly-quarto-mode)
          (live-tex-preview--prepare-frontend)
          (should (= (length (live-tex-preview-markdown--scan-region 1 (point-max))) 3))
          (let* ((entry (car (live-tex-preview-markdown--scan-region 1 (point-max))))
                 (ov (live-tex-preview--ensure-overlay (car entry) (cadr entry)))
                 (before (buffer-substring-no-properties 1 (point-max))))
            (goto-char 1) (search-forward "x =")
            (let* ((pos (point)) (span (pm-innermost-span)) (inner (pm-span-buffer span)))
              (should-not (eq inner host))
              (with-current-buffer inner
                (pm-narrow-to-span span)
                (goto-char pos)
                (let ((bounds (cons (point-min) (point-max))))
                  (cl-letf (((symbol-function 'live-tex-preview--graphical-frame-p) (lambda () t)))
                    (live-tex-preview-mode 1)
                    (should-not live-tex-preview-mode))
                  (live-tex-preview--with-document
                   (should (eq (current-buffer) host))
                   (should (= (length (funcall live-tex-preview-scan-function 1 (point-max))) 3)))
                  (should (equal bounds (cons (point-min) (point-max))))))
              ;; Use Polymode's actual overlay-transfer operation.
              (pm--move-overlays host inner)
              (should (eq (overlay-buffer ov) host)))
            (should (equal before (buffer-substring-no-properties 1 (point-max))))))
      (when (buffer-live-p host) (kill-buffer host)))))

(ert-deftest live-tex-preview-qmd-math-inner-live ()
  (skip-unless (and (require 'quarto-mode nil t) (executable-find "latex")))
  (let ((host (generate-new-buffer "math-test.qmd"))
        (directory (make-temp-file "live-tex-qmd-" t)))
    (unwind-protect
        (with-current-buffer host
          (insert "Before $x_1$ after.\n\n```{python}\nx = '$no$'\n```\n\nEnd $y$.\n")
          (poly-quarto-mode)
          (setq default-directory (file-name-as-directory directory))
          (setq-local markdown-enable-math t)
          (pm-flush-span-cache 1 (point-max))
          (cl-letf (((symbol-function 'live-tex-preview--graphical-frame-p) (lambda () t)))
            (live-tex-preview-mode 1)
            (live-tex-preview-buffer)
            ;; Callback must work even if the host has become narrowed.
            (save-restriction
              (narrow-to-region 1 6)
              (live-tex-preview-test--await (car live-tex-preview--jobs))
              (should (= (point-max) 6)))
            (goto-char 10)
            (let* ((source (car (overlays-at (point))))
                   (old-key (plist-get (overlay-get source 'live-tex-preview-metadata) :key))
                   (span (pm-innermost-span))
                   (inner (pm-span-buffer span)))
              (should (car span))
              (should-not (eq inner host))
              (with-current-buffer inner
                (pm-narrow-to-span span)
                (goto-char 10)
                (live-tex-preview-polymode--after-switch host inner)
                (should-not live-tex-preview-mode)
                (should (eq source live-tex-preview-polymode--source))
                (should (overlay-get live-tex-preview-polymode--presentation 'after-string))
                (insert "z")
                (live-tex-preview-polymode--regenerate)
                (live-tex-preview-test--await (car (buffer-local-value 'live-tex-preview--jobs host)))
                (should-not (equal old-key (plist-get (overlay-get source 'live-tex-preview-metadata) :key)))
                (live-tex-preview-polymode--before-switch inner host)
                (should-not live-tex-preview-polymode--presentation))
              (should (eq (overlay-buffer source) host))
              (should (overlay-get source 'display)))
            (live-tex-preview-mode -1)))
      (when (buffer-live-p host) (kill-buffer host))
      (delete-directory directory t))))

(ert-deftest live-tex-preview-md-block-after-inline-view ()
  (live-tex-preview-test--markdown "$$x$$"
				   (let ((ov (live-tex-preview--ensure-overlay 1 6))
					 (live-tex-preview-live--block-p nil) shown)
				     (overlay-put ov 'live-tex-preview-image '(image :type svg :file "unused.svg"))
				     (cl-letf (((symbol-function 'live-tex-preview-live--show-popup)
						(lambda (overlay _image) (setq shown overlay))))
				       (live-tex-preview-live--ensure-open-overlay ov)
				       (should (eq ov shown))
				       (should-not (overlay-get ov 'after-string))))))

(provide 'markdown-test)
