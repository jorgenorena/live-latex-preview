;;; live-tex-preview-polymode.el --- Markdown host ownership -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Jorge Noreña
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Optional adapter for poly-markdown/poly-quarto.  Rendered source overlays
;; always stay in the Markdown host.  A math innermode gets a temporary live
;; image presentation, not its own frontend or preview minor mode.  Code
;; innermodes get neither.  No span, syntax, execution or navigation settings
;; are changed.  GPL version 3 or later; see COPYING.
;;; Code:
(require 'live-tex-preview-markdown)
(require 'polymode)

(defvar-local live-tex-preview-polymode--source nil)
(defvar-local live-tex-preview-polymode--presentation nil)
(defvar-local live-tex-preview-polymode--views nil)

(defun live-tex-preview-polymode--excluded-p (pos)
  "Return non-nil if POS is in a non-math inner span."
  (when (bound-and-true-p polymode-mode)
    (let ((span (pm-innermost-span pos)))
      (and (car span)
           (not (memq (oref (nth 3 span) mode) '(latex-mode LaTeX-mode tex-mode)))))))

(defun live-tex-preview-polymode--refresh (&optional source)
  "Refresh this math view from SOURCE or its saved host overlay."
  (let ((source (or source live-tex-preview-polymode--source))
        (view live-tex-preview-polymode--presentation))
    (when (and (overlayp source) (overlay-buffer source)
               (overlayp view) (overlay-buffer view))
      (when-let ((image (overlay-get source 'live-tex-preview-image)))
        (let ((block (with-current-buffer (overlay-buffer source)
                       (save-restriction
                         (widen)
                         (live-tex-preview-markdown--block-p
                          (buffer-substring-no-properties (overlay-start source) (overlay-end source)))))))
          (when (live-tex-preview-live--context-enabled-p
                 (if block 'block 'inline) live-tex-preview-display-live)
            (if (and block (eq live-tex-preview-live-block-display 'posframe))
                (live-tex-preview-live--show-popup view image)
              (when (or (not block) (eq live-tex-preview-live-block-display 'buffer))
                (live-tex-preview-live--install-docstring view block)
                (live-tex-preview-live--update-props image '(:box t))))))))))

(defun live-tex-preview-polymode--updated (source)
  "Deliver a host SOURCE image to any active math view."
  (dolist (buffer live-tex-preview-polymode--views)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq source live-tex-preview-polymode--source)
          (live-tex-preview-polymode--refresh source))))))

(defun live-tex-preview-polymode--closed (source)
  "Remove math presentations of SOURCE when its host preview is cleared."
  (dolist (buffer live-tex-preview-polymode--views)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq source live-tex-preview-polymode--source)
          (live-tex-preview-polymode--detach))))))

(defun live-tex-preview-polymode--regenerate (&rest _)
  "Regenerate the host fragment underlying this math view."
  (let ((source live-tex-preview-polymode--source))
    (when (and (overlayp source) (overlay-buffer source))
      (with-current-buffer (overlay-buffer source)
        (save-restriction
          (widen)
          (live-tex-preview-mode--regenerate-overlay source))))))

(defun live-tex-preview-polymode--changed (beg end old-length)
  "Invalidate the host image after a math-view edit at BEG END OLD-LENGTH."
  (when (and (overlayp live-tex-preview-polymode--source)
             (overlay-buffer live-tex-preview-polymode--source))
    (live-tex-preview--mark-modified live-tex-preview-polymode--source t beg end old-length)
    (when live-tex-preview-live--generator
      (funcall live-tex-preview-live--generator))))

(defun live-tex-preview-polymode--detach ()
  "Remove this buffer's temporary math presentation and timers."
  (remove-hook 'after-change-functions #'live-tex-preview-polymode--changed t)
  (remove-hook 'post-command-hook #'live-tex-preview-polymode--refresh t)
  (remove-hook 'pre-command-hook #'live-tex-preview-polymode--before-motion t)
  (when (overlayp live-tex-preview-polymode--presentation)
    (delete-overlay live-tex-preview-polymode--presentation))
  (setq live-tex-preview-polymode--presentation nil
        live-tex-preview-polymode--source nil
        live-tex-preview-live--generator nil)
  (live-tex-preview--cleanup))

(defun live-tex-preview-polymode--before-switch (old _new)
  "Retire OLD's math view before Polymode switches buffers."
  (when (buffer-live-p old)
    (with-current-buffer old
      (when (and live-tex-preview-mode live-tex-preview-mode--from-overlay
                 (markerp live-tex-preview-mode--marker)
                 (marker-position live-tex-preview-mode--marker))
        (save-restriction
          (widen)
          (live-tex-preview-mode--close-previous-overlay)))
      (when live-tex-preview-polymode--source
        (let ((source live-tex-preview-polymode--source))
          (live-tex-preview-polymode--detach)
          (when (overlay-buffer source)
            (with-current-buffer (overlay-buffer source)
              (save-restriction
                (widen)
                (overlay-put source 'live-tex-preview-view-text nil)
                (if (eq (overlay-get source 'live-tex-preview-state) 'modified)
                    (live-tex-preview-mode--regenerate-overlay source)
                  (overlay-put source 'display (overlay-get source 'live-tex-preview-image)))))))))))

(defun live-tex-preview-polymode--after-switch (_old new)
  "Attach a temporary math view in NEW, if its host owns a preview there."
  (when (buffer-live-p new)
    (with-current-buffer new
      (let* ((host (live-tex-preview--document-buffer))
             (pos (point))
             (span (and (not (eq host new)) (pm-innermost-span pos)))
             (source
              (and span (memq major-mode '(latex-mode LaTeX-mode tex-mode))
                   (buffer-local-value 'live-tex-preview-mode host)
                   (with-current-buffer host
                     (save-restriction
                       (widen)
                       (cl-find-if
                        (lambda (ov)
                          (and (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
                               (live-tex-preview-mode--fragment-for-overlay ov)))
                        (overlays-at pos)))))))
        (when source
          (setq live-tex-preview-polymode--source source
                live-tex-preview-polymode--presentation
                (make-overlay (nth 1 span) (nth 2 span) nil nil t))
          (overlay-put live-tex-preview-polymode--presentation 'live-tex-preview-presentation t)
          (overlay-put source 'live-tex-preview-view-text t)
          (overlay-put source 'display nil)
          (with-current-buffer host (cl-pushnew new live-tex-preview-polymode--views))
          (dolist (variable '(live-tex-preview-display-live live-tex-preview-live-block-display
							    live-tex-preview-update-delay live-tex-preview-update-throttle))
            (set (make-local-variable variable) (buffer-local-value variable host)))
          (setq-local live-tex-preview-live--generator
                      (live-tex-preview-live--debounce
                       (live-tex-preview-live--throttle #'live-tex-preview-polymode--regenerate)
                       live-tex-preview-update-delay))
          (add-hook 'after-change-functions #'live-tex-preview-polymode--changed nil t)
          (add-hook 'post-command-hook #'live-tex-preview-polymode--refresh 95 t)
          (add-hook 'pre-command-hook #'live-tex-preview-polymode--before-motion -90 t)
          (add-hook 'kill-buffer-hook #'live-tex-preview-polymode--detach nil t)
          (live-tex-preview-polymode--refresh))))))

(defun live-tex-preview-polymode--before-motion ()
  "Clear inline math-view display strings before visual motion."
  (when (and (overlayp live-tex-preview-polymode--presentation)
             (memq this-command live-tex-preview-live-clear-before-commands))
    (overlay-put live-tex-preview-polymode--presentation 'after-string nil)))

(defun live-tex-preview-polymode--teardown ()
  "Detach all math presentations belonging to this host."
  (dolist (buffer live-tex-preview-polymode--views)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (live-tex-preview-polymode--detach))))
  (setq live-tex-preview-polymode--views nil))

(defun live-tex-preview-polymode--setup ()
  "Set up a Markdown host's optional Polymode adapter."
  (add-hook 'live-tex-preview-overlay-update-functions #'live-tex-preview-polymode--updated nil t)
  (add-hook 'live-tex-preview-overlay-close-functions #'live-tex-preview-polymode--closed nil t)
  (add-hook 'live-tex-preview-mode-hook #'live-tex-preview-polymode--mode-changed nil t)
  (add-hook 'kill-buffer-hook #'live-tex-preview-polymode--teardown nil t))

(defun live-tex-preview-polymode--mode-changed ()
  "Detach math views when the host preview mode is disabled."
  (unless live-tex-preview-mode (live-tex-preview-polymode--teardown)))

;; Polymode otherwise moves every ordinary overlay into the new inner buffer.
;; These predicates affect only this package's two overlay types.
(add-to-list 'polymode-ignore-overlays-with-these-properties 'live-tex-preview-type)
(add-to-list 'polymode-ignore-overlays-with-these-properties 'live-tex-preview-presentation)
(add-hook 'polymode-before-switch-buffer-hook #'live-tex-preview-polymode--before-switch)
(add-hook 'polymode-after-switch-buffer-hook #'live-tex-preview-polymode--after-switch)

(provide 'live-tex-preview-polymode)
;;; live-tex-preview-polymode.el ends here
