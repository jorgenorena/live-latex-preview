;;; live-tex-preview.el --- Preview placement and editing -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'subr-x)
(require 'live-tex-preview-engine)
(defvar live-tex-preview-overlay-open-functions nil)
(defvar live-tex-preview-overlay-close-functions nil)
(defvar live-tex-preview-overlay-update-functions nil)
(defvar-local live-tex-preview--jobs nil)
(defvar-local live-tex-preview--timers nil)
(defvar live-tex-preview-overlay-priority)
(defcustom live-tex-preview-zoom 1.0
  "Font-relative display zoom; independent of compiled cache contents."
  :type 'number :group 'live-tex-preview-engine)

(defun live-tex-preview--mark-modified (ov after-p _beg _end &optional _length)
  "After a source change, invalidate pending renders and reveal OV."
  (when after-p
    (overlay-put ov 'live-tex-preview-generation (1+ (or (overlay-get ov 'live-tex-preview-generation) 0)))
    (overlay-put ov 'live-tex-preview-state 'modified)
    (overlay-put ov 'display nil)
    (overlay-put ov 'face nil)))

(defun live-tex-preview--ensure-overlay (beg end)
  "Find or create this package's overlay for BEG to END."
  (or (cl-find-if (lambda (ov)
                    (and (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
                         (= (overlay-start ov) beg) (= (overlay-end ov) end)))
                  (overlays-in beg end))
      (let ((ov (make-overlay beg end nil nil t)))
        (overlay-put ov 'live-tex-preview-type 'live-tex-preview-overlay)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority live-tex-preview-overlay-priority)
        (dolist (hook '(modification-hooks insert-in-front-hooks insert-behind-hooks))
          (overlay-put ov hook '(live-tex-preview--mark-modified)))
        ov)))

(cl-defun live-tex-preview-place (entries &key preamble page-width cache-directory input-directory)
  "Render (BEG END LATEX) ENTRIES and place previews in the current buffer.
Return the render job.  Keyword arguments are passed to
`live-tex-preview-render'.  Callbacks ignore deleted or edited overlays.
No LaTeX major mode is required.  Generic interaction uses the three
buffer-local fragment, render and block callback variables below."
  (let* ((buffer (current-buffer))
         (targets (mapcar
                   (lambda (entry)
                     (pcase-let ((`(,beg ,end ,_latex) entry))
                       (let* ((ov (live-tex-preview--ensure-overlay beg end))
                              (generation (1+ (or (overlay-get ov 'live-tex-preview-generation) 0))))
                         (overlay-put ov 'live-tex-preview-generation generation)
                         (list ov generation (buffer-substring-no-properties beg end)))))
                   entries))
         job)
    (setq job
          (live-tex-preview-render
           (mapcar #'caddr entries)
           (lambda (results error-text)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq live-tex-preview--jobs (delq job live-tex-preview--jobs))
                 (cl-loop for (ov generation source) in targets
                          for info across results
                          when (and (overlay-buffer ov)
                                    (= generation (overlay-get ov 'live-tex-preview-generation))
                                    (equal source (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
                          do
                          (if (null info)
                              (progn
                                (overlay-put ov 'help-echo error-text)
                                (overlay-put ov 'live-tex-preview-state 'modified))
                            (let ((image (live-tex-preview-image info live-tex-preview-zoom))
                                  (face (or (and (> (overlay-start ov) (point-min))
                                                 (get-text-property (1- (overlay-start ov)) 'face))
                                            'default)))
                              (overlay-put ov 'help-echo nil)
                              (overlay-put ov 'live-tex-preview-metadata info)
                              (overlay-put ov 'live-tex-preview-image image)
                              (overlay-put ov 'live-tex-preview-hidden-face face)
                              (overlay-put ov 'live-tex-preview-state 'active)
                              (unless (overlay-get ov 'live-tex-preview-view-text)
                                (overlay-put ov 'display image)
                                (overlay-put ov 'face face))
                              (live-tex-preview--sync-overlay-display ov)
                              (run-hook-with-args 'live-tex-preview-overlay-update-functions ov))))
                 (when error-text (message "live-tex-preview: %s" error-text)))))
           :preamble preamble :page-width page-width :cache-directory cache-directory
           :input-directory input-directory))
    (push job live-tex-preview--jobs)
    (add-hook 'kill-buffer-hook #'live-tex-preview--cleanup nil t)
    job))

(defun live-tex-preview-clear-overlays (&optional beg end)
  "Remove package previews between BEG and END (default: entire buffer)."
  (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
    (when (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
      (live-tex-preview-live--clearout ov)
      (delete-overlay ov))))

(defun live-tex-preview--cleanup ()
  "Cancel pending work and hide live UI owned by this buffer."
  (dolist (timer live-tex-preview--timers) (cancel-timer timer))
  (setq live-tex-preview--timers nil)
  (dolist (job (copy-sequence live-tex-preview--jobs)) (live-tex-preview-cancel job))
  (setq live-tex-preview--jobs nil)
  (live-tex-preview-live--hide-popup))

(defun live-tex-preview--schedule (delay function &rest args)
  "Run FUNCTION with ARGS after DELAY in this buffer; track its lifetime."
  (let ((buffer (current-buffer)) timer)
    (setq timer
          (run-at-time delay nil
                       (lambda ()
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (setq live-tex-preview--timers (delq timer live-tex-preview--timers))
                             (apply function args))))))
    (push timer live-tex-preview--timers)
    timer))
(declare-function posframe-hide "posframe")
(declare-function posframe-hidehandler-when-buffer-switch "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-workable-p "posframe")
(declare-function posframe-poshandler-point-bottom-left-corner "posframe")
(declare-function posframe-poshandler-point-bottom-left-corner-upward "posframe")
(defgroup live-tex-preview nil
  "Fast LaTeX previews and live editing."
  :group 'tex
  :prefix "live-tex-preview-")








(defcustom live-tex-preview-overlay-priority 20
  "Overlay priority used for rendered LaTeX previews.
This defaults to 20 so previews beat AUCTeX's `TeX-fold' overlays, including
inline math in folded section titles."
  :type '(choice (const :tag "No priority" nil)
                 (integer :tag "Overlay priority"))
  :group 'live-tex-preview)

(defcustom live-tex-preview-lazy-display nil
  "When non-nil, only display rendered previews near visible windows.
The image files and overlay metadata are still generated for every requested
fragment.  Off-screen overlays retain image metadata without an active
`display' property.  This avoids forcing Emacs/librsvg to realize a whole
buffer of SVGs in one redisplay pass."
  :type 'boolean
  :group 'live-tex-preview)

(defcustom live-tex-preview-lazy-display-margin 2000
  "Characters before and after visible windows kept as active preview images.
Only used when `live-tex-preview-lazy-display' is non-nil."
  :type 'integer
  :group 'live-tex-preview)


(defcustom live-tex-preview-display-live '(block inline)
  "Whether to show a live-updating preview while editing a fragment.
Either t (all fragments), nil (none), or a list of contexts among
`block' (display below environments / \\[ \\] / $$ $$) and `inline'
(display beside $ $ / \\( \\)).

Inline live previews use a small in-buffer `after-string'.  Block live previews
use a child-frame popup by default, so large rendered equations do not
participate in visual-line layout while point moves through the source."
  :type '(choice (const :tag "Everywhere" t)
                 (const :tag "Never" nil)
                 (set :tag "Contexts"
                      (const :tag "Block (below)" block)
                      (const :tag "Inline (beside)" inline)))
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-open-contexts '(block inline)
  "Fragment contexts where live previews are shown immediately on cursor entry.
This option only matters for contexts also enabled by
`live-tex-preview-display-live'."
  :type '(set :tag "Contexts"
              (const :tag "Block (below)" block)
              (const :tag "Inline (beside)" inline))
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-block-display 'posframe
  "How to display live previews for block fragments.
`posframe' shows them in a child-frame popup and is the default because it does
not change buffer line layout.  `buffer' uses the Org-style overlay
`after-string' path; keep it for debugging only, because large block display
strings can make vertical cursor motion hang in LaTeX buffers.  nil disables
block live previews even when `live-tex-preview-display-live' includes
`block'."
  :type '(choice (const :tag "Child-frame popup" posframe)
                 (const :tag "In-buffer after-string" buffer)
                 (const :tag "Disabled" nil))
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-popup-buffer " *live-tex-preview-block*"
  "Buffer name used for block live-preview popups."
  :type 'string
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-popup-border-width 1
  "Border width in pixels for block live-preview popups."
  :type 'integer
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-popup-internal-border-width 8
  "Internal border width in pixels for block live-preview popups."
  :type 'integer
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-popup-frame-margin 24
  "Pixels kept free around block live-preview popups."
  :type 'integer
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-popup-position 'above
  "Preferred position for block live-preview popups.
`above' uses posframe's upward point handler, which falls back below point when
there is not enough room above.  `below' uses the ordinary below-point handler."
  :type '(choice (const :tag "Above point, fallback below" above)
                 (const :tag "Below point, fallback above" below))
  :group 'live-tex-preview)

(defcustom live-tex-preview-live-clear-before-commands
  '(next-line previous-line
    evil-next-line evil-previous-line evil-line-move
    evil-next-visual-line evil-previous-visual-line
    evil-forward-char evil-backward-char
    scroll-up-command scroll-down-command
    evil-scroll-line-up evil-scroll-line-down
    evil-scroll-page-up evil-scroll-page-down)
  "Commands before which live preview display strings are cleared.
The source overlay remains open, but the extra live preview image is removed
before visual motion computes the next point position.  It is recreated
after the command if point is still inside a preview."
  :type '(repeat symbol)
  :group 'live-tex-preview)

(defcustom live-tex-preview-update-delay 0.5
  "Idle seconds after the last edit before the live preview regenerates."
  :type 'number
  :group 'live-tex-preview)

(defvar live-tex-preview-update-throttle 1.0
  "Minimum seconds between successive live preview regenerations.
This is a fixed throttle, preserving the former local implementation.")

(defcustom live-tex-preview-ignored-commands
  '(live-tex-preview-buffer live-tex-preview-region live-tex-preview-at-point)
  "Commands after which a preview overlay at point should not auto-open.
For example, running `live-tex-preview-buffer' while the cursor sits on
an equation should render it, not immediately reveal its source."
  :type '(repeat symbol)
  :group 'live-tex-preview)

(defun live-tex-preview--graphical-frame-p ()
  "Return non-nil when previews are allowed in the selected frame."
  (and (not noninteractive)
       (display-graphic-p (selected-frame))))

(defun live-tex-preview--terminal-message (action)
  "Report that ACTION cannot run in a terminal frame."
  (format "live-tex-preview: %s requires a graphical Emacs frame; previews are disabled in terminal frames"
          action))

(defun live-tex-preview--ensure-graphical (action &optional signal)
  "Return non-nil when ACTION may use preview UI in the selected frame.
When SIGNAL is non-nil, raise `user-error' on terminal frames; otherwise print a
message and return nil."
  (if (live-tex-preview--graphical-frame-p)
      t
    (let ((message (live-tex-preview--terminal-message action)))
      (if signal
          (user-error "%s" message)
        (message "%s" message))
      nil)))

(defun live-tex-preview--overlay-near-visible-window-p (ov)
  "Return non-nil when OV is near a visible window for its buffer."
  (or (not live-tex-preview-lazy-display)
      (let ((buffer (overlay-buffer ov))
            (start (overlay-start ov))
            (end (overlay-end ov))
            visible)
        (when buffer
          (walk-windows
           (lambda (window)
             (when (eq (window-buffer window) buffer)
               (let ((win-start (max (point-min)
                                     (- (window-start window)
                                        live-tex-preview-lazy-display-margin)))
                     (win-end (min (point-max)
                                   (+ (window-end window t)
                                      live-tex-preview-lazy-display-margin))))
                 (when (and (< start win-end) (> end win-start))
                   (setq visible t)))))
           nil t))
        visible)))

(defun live-tex-preview--sync-overlay-display (ov)
  "Show or hide preview image OV according to visible-window proximity."
  (when (and live-tex-preview-lazy-display
             (overlay-buffer ov)
             (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
             (not (overlay-get ov 'live-tex-preview-view-text)))
    (if (live-tex-preview--overlay-near-visible-window-p ov)
        (when-let ((image (overlay-get ov 'live-tex-preview-image)))
          (overlay-put ov 'display image)
          (overlay-put ov 'face (overlay-get ov 'live-tex-preview-hidden-face)))
      (overlay-put ov 'display nil)
      (overlay-put ov 'face nil))))

(defun live-tex-preview--refresh-visible-overlays ()
  "Refresh active image display for previews in the current buffer."
  (when live-tex-preview-lazy-display
    (dolist (ov (overlays-in (point-min) (point-max)))
      (live-tex-preview--sync-overlay-display ov))))

(defvar-local live-tex-preview-mode--from-overlay nil
  "Whether the cursor started the current command within a preview overlay.")
(defvar-local live-tex-preview-mode--marker nil
  "Marker tracking the previous cursor position.")

(defsubst live-tex-preview-mode--move-into (ov)
  "Adjust column when moving into overlay OV from below."
  (when (and (markerp live-tex-preview-mode--marker)
             (marker-position live-tex-preview-mode--marker)
             (> (marker-position live-tex-preview-mode--marker)
                (line-end-position)))
    (goto-char (overlay-end ov))
    (goto-char (max (line-beginning-position) (overlay-start ov)))))

(defvar-local live-tex-preview-fragment-function nil
  "Function of START returning the frontend's (BEG END LATEX) entry.")
(defvar-local live-tex-preview-render-function nil
  "Function accepting entries to render using the frontend's settings.")
(defvar-local live-tex-preview-block-function nil
  "Function of LATEX returning non-nil for a block fragment.")

(defun live-tex-preview-mode--fragment-for-overlay (ov)
  "Return the frontend fragment exactly bracketed by OV."
  (when (and (overlay-buffer ov) live-tex-preview-fragment-function)
    (when-let ((entry (funcall live-tex-preview-fragment-function (overlay-start ov))))
      (and (= (car entry) (overlay-start ov))
           (= (cadr entry) (overlay-end ov)) entry))))

(defun live-tex-preview-mode--regenerate-overlay (ov)
  "Regenerate OV through its frontend, or discard invalid boundaries."
  (when (overlay-buffer ov)
    (with-current-buffer (overlay-buffer ov)
      (if-let ((entry (live-tex-preview-mode--fragment-for-overlay ov)))
          (funcall live-tex-preview-render-function (list entry))
        (live-tex-preview-live--clearout ov)
        (delete-overlay ov)))))

(defun live-tex-preview-mode--open-this-overlay ()
  "Reveal the raw LaTeX of the preview overlay at point."
  (dolist (ov (overlays-at (point)))
    (when (and (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
               (not (memq this-command live-tex-preview-ignored-commands)))
      (overlay-put ov 'display nil)
      (overlay-put ov 'live-tex-preview-view-text t)
      (when-let ((f (overlay-get ov 'face)))
        (overlay-put ov 'live-tex-preview-hidden-face f)
        (overlay-put ov 'face nil))
      (live-tex-preview-mode--move-into ov)
      (setq live-tex-preview-mode--from-overlay nil)
      (run-hook-with-args 'live-tex-preview-overlay-open-functions ov))))

(defun live-tex-preview-mode--close-previous-overlay ()
  "Restore the image of the overlay at the previous cursor position.
Recompile first if the fragment was edited while open."
  (dolist (ov (overlays-at (marker-position live-tex-preview-mode--marker)))
    (when (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
      (overlay-put ov 'live-tex-preview-view-text nil)
      (if (eq (overlay-get ov 'live-tex-preview-state) 'modified)
          ;; Brief timer so Emacs can process queued input first; mirrors
          ;; the Org implementation.
          (live-tex-preview--schedule 0.01 #'live-tex-preview-mode--regenerate-overlay ov)
        (when-let ((f (overlay-get ov 'live-tex-preview-hidden-face)))
          (unless (eq f 'live-tex-preview-processing-face)
            (overlay-put ov 'face f))
          (overlay-put ov 'live-tex-preview-hidden-face nil))
        (overlay-put ov 'display (overlay-get ov 'live-tex-preview-image)))
      (run-hook-with-args 'live-tex-preview-overlay-close-functions ov))))

(defun live-tex-preview-mode--handle-pre-cursor ()
  "Record cursor state relative to preview overlays before a command.
Intended for `pre-command-hook'."
  (setq live-tex-preview-mode--from-overlay
        (eq (get-char-property (point) 'live-tex-preview-type) 'live-tex-preview-overlay))
  (set-marker live-tex-preview-mode--marker (point)))

(defun live-tex-preview-mode--handle-post-cursor ()
  "Open or close preview overlays based on cursor movement.
Intended for `post-command-hook'."
  (let ((into (eq (get-char-property (point) 'live-tex-preview-type)
                  'live-tex-preview-overlay)))
    (cond
     ((and into live-tex-preview-mode--from-overlay)
      (unless (or (get-char-property (point) 'live-tex-preview-view-text)     ;same overlay
                  (= (point) live-tex-preview-mode--marker) ;didn't move
                  (get-char-property (point) 'invisible))
        (live-tex-preview-mode--close-previous-overlay)
        (live-tex-preview-mode--open-this-overlay)))
     ((and into (not live-tex-preview-mode--from-overlay))
      (unless (get-char-property (point) 'invisible)
        (live-tex-preview-mode--open-this-overlay)))
     (live-tex-preview-mode--from-overlay
      (live-tex-preview-mode--close-previous-overlay)))
    (set-marker live-tex-preview-mode--marker (point))))

;;; Live preview while editing
;;
;; While the cursor sits in an opened fragment, show a continuously-updating
;; preview image: below the source for display math, beside it for inline math.
;; Driven by the engine's overlay open/close/update hooks plus a debounced and
;; throttled `after-change-functions' generator.  This is a port of Org's
;; `live-tex-preview-live--*' code, with its two `org-element' calls replaced
;; by `live-tex-preview--block-fragment-p' (inline vs block) and our
;; `live-tex-preview--fragment-at'-based regeneration.

(defvar-local live-tex-preview-live--docstring " "
  "String holding the live preview image as a text property.")
(defvar-local live-tex-preview-live--block-p 'unset
  "Cached block/inline classification of the fragment being live-previewed.")
(defvar-local live-tex-preview-live--display-range '(0 . 1)
  "Cons cell giving the text-property range for the live preview image.")
(defvar-local live-tex-preview-live--generator nil
  "Debounced/throttled `after-change-functions' entry for live previews.")
(defvar-local live-tex-preview-live--popup-overlay nil
  "Currently displayed block live-preview popup overlay.")
(defvar-local live-tex-preview-live--popup-warned nil
  "Whether this buffer has already warned that block popups are unavailable.")

(defun live-tex-preview-live--debounce (func duration)
  "Return FUNC debounced by DURATION seconds."
  (let (timer)
    (lambda (&rest args)
      (if (timerp timer)
          (timer-set-time timer (+ (float-time) duration))
        (setq timer (live-tex-preview--schedule
                     duration
                     (lambda ()
                       (cancel-timer timer)
                       (setq timer nil)
                       (apply func args))))))))

(defun live-tex-preview-live--throttle (func)
  "Return FUNC throttled to `live-tex-preview-update-throttle' seconds."
  (let (waiting)
    (lambda (&rest args)
      (unless waiting
        (apply func args)
        (setq waiting t)
        (live-tex-preview--schedule live-tex-preview-update-throttle
                                    (lambda ()
                                      (setq waiting nil)
                                      (apply func args)))))))

(defun live-tex-preview-live--update-props (image-spec &optional box-face)
  "Set the live preview docstring's trailing char to display IMAGE-SPEC.
BOX-FACE, if given, is applied as a face on the same character."
  (let ((beg (car live-tex-preview-live--display-range))
        (end (cdr live-tex-preview-live--display-range)))
    (put-text-property beg end 'display image-spec live-tex-preview-live--docstring)
    (when box-face
      (put-text-property beg end 'face box-face live-tex-preview-live--docstring))))

(defun live-tex-preview-live--clearout (ov)
  "Remove the live preview attached to overlay OV."
  (setq live-tex-preview-live--block-p 'unset)
  (overlay-put ov 'live-tex-preview-live-edited nil)
  (overlay-put ov 'before-string nil)
  (overlay-put ov 'after-string nil)
  (live-tex-preview-live--hide-popup ov))

(defun live-tex-preview-live--clear-before-motion ()
  "Clear live preview display strings before visual-motion commands."
  (when (memq this-command live-tex-preview-live-clear-before-commands)
    (dolist (ov (cl-delete-duplicates
                 (append
                  (overlays-at (point))
                  (when (and (markerp live-tex-preview-mode--marker)
                             (marker-position live-tex-preview-mode--marker))
                    (overlays-at (marker-position live-tex-preview-mode--marker))))
                 :test #'eq))
      (when (and (eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay)
                 (overlay-get ov 'live-tex-preview-view-text)
                 (or (overlay-get ov 'before-string)
                     (overlay-get ov 'after-string)))
        (live-tex-preview-live--clearout ov)))))

(defun live-tex-preview-live--install-docstring (ov block-p)
  "Install the live preview string on OV for a block or inline fragment."
  (overlay-put ov 'before-string nil)
  (setq live-tex-preview-live--docstring
        (concat (and block-p "\n​") " ")
        live-tex-preview-live--display-range
        (cons (1- (length live-tex-preview-live--docstring))
              (length live-tex-preview-live--docstring)))
  (overlay-put ov 'after-string live-tex-preview-live--docstring))

(defun live-tex-preview-live--face-color (face attribute fallback)
  "Return FACE ATTRIBUTE, or FALLBACK when it is unspecified."
  (let ((color (face-attribute face attribute nil t)))
    (if (or (null color) (eq color 'unspecified))
        fallback
      color)))

(defun live-tex-preview-live--source-window ()
  "Return the window where the current buffer's preview should be anchored."
  (cond
   ((eq (window-buffer (selected-window)) (current-buffer)) (selected-window))
   ((get-buffer-window (current-buffer) t))))

(defun live-tex-preview-live--popup-workable-p ()
  "Return non-nil when a block live-preview popup can be shown."
  (and (eq live-tex-preview-live-block-display 'posframe)
       (live-tex-preview--graphical-frame-p)
       (require 'posframe nil t)
       (posframe-workable-p)))

(defun live-tex-preview-live--popup-string (image-spec)
  "Return a string displaying IMAGE-SPEC for a block live-preview popup."
  (let ((string (copy-sequence " ")))
    (put-text-property 0 1 'display image-spec string)
    (put-text-property 0 1 'face '(:box t) string)
    string))

(defun live-tex-preview-live--image-pixel-size (image-spec)
  "Return IMAGE-SPEC size in pixels, or nil if Emacs cannot compute it."
  (condition-case nil
      (image-size image-spec t)
    (error nil)))

(defun live-tex-preview-live--popup-max-image-size (window)
  "Return the maximum image pixel size usable for a popup in WINDOW."
  (let* ((frame (window-frame window))
         (border-pixels (+ (* 2 live-tex-preview-live-popup-border-width)
                           (* 2 live-tex-preview-live-popup-internal-border-width)))
         (margin live-tex-preview-live-popup-frame-margin))
    (cons (max 1 (- (frame-pixel-width frame) border-pixels margin))
          (max 1 (- (frame-pixel-height frame)
                    border-pixels margin
                    (* 3 (default-line-height)))))))

(defun live-tex-preview-live--scale-image-to-fit (image-spec max-size)
  "Return IMAGE-SPEC scaled down to fit MAX-SIZE pixels when needed."
  (if-let* ((size (live-tex-preview-live--image-pixel-size image-spec))
            (width (car size))
            (height (cdr size))
            ((> width 0))
            ((> height 0))
            (scale (min 1.0
                        (/ (float (car max-size)) width)
                        (/ (float (cdr max-size)) height)))
            ((< scale 1.0)))
      (let* ((plist (copy-sequence (cdr image-spec)))
             (old-height (plist-get plist :height))
             (new-height
              (cond
               ((and (consp old-height) (numberp (car old-height)))
                (cons (* scale (car old-height)) (cdr old-height)))
               ((numberp old-height)
                (max 1 (round (* scale old-height))))
               (t nil))))
        (if new-height
            (cons (car image-spec) (plist-put plist :height new-height))
          (cons (car image-spec)
                (plist-put plist :width (max 1 (floor (* scale width)))))))
    image-spec))

(defun live-tex-preview-live--popup-frame-size (image-spec)
  "Return a cons of popup frame width/height in character units for IMAGE-SPEC."
  (let* ((size (or (live-tex-preview-live--image-pixel-size image-spec)
                   (cons (default-font-width) (default-line-height))))
         (width-chars (ceiling (/ (+ (float (car size)) (default-font-width))
                                  (max 1 (default-font-width)))))
         (height-chars (ceiling (/ (+ (float (cdr size)) (default-line-height))
                                   (max 1 (default-line-height))))))
    (cons (max 2 width-chars)
          (max 2 height-chars))))

(defun live-tex-preview-live--popup-poshandler ()
  "Return the posframe position handler for block live previews."
  (if (eq live-tex-preview-live-popup-position 'above)
      #'posframe-poshandler-point-bottom-left-corner-upward
    #'posframe-poshandler-point-bottom-left-corner))

(defun live-tex-preview-live--hide-popup (&optional ov)
  "Hide the block live-preview popup.
When OV is non-nil, hide only if OV owns the current popup."
  (when (or (null ov)
            (eq ov live-tex-preview-live--popup-overlay))
    (setq live-tex-preview-live--popup-overlay nil)
    (when (fboundp 'posframe-hide)
      (posframe-hide live-tex-preview-live-popup-buffer))))

(defun live-tex-preview-live--show-popup (ov image-spec)
  "Show IMAGE-SPEC for block overlay OV in a child-frame popup."
  (if (not (live-tex-preview-live--popup-workable-p))
      (unless live-tex-preview-live--popup-warned
        (setq live-tex-preview-live--popup-warned t)
        (message "live-tex-preview: block live previews need posframe in a graphical frame"))
    (when-let ((window (live-tex-preview-live--source-window)))
      (let* ((fitted-image
              (live-tex-preview-live--scale-image-to-fit
               image-spec (live-tex-preview-live--popup-max-image-size window)))
             (frame-size (live-tex-preview-live--popup-frame-size fitted-image))
             (max-width (max 2 (- (frame-width (window-frame window)) 2)))
             (max-height (max 2 (- (frame-height (window-frame window)) 2)))
             (width (min max-width (car frame-size)))
             (height (min max-height (cdr frame-size))))
        (setq live-tex-preview-live--popup-overlay ov)
        (with-selected-window window
          (posframe-show
           live-tex-preview-live-popup-buffer
           :string (live-tex-preview-live--popup-string fitted-image)
           :position (point)
           :poshandler (live-tex-preview-live--popup-poshandler)
           :width width
           :height height
           :min-width width
           :min-height height
           :max-width max-width
           :max-height max-height
           :lines-truncate t
           :accept-focus nil
           :hidehandler #'posframe-hidehandler-when-buffer-switch
           :border-width live-tex-preview-live-popup-border-width
           :border-color (live-tex-preview-live--face-color
                          'shadow :foreground
                          (live-tex-preview-live--face-color
                           'mode-line-inactive :background
                           (face-foreground 'default nil t)))
           :internal-border-width live-tex-preview-live-popup-internal-border-width
           ;; `tooltip' is commonly left at toolkit defaults (for example,
           ;; light yellow with black text), even when the active theme is
           ;; dark.  Match the source buffer instead.
           :background-color (face-background 'default nil t)
           :foreground-color (face-foreground 'default nil t)))))))

(defun live-tex-preview-live--context-enabled-p (context contexts)
  "Return non-nil when CONTEXT is enabled by CONTEXTS.
CONTEXT is either `block' or `inline'.  CONTEXTS may be t, nil, or a list."
  (or (eq contexts t)
      (and (consp contexts)
           (memq context contexts))))

(defun live-tex-preview-live--ensure-overlay (&optional ov reason)
  "Attach a live preview image to open overlay OV (or the one at point).
REASON is `open' when point merely entered the overlay and `update' when an
edited fragment was regenerated."
  (when-let* ((ov (or ov (cdr (get-char-property-and-overlay
                               (point) 'live-tex-preview-type))))
              ((eq (overlay-get ov 'live-tex-preview-type) 'live-tex-preview-overlay))
              (image (overlay-get ov 'live-tex-preview-image))
              (end (overlay-end ov)))
    (if (and (eq (overlay-get ov 'live-tex-preview-state) 'modified)
             (not (live-tex-preview-mode--fragment-for-overlay ov)))
        (progn
          (live-tex-preview-live--clearout ov)
          (delete-overlay ov))
      (let ((block-p (if (eq live-tex-preview-live--block-p 'unset)
                         (setq live-tex-preview-live--block-p
                               (funcall live-tex-preview-block-function
                                (buffer-substring-no-properties
                                 (overlay-start ov) end)))
                       live-tex-preview-live--block-p)))
        (let ((context (if block-p 'block 'inline)))
          (when (and (live-tex-preview-live--context-enabled-p
                      context live-tex-preview-display-live)
                     (or (eq reason 'update)
                         (overlay-get ov 'live-tex-preview-live-edited)
                         (live-tex-preview-live--context-enabled-p
                          context live-tex-preview-live-open-contexts)))
            (cond
             ((and block-p (eq live-tex-preview-live-block-display 'posframe))
              (overlay-put ov 'live-tex-preview-view-text t)
              (overlay-put ov 'before-string nil)
              (overlay-put ov 'after-string nil)
              (live-tex-preview-live--show-popup ov image))
             ((or (not block-p)
                  (eq live-tex-preview-live-block-display 'buffer))
              (unless (or (overlay-get ov 'before-string)
                          (overlay-get ov 'after-string))
                (overlay-put ov 'live-tex-preview-view-text t)
                (live-tex-preview-live--install-docstring ov block-p))
              (live-tex-preview-live--update-props image '(:box t))))))))))

(defun live-tex-preview-live--ensure-open-overlay (ov)
  "Attach a live preview to OV when cursor-entry policy allows it."
  (live-tex-preview-live--ensure-overlay ov 'open))

(defun live-tex-preview-live--update-overlay (ov)
  "Refresh the live preview image for overlay OV after a regeneration."
  (when (and (memq ov (overlays-at (point)))
             (overlay-get ov 'live-tex-preview-view-text))
    (if (overlay-get ov 'after-string)
        (live-tex-preview-live--update-props (overlay-get ov 'live-tex-preview-image))
      (if (overlay-get ov 'before-string)
          (live-tex-preview-live--update-props (overlay-get ov 'live-tex-preview-image))
        (live-tex-preview-live--ensure-overlay ov 'update)))))

(defun live-tex-preview-live--refresh-popup ()
  "Refresh live-preview display after commands.
The pre-command motion guard removes live display strings before cursor movement
so `line-move' never computes through them.  If point remains inside an opened
fragment, recreate the inline live string or reposition the block popup here."
  (pcase-let ((`(,type . ,ov) (get-char-property-and-overlay
                               (point) 'live-tex-preview-type)))
    (if (and ov
             (eq type 'live-tex-preview-overlay)
             (overlay-get ov 'live-tex-preview-view-text)
             (overlay-get ov 'live-tex-preview-image))
        (progn
          (when (and live-tex-preview-live--popup-overlay
                     (not (eq ov live-tex-preview-live--popup-overlay)))
            (live-tex-preview-live--hide-popup))
          (live-tex-preview-live--ensure-overlay ov 'open))
      (live-tex-preview-live--hide-popup))))

(defun live-tex-preview-live--regenerate (&rest _)
  "Regenerate the live preview for the fragment at point.
Run (debounced and throttled) from `after-change-functions'."
  (pcase-let ((`(,type . ,ov) (get-char-property-and-overlay
                               (point) 'live-tex-preview-type)))
    (when (and ov (eq type 'live-tex-preview-overlay)
               (overlay-get ov 'live-tex-preview-state))
      (overlay-put ov 'live-tex-preview-live-edited t)
      (live-tex-preview-mode--regenerate-overlay ov)
      (unless (and (overlay-buffer ov) (overlay-get ov 'live-tex-preview-image))
        (live-tex-preview-live--clearout ov)))))

(defun live-tex-preview-live--setup ()
  "Enable live-preview hooks in the current buffer."
  (setq live-tex-preview-live--docstring " "
        live-tex-preview-live--block-p 'unset)
  (setq-local live-tex-preview-live--generator
              (thread-first #'live-tex-preview-live--regenerate
                            (live-tex-preview-live--throttle)
                            (live-tex-preview-live--debounce
                             live-tex-preview-update-delay)))
  (add-hook 'live-tex-preview-overlay-open-functions
            #'live-tex-preview-live--ensure-open-overlay nil 'local)
  (add-hook 'live-tex-preview-overlay-close-functions
            #'live-tex-preview-live--clearout nil 'local)
  (add-hook 'live-tex-preview-overlay-update-functions
            #'live-tex-preview-live--update-overlay nil 'local)
  (add-hook 'live-tex-preview-overlay-update-functions
            #'live-tex-preview--sync-overlay-display 90 'local)
  (add-hook 'pre-command-hook
            #'live-tex-preview-live--clear-before-motion -90 'local)
  (add-hook 'post-command-hook
            #'live-tex-preview-live--refresh-popup 95 'local)
  (add-hook 'after-change-functions
            live-tex-preview-live--generator 90 'local))

(defun live-tex-preview-live--teardown ()
  "Disable live-preview hooks in the current buffer."
  (when-let* ((props (get-char-property-and-overlay (point) 'live-tex-preview-type))
              ((eq (car props) 'live-tex-preview-overlay))
              (ov (cdr props)))
    (live-tex-preview-live--clearout ov))
  (remove-hook 'live-tex-preview-overlay-open-functions
               #'live-tex-preview-live--ensure-open-overlay 'local)
  ;; Remove the pre-2026-07 hook name from already-open buffers when this file is
  ;; reloaded into a long-lived daemon.
  (remove-hook 'live-tex-preview-overlay-open-functions
               #'live-tex-preview-live--ensure-overlay 'local)
  (remove-hook 'live-tex-preview-overlay-close-functions
               #'live-tex-preview-live--clearout 'local)
  (remove-hook 'live-tex-preview-overlay-update-functions
               #'live-tex-preview-live--update-overlay 'local)
  (remove-hook 'live-tex-preview-overlay-update-functions
               #'live-tex-preview--sync-overlay-display 'local)
  (remove-hook 'pre-command-hook
               #'live-tex-preview-live--clear-before-motion 'local)
  (remove-hook 'post-command-hook
               #'live-tex-preview-live--refresh-popup 'local)
  (when live-tex-preview-live--generator
    (remove-hook 'after-change-functions live-tex-preview-live--generator 'local))
  (live-tex-preview-live--hide-popup)
  (setq-local live-tex-preview-live--generator nil))


(provide 'live-tex-preview)
;;; live-tex-preview.el ends here
