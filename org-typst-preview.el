;;; org-typst-preview.el --- Live Typst previews in Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ad

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2") (org "9.8"))
;; Keywords: outlines, tools
;; Homepage: https://github.com/skissue/org-typst

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Enable `org-typst-preview-mode' in a trusted local Org buffer to preview
;; Typst export snippets and blocks as you edit.  Requires graphical Emacs
;; with SVG support and the `typst' executable.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-src)
(require 'cl-lib)
(require 'face-remap)
(require 'image)

(defvar org-typst-preview-mode)
(cl-defstruct org-typst-preview--session overlays tick observed)
(defvar org-typst-preview--sessions (make-hash-table :test #'eq)
  "Sessions keyed by the actual enabled buffer, never by copyable locals.")
(defvar org-typst-preview--timer nil
  "Shared timer that checks for dead buffers and stable character ticks.")

(defun org-typst-preview--display (ov)
  "Update overlay OV to show its image, source, or diagnostic.
Reveal source at point and on error.  In an open block or edit buffer,
show the available preview below the source."
  (let* ((kind (overlay-get ov 'kind)) (img (overlay-get ov 'image))
         (err (overlay-get ov 'error))
         (open (or (eq kind 'edit) (<= (overlay-start ov) (point) (1- (overlay-end ov))))))
    (overlay-put ov 'display (and (not open) (not err) img))
    (overlay-put ov 'help-echo err)
    (overlay-put ov 'after-string
                 (cond (err (concat (if (eq kind 'inline) " " "\n")
                                    (propertize err 'face 'error)))
                       ((and open img (not (eq kind 'inline)))
                        (concat "\n" (propertize " " 'display img)))))))

(defun org-typst-preview--cursor ()
  "Update preview visibility for the current position of point."
  (when-let* ((s (gethash (current-buffer) org-typst-preview--sessions)))
    (dolist (ov (org-typst-preview--session-overlays s))
      (when (overlay-buffer ov) (org-typst-preview--display ov)))))

(defun org-typst-preview--ascent (svg)
  "Return the baseline in SVG as a percentage of its height, or nil.
Typst 0.15 positions the labeled baseline box in page coordinates."
  (let ((h (and (string-match "\\`<svg[^>]* height=\"\\([0-9.]+\\)pt\"" svg)
                (string-to-number (match-string 1 svg))))
        (y (and (string-match "<g transform=\"translate([^ )]+ \\([0-9.]+\\))\" data-typst-label=\"org-typst-preview-baseline\"/>" svg)
                (string-to-number (match-string 1 svg)))))
    (when (and h y (> h 0) (<= 0 y h)) (round (* 100.0 y) h))))

(defun org-typst-preview--resize (ov)
  "Resize the cached image for overlay OV to match the buffer's text scale."
  (when-let* ((base (overlay-get ov 'base-image)))
    (let ((img (copy-sequence base)))
      (when text-scale-mode
        (setf (image-property img :scale) 1.0
              (image-property img :height)
              (max 1 (round (* (cdr (image-size base t))
                               (expt text-scale-mode-step text-scale-mode-amount))))))
      (overlay-put ov 'image img)))
  (org-typst-preview--display ov))

(defun org-typst-preview--zoom ()
  "Resize all previews in the current buffer without recompiling them."
  (when-let* ((s (gethash (current-buffer) org-typst-preview--sessions)))
    (dolist (ov (org-typst-preview--session-overlays s))
      (when (overlay-buffer ov) (org-typst-preview--resize ov)))))

(defun org-typst-preview--remove (ov)
  "Detach OV, cancel its compiler and remove it from display."
  (when-let* ((proc (overlay-get ov 'job)))
    ;; `delete-process' can invoke the sentinel synchronously.
    (overlay-put ov 'job nil)
    (when (process-live-p proc) (delete-process proc)))
  (delete-overlay ov))

(defun org-typst-preview--changed (beg end old-length)
  "Hide directly edited previews without parsing Org or visiting other views."
  (when-let* ((s (gethash (current-buffer) org-typst-preview--sessions))
              ((not (eql (org-typst-preview--session-tick s) (buffer-chars-modified-tick)))))
    (if (org-src-edit-buffer-p)
        ;; The entire buffer is Typst input, regardless of where its markers moved.
        (mapc #'org-typst-preview--remove (org-typst-preview--session-overlays s))
      ;; Overlay lookup can cross a restriction: include empty overlays at its edge.
      (dolist (ov (overlays-in (max 1 (1- beg)) (min (1+ (buffer-size)) (1+ end))))
        (when (eq (overlay-get ov 'category) 'org-typst-preview)
          (let* ((from (overlay-start ov)) (to (overlay-end ov)) (source (overlay-get ov 'source))
                 (lo (max from beg (point-min))) (hi (min to end (point-max))))
            (cond
             ;; `insert-before-markers' at the old end didn't change the fragment.
             ((and (zerop old-length) (= end to) (= (- beg from) (length source)))
              (move-overlay ov from beg))
             ((or (/= (- to from) (length source))
                  (and (< lo hi)
                       (not (equal (substring source (- lo from) (- hi from))
                                   (buffer-substring-no-properties lo hi)))))
              (org-typst-preview--remove ov)))))))))

(defun org-typst-preview--diagnostic (err)
  "Read Typst's diagnostic from ERR, adjusting for the two preamble lines."
  (with-current-buffer err
    (goto-char (point-min))
    (if (re-search-forward "<stdin>:\\([0-9]+\\):\\([0-9]+\\): error: \\(.*\\)" nil t)
        (format "Body %d:%s: %s" (- (string-to-number (match-string 1)) 2)
                (match-string 2) (match-string 3))
      (buffer-substring (point-min) (line-end-position)))))

(defun org-typst-preview--accept (ov)
  "Publish a completed result for OV after validating its membership."
  (when-let* ((result (overlay-get ov 'ready)))
    (overlay-put ov 'ready nil)
    (overlay-put ov (if (stringp result) 'error 'base-image) result)
    (org-typst-preview--resize ov)))

(defun org-typst-preview--finish (proc ov measure out err)
  "Stage PROC's result if OV still owns it, then release OUT and ERR.
MEASURE is non-nil for inline equations with a baseline marker."
  (unwind-protect
      (when (eq proc (overlay-get ov 'job))
        (overlay-put ov 'job nil)
        (when-let* ((buf (overlay-buffer ov)) (s (gethash buf org-typst-preview--sessions)))
          (with-current-buffer buf
            (overlay-put ov 'ready
                         (if (zerop (process-exit-status proc))
                             (let ((svg (with-current-buffer out (buffer-string))))
                               (create-image svg 'svg t :ascent
                                             (or (and measure (org-typst-preview--ascent svg)) 78)))
                           (org-typst-preview--diagnostic err)))
            ;; Matching our bytes isn't enough: an edit elsewhere could have
            ;; turned the fragment into a comment.  Wait for a fresh parse.
            (when (eql (org-typst-preview--session-tick s) (buffer-chars-modified-tick))
              (org-typst-preview--accept ov)))))
    (kill-buffer out)
    (kill-buffer err)))

(defun org-typst-preview--render (ov body)
  "Compile Typst BODY asynchronously for overlay OV."
  ;; Only append markup to a self-contained inline equation, never to
  ;; arbitrary code, trailing comments, or a display equation ($ … $).
  (let* ((measure (and (eq (overlay-get ov 'kind) 'inline)
                       (string-match-p "\\`\\$[^$[:space:]]\\(?:[^$\n]*[^$[:space:]]\\)?\\$\\'" body)))
         (fg (apply #'format "%02x%02x%02x" (mapcar (lambda (x) (/ x 256))
                         (color-values (face-foreground 'default nil t)))))
         (head (format "#set page(width:auto,height:auto,margin:0pt,fill:none)\n#set text(size:%spt,fill:rgb(\"%s\"),top-edge:\"bounds\",bottom-edge:\"bounds\")\n"
                       (/ (face-attribute 'default :height) 10.0) fg))
         (out (generate-new-buffer " *typst-svg*"))
         (err (generate-new-buffer " *typst-stderr*"))
         proc sent)
    (unwind-protect
        (progn
          (setq proc (make-process
                      :name "org-typst" :buffer out :stderr err :noquery t :connection-type 'pipe
                      :coding 'utf-8-unix :command (list "typst" "compile" "--format" "svg" "--root"
                                                        (expand-file-name default-directory)
                                                        "--diagnostic-format" "short" "-" "-")
                      :sentinel (lambda (p _event)
                                  (when (memq (process-status p) '(exit signal))
                                    (org-typst-preview--finish p ov measure out err)))))
          (overlay-put ov 'job proc)
          (process-send-string proc (concat head body (when measure "#box(width:0pt,height:0pt)[]<org-typst-preview-baseline>")))
          (process-send-eof proc)
          (setq sent t))
      (unless sent
        (org-typst-preview--remove ov)
        (kill-buffer out)
        (kill-buffer err)))))

(defun org-typst-preview--snapshot ()
  "Return (BEG END KIND BODY) for each fragment in the widened buffer."
  (if (org-src-edit-buffer-p)
      (list (list (point-min) (point-max) 'edit (buffer-substring-no-properties (point-min) (point-max))))
    (org-element-map (org-element-parse-buffer) '(export-snippet export-block)
      (lambda (el)
        (let ((end (org-element-property :end el)) (body (org-element-property :value el)))
          (cond ((equal (org-element-property :back-end el) "typst")
                 (list (org-element-property :begin el) (- end (org-element-property :post-blank el)) 'inline body))
                ((equal (org-element-property :type el) "TYPST")
                 (list (org-element-property :post-affiliated el)
                       (save-excursion (goto-char end) (skip-chars-backward " \t\n") (point))
                       'block (org-remove-indentation body)))))))))

(defun org-typst-preview--refresh ()
  "Reconcile the current session's work against one whole-document snapshot."
  (save-restriction
    (widen)
    ;; A labeled restriction can survive `widen'.  Leave the tick dirty for the next poll.
    (unless (buffer-narrowed-p)
      (let* ((s (gethash (current-buffer) org-typst-preview--sessions))
             (tick (buffer-chars-modified-tick)) (snapshot (org-typst-preview--snapshot))
             (old (make-hash-table :test #'equal)) found renders)
        (dolist (ov (org-typst-preview--session-overlays s))
          (when (overlay-buffer ov)
            (push ov (gethash (list (overlay-start ov) (overlay-end ov)
                                    (overlay-get ov 'kind) (overlay-get ov 'source)) old))))
        (pcase-dolist (`(,beg ,end ,kind ,body) snapshot)
          (let* ((raw (buffer-substring-no-properties beg end))
                 (ov (pop (gethash (list beg end kind raw) old))))
            (if ov (push ov found) (push (list beg end kind raw body) renders))))
        (maphash (lambda (_ ovs) (mapc #'org-typst-preview--remove ovs)) old)
        (setf (org-typst-preview--session-overlays s) found
              (org-typst-preview--session-tick s) tick
              (org-typst-preview--session-observed s) tick)
        (mapc #'org-typst-preview--accept found)
        (pcase-dolist (`(,beg ,end ,kind ,raw ,body) (nreverse renders))
          (let ((ov (make-overlay beg end nil (not (eq kind 'edit)) (eq kind 'edit))))
            (overlay-put ov 'category 'org-typst-preview)
            (overlay-put ov 'kind kind)
            (overlay-put ov 'source raw)
            (push ov (org-typst-preview--session-overlays s))
            (org-typst-preview--render ov body)))
        (org-typst-preview--cursor)))))

(defun org-typst-preview--teardown (&optional buffer)
  "Release the session of BUFFER, which may already be dead."
  (let ((buf (or buffer (current-buffer))))
    (when-let* ((s (gethash buf org-typst-preview--sessions)))
      (remhash buf org-typst-preview--sessions)
      (mapc #'org-typst-preview--remove (org-typst-preview--session-overlays s))
      (when (zerop (hash-table-count org-typst-preview--sessions))
        (when (< emacs-major-version 29)
          (advice-remove 'make-indirect-buffer #'org-typst-preview--disable-copy))
        (cancel-timer org-typst-preview--timer)
        (setq org-typst-preview--timer nil)))))

(defun org-typst-preview--poll ()
  "Reap dead sessions; parse only ticks stable across consecutive polls."
  (maphash
   (lambda (buf s)
     (if (not (buffer-live-p buf)) (org-typst-preview--teardown buf)
       (let ((tick (buffer-chars-modified-tick buf)))
         (cond ((not (eql tick (org-typst-preview--session-observed s)))
                (setf (org-typst-preview--session-observed s) tick))
               ((not (eql tick (org-typst-preview--session-tick s)))
                (with-current-buffer buf (org-typst-preview--refresh)))))))
   org-typst-preview--sessions))

(defun org-typst-preview--typst-edit-p ()
  "Return non-nil if the current Org edit buffer edits a Typst export block."
  (and (eq (org-src-source-type) 'export-block)
       (org-src-do-at-code-block
        (equal (org-element-property :type (org-element-at-point)) "TYPST"))))

(defun org-typst-preview--disable-copy (&optional clone)
  "Discard copied preview state in CLONE or the current buffer.  Return CLONE."
  (with-current-buffer (or clone (current-buffer))
    (when (or org-typst-preview-mode (gethash (current-buffer) org-typst-preview--sessions))
      (org-typst-preview-mode -1)))
  clone)

(defconst org-typst-preview--hooks
  '((after-change-functions . org-typst-preview--changed)
    (post-command-hook . org-typst-preview--cursor)
    (text-scale-mode-hook . org-typst-preview--zoom)
    (change-major-mode-hook . org-typst-preview--teardown)
    (kill-buffer-hook . org-typst-preview--teardown)
    (clone-indirect-buffer-hook . org-typst-preview--disable-copy)))

;;;###autoload
(define-minor-mode org-typst-preview-mode
  "Preview literal Typst export fragments in this trusted local buffer."
  :lighter " Typst◉"
  (if org-typst-preview-mode
      (let (enabled)
        (unwind-protect
            (progn
              (unless (if (org-src-edit-buffer-p) (org-typst-preview--typst-edit-p) (derived-mode-p 'org-mode))
                (user-error "Need an Org buffer or a Typst export block edit buffer"))
              (unless (and (not (file-remote-p default-directory)) (display-graphic-p)
                           (image-type-available-p 'svg) (executable-find "typst"))
                (user-error "Need a local buffer, graphical SVG support and typst on exec-path"))
              (unless (gethash (current-buffer) org-typst-preview--sessions)
                (puthash (current-buffer) (make-org-typst-preview--session) org-typst-preview--sessions))
              (dolist (hook org-typst-preview--hooks) (add-hook (car hook) (cdr hook) nil t))
              (unless org-typst-preview--timer
                ;; Before Emacs 29, `make-indirect-buffer' copies overlays and
                ;; locals without running `clone-indirect-buffer-hook'.
                (when (< emacs-major-version 29)
                  (advice-add 'make-indirect-buffer :filter-return #'org-typst-preview--disable-copy))
                (setq org-typst-preview--timer (run-at-time 0.2 0.2 #'org-typst-preview--poll)))
              (org-typst-preview--refresh)
              (setq enabled t))
          (unless enabled (org-typst-preview-mode -1))))
    (dolist (hook org-typst-preview--hooks) (remove-hook (car hook) (cdr hook) t))
    (if (gethash (current-buffer) org-typst-preview--sessions)
        (org-typst-preview--teardown)
      (save-restriction (widen) (remove-overlays nil nil 'category 'org-typst-preview)))))

(defun org-typst-preview--setup-edit-buffer ()
  "Enable preview for a Typst export block if its parent has preview enabled."
  (when (org-src-edit-buffer-p)
    (if (and (org-typst-preview--typst-edit-p)
             (org-src-do-at-code-block (gethash (current-buffer) org-typst-preview--sessions)))
        (org-typst-preview-mode 1)
      ;; A footnote editor can both have run org-mode-hook and have had its
      ;; locals overwritten.  Only the registry knows whose work to release.
      (when (or org-typst-preview-mode (gethash (current-buffer) org-typst-preview--sessions))
        (org-typst-preview-mode -1)))))

;; `clone-buffer' runs the major mode, then copies the parent's locals,
;; potentially overwriting both the new mode flag and a buffer-local hook.
(add-hook 'clone-buffer-hook #'org-typst-preview--disable-copy)
(add-hook 'org-src-mode-hook #'org-typst-preview--setup-edit-buffer)

(provide 'org-typst-preview)

;;; org-typst-preview.el ends here
