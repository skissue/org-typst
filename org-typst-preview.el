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

(defvar-local org-typst-preview--overlays nil
  "Overlays tracking Typst fragments in the current buffer.")
(defvar-local org-typst-preview--timer nil
  "Most recently scheduled preview refresh timer for the current buffer.")

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
                                    (when (and img (not (eq kind 'inline)))
                                      (concat (propertize " " 'display img) "  Last good preview · "))
                                    (propertize err 'face 'error)))
                       ((and open img (not (eq kind 'inline)))
                        (concat "\n" (propertize " " 'display img)))))))

(defun org-typst-preview--cursor ()
  "Update preview visibility for the current position of point."
  (mapc #'org-typst-preview--display org-typst-preview--overlays))

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
  (mapc #'org-typst-preview--resize org-typst-preview--overlays))

(defun org-typst-preview--render (ov body raw)
  "Compile Typst BODY asynchronously for overlay OV.
RAW is the original buffer text covered by OV, including any Org markup.
Apply the result only if OV still tracks this job and its text matches RAW."
  (let* ((out (generate-new-buffer " *typst-svg*"))
         (err (generate-new-buffer " *typst-stderr*"))
         ;; Only append markup to a self-contained inline equation, never to
         ;; arbitrary code, trailing comments, or a display equation ($ … $).
         (measure (and (eq (overlay-get ov 'kind) 'inline)
                       (string-match-p "\\`\\$[^$[:space:]]\\(?:[^$\n]*[^$[:space:]]\\)?\\$\\'" body)))
         (fg (apply #'format "%02x%02x%02x" (mapcar (lambda (x) (/ x 256))
                         (color-values (face-foreground 'default nil t)))))
         (head (format "#set page(width:auto,height:auto,margin:0pt,fill:none)\n#set text(size:%spt,fill:rgb(\"%s\"),top-edge:\"bounds\",bottom-edge:\"bounds\")\n"
                       (/ (face-attribute 'default :height) 10.0) fg))
         (proc (make-process
                :name "org-typst" :buffer out :stderr err :noquery t :connection-type 'pipe
                :coding 'utf-8-unix :command (list "typst" "compile" "--format" "svg" "--root"
                                                  (expand-file-name default-directory)
                                                  "--diagnostic-format" "short" "-" "-")
                :sentinel
                (lambda (p _event)
                  (when (memq (process-status p) '(exit signal))
                    (unwind-protect
                        (when (and (overlay-buffer ov) (eq p (overlay-get ov 'job)))
                          (with-current-buffer (overlay-buffer ov)
                            (when (equal raw (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                              (if (zerop (process-exit-status p))
                                  (let ((svg (with-current-buffer out (buffer-string))))
                                    (overlay-put ov 'base-image
                                                 (create-image svg 'svg t :ascent
                                                               (or (and measure (org-typst-preview--ascent svg)) 78)))
                                    (overlay-put ov 'error nil))
                                (overlay-put ov 'error
                                             (with-current-buffer err
                                               (goto-char (point-min))
                                               (if (re-search-forward "<stdin>:\\([0-9]+\\):\\([0-9]+\\): error: \\(.*\\)" nil t)
                                                   (format "Body %d:%s: %s" (- (string-to-number (match-string 1)) 2)
                                                           (match-string 2) (match-string 3))
                                                 (buffer-substring (point-min) (line-end-position))))))
                              (org-typst-preview--resize ov))))
                      (kill-buffer out) (kill-buffer err)))))))
    (overlay-put ov 'job proc)
    (process-send-string proc (concat head body (when measure "#box(width:0pt,height:0pt)[]<org-typst-preview-baseline>")))
    (process-send-eof proc)))

(defun org-typst-preview--refresh ()
  "Update overlays for fragments in the accessible part of the buffer.
In an Org edit buffer, treat the accessible text as one Typst fragment."
  (let ((old org-typst-preview--overlays) found)
    (cl-labels ((track (beg end kind body)
                  "Track a KIND of fragment from BEG to END and render BODY if changed."
                  (let* ((ov (or (cl-find beg old :key #'overlay-start)
                                 (make-overlay beg end)))
                         (raw (buffer-substring-no-properties beg end)))
                    (move-overlay ov beg end)
                    (overlay-put ov 'kind kind)
                    (push ov found)
                    (unless (equal raw (overlay-get ov 'source))
                      (overlay-put ov 'source raw)
                      (org-typst-preview--render ov body raw)))))
      (if (org-src-edit-buffer-p)
          (track (point-min) (point-max) 'edit (buffer-substring-no-properties (point-min) (point-max)))
        (org-element-map (org-element-parse-buffer) '(export-snippet export-block)
          (lambda (el)
            (let ((end (org-element-property :end el)) (body (org-element-property :value el)))
              (cond ((equal (org-element-property :back-end el) "typst")
                     (track (org-element-property :begin el) (- end (org-element-property :post-blank el)) 'inline body))
                    ((equal (org-element-property :type el) "TYPST")
                     (track (org-element-property :post-affiliated el)
                            (save-excursion (goto-char end) (skip-chars-backward " \t\n") (point))
                            'block (org-remove-indentation body)))))))))
    (mapc #'delete-overlay (cl-set-difference old found))
    (setq org-typst-preview--overlays found)
    (org-typst-preview--cursor)))

(defun org-typst-preview--changed (&rest _)
  "Schedule a preview refresh after typing pauses.
Ignore the arguments supplied by `after-change-functions'."
  (when (timerp org-typst-preview--timer) (cancel-timer org-typst-preview--timer))
  (setq org-typst-preview--timer
        (run-at-time 0.2 nil (lambda (buf)
                               (when (buffer-live-p buf)
                                 (with-current-buffer buf (org-typst-preview--refresh)))) (current-buffer))))

;;;###autoload
(define-minor-mode org-typst-preview-mode
  "Preview literal Typst export fragments in this trusted local buffer."
  :lighter " Typst◉"
  (if org-typst-preview-mode
      (progn
        (unless (and (not (file-remote-p default-directory)) (display-graphic-p)
                     (image-type-available-p 'svg) (executable-find "typst"))
          (setq org-typst-preview-mode nil)
          (user-error "Need a local buffer, graphical SVG support and typst on exec-path"))
        (add-hook 'post-command-hook #'org-typst-preview--cursor nil t)
        (add-hook 'after-change-functions #'org-typst-preview--changed nil t)
        (add-hook 'text-scale-mode-hook #'org-typst-preview--zoom nil t)
        (org-typst-preview--refresh))
    (remove-hook 'post-command-hook #'org-typst-preview--cursor t)
    (remove-hook 'after-change-functions #'org-typst-preview--changed t)
    (remove-hook 'text-scale-mode-hook #'org-typst-preview--zoom t)
    (when (timerp org-typst-preview--timer) (cancel-timer org-typst-preview--timer))
    (mapc #'delete-overlay org-typst-preview--overlays)
    (setq org-typst-preview--overlays nil)))

(defun org-typst-preview--setup-edit-buffer ()
  "Enable preview for a Typst export block if its parent has preview enabled."
  (when (and (org-src-edit-buffer-p)
             (eq (org-src-source-type) 'export-block)
             (org-src-do-at-code-block
              (and org-typst-preview-mode
                   (equal (org-element-property :type (org-element-at-point)) "TYPST"))))
    (org-typst-preview-mode 1)))

(add-hook 'org-src-mode-hook #'org-typst-preview--setup-edit-buffer)

(provide 'org-typst-preview)

;;; org-typst-preview.el ends here
