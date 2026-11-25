;;; org-typst-preview-test.el --- Preview lifecycle tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'org-typst-preview)

(defmacro org-typst-preview-test--with-buffer (&rest body)
  (declare (indent 0))
  `(let ((buf (generate-new-buffer "preview-test.org")))
     (unwind-protect (with-current-buffer buf ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

(defun org-typst-preview-test--start (&optional text)
  "Enable preview on TEXT, holding the compiler at stdin until released."
  (unless (and (display-graphic-p) (image-type-available-p 'svg) (executable-find "typst"))
    (ert-skip "Requires graphical SVG support and typst"))
  (org-mode)
  (insert (or text "@@typst:$x$@@ tail\n"))
  (let (job)
    (cl-letf (((symbol-function 'process-send-eof) (lambda (proc) (setq job proc))))
      (org-typst-preview-mode 1))
    (should (process-live-p job))
    job))

(defmacro org-typst-preview-test--await (condition)
  `(let ((deadline (+ (float-time) 5)))
     (while (and (not ,condition) (< (float-time) deadline))
       (accept-process-output nil 0.02))
     (should ,condition)))

(ert-deftest org-typst-preview-edit-undo-first-render ()
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start)))
      (goto-char (point-min))
      (search-forward "$x")
      (buffer-enable-undo)
      (undo-boundary)
      (insert "y")
      (should-not (process-live-p job))
      (undo-boundary)
      (undo-only 1)
      (goto-char (point-max))
      (should (equal (buffer-string) "@@typst:$x$@@ tail\n"))
      (org-typst-preview-test--await (get-char-property (point-min) 'display))
      (goto-char (point-min))
      (search-forward "$x")
      (insert "^2")
      (goto-char (point-max))
      (text-scale-increase 1)
      (should-not (get-char-property (point-min) 'display))
      (org-typst-preview-test--await (get-char-property (point-min) 'display)))))

(ert-deftest org-typst-preview-teardown-with-pending-work ()
  (dolist (teardown (list (lambda () (org-typst-preview-mode -1))
                          #'text-mode #'kill-buffer))
    (org-typst-preview-test--with-buffer
      (let* ((buf (current-buffer))
             (job (org-typst-preview-test--start))
             (output (process-buffer job)))
        (insert "prose") ; Schedule a refresh without touching the running job.
        (funcall teardown)
        (should-not (process-live-p job))
        (should-not (buffer-live-p output))
        (sleep-for 0.3) ; Let an orphaned debounce timer fire.
        (when (buffer-live-p buf)
          (should-not (overlays-in (point-min) (point-max))))))))

(ert-deftest org-typst-preview-child-does-not-own-parent-job ()
  (dolist (child '(footnote indirect))
    (org-typst-preview-test--with-buffer
      (let ((job (org-typst-preview-test--start "@@typst:$x$@@ [fn:a]\n\n[fn:a] note\n")))
        ;; The child must leave the parent's scheduled refresh alone as well.
        (insert "\n* More\n@@typst:$y$@@\n")
        (pcase child
          ('footnote
           (save-window-excursion
             (switch-to-buffer (current-buffer))
             (goto-char (point-min)) (search-forward "[fn:a")
             (org-edit-special)
             (org-edit-src-abort)))
          ('indirect (kill-buffer (org-get-indirect-buffer))))
        (should (process-live-p job))
        (process-send-eof job)
        (org-typst-preview-test--await (get-char-property (point-min) 'display))
        (org-typst-preview-test--await (get-char-property (- (point-max) 2) 'display))))))

(ert-deftest org-typst-preview-refused-parent-edit-preserves-image ()
  (org-typst-preview-test--with-buffer
    (let ((parent (current-buffer))
          (job (org-typst-preview-test--start "#+begin_export typst\n$x$\n#+end_export\n\ntail\n")))
      (process-send-eof job)
      (org-typst-preview-test--await (get-char-property (point-min) 'display))
      (save-window-excursion
        (switch-to-buffer parent)
        (goto-char (point-min)) (search-forward "$x")
        (org-edit-special)
        (unwind-protect
            (progn
              (with-current-buffer parent
                (let ((image (get-char-property (point-min) 'display)))
                  (should image)
                  (should-error (insert "!") :type 'user-error)
                  (should (eq image (get-char-property (point-min) 'display)))))
              ;; The whole edit buffer is input, even if the edit moves the
              ;; preview's start marker past the inserted text.
              (org-typst-preview-test--await (get-char-property (point-min) 'after-string))
              (goto-char (point-min)) (insert-before-markers "1+")
              (should-not (get-char-property (1- (point-max)) 'after-string))
              (org-typst-preview-test--await (get-char-property (point-min) 'after-string))
              (goto-char (point-max)) (insert "!")
              (should-not (get-char-property (point-min) 'after-string)))
          (org-edit-src-abort))))))

(ert-deftest org-typst-preview-adjacent-prose-preserves-job ()
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start)))
      (goto-char (point-min)) (search-forward "$@@")
      (insert ".")
      (should (process-live-p job))
      (backward-char)
      (let ((edge (point)))
        (org-insert-timestamp (encode-time 0 0 12 25 11 2026))
        (should (process-live-p job))
        (process-send-eof job)
        (org-typst-preview-test--await (get-char-property (point-min) 'display))
        (let ((image (get-char-property (point-min) 'display)))
          (goto-char edge) (insert-before-markers "!")
          (should (eq image (get-char-property (1- edge) 'display)))
          (should-not (get-char-property edge 'display)))))))

(ert-deftest org-typst-preview-indirect-clone-starts-without-copied-images ()
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start)) child)
      (process-send-eof job)
      (org-typst-preview-test--await (get-char-property 1 'display))
      ;; Org calls make-indirect-buffer directly, even on Emacs 28.  A
      ;; clone made while narrowed must not acquire hidden images either.
      (save-restriction
        (narrow-to-region (point-max) (point-max))
        (setq child (org-get-indirect-buffer)))
      (unwind-protect
          (with-current-buffer child
            (widen)
            (should-not org-typst-preview-mode)
            (should-not (get-char-property 1 'display))
            (org-typst-preview-mode 1)
            (org-typst-preview-test--await (get-char-property 1 'display))
            (org-typst-preview-mode -1)
            (should-not (get-char-property 1 'display)))
        (kill-buffer child))
      (should (get-char-property 1 'display)))))

(ert-deftest org-typst-preview-clone-of-disabled-buffer-stops-rendering ()
  (org-typst-preview-test--with-buffer
    (org-typst-preview-test--start)
    (org-typst-preview-mode -1)
    ;; The parent's local hook list is copied over the clone's hook list.
    (add-hook 'clone-buffer-hook #'ignore nil t)
    (let ((org-mode-hook (cons #'org-typst-preview-mode org-mode-hook)) job copy)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'process-send-eof) (lambda (p) (setq job p))))
              (setq copy (clone-buffer)))
            (with-current-buffer copy
              (should-not org-typst-preview-mode)
              (should-not (and job (process-live-p job)))
              (should-not (get-char-property 1 'display))))
        (when (buffer-live-p copy) (kill-buffer copy))))))

(ert-deftest org-typst-preview-refresh-while-narrowed ()
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start "@@typst:$a$@@\n* More\nprose\n")))
      (process-send-eof job)
      (org-typst-preview-test--await (get-char-property 1 'display))
      (let ((hidden (get-char-property 1 'display)))
        (save-restriction
          (search-backward "* More") (narrow-to-region (point) (point-max))
          (goto-char (point-max)) (insert "@@typst:$b$@@ tail")
          (org-typst-preview-test--await (get-char-property (- (point-max) 6) 'display)))
        (should (eq hidden (get-char-property 1 'display)))))))

(ert-deftest org-typst-preview-defers-incomplete-snapshot ()
  (skip-unless (fboundp 'with-restriction))
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start "@@typst:$a$@@\n* More\nprose\n")))
      (process-send-eof job)
      (org-typst-preview-test--await (get-char-property 1 'display))
      (let ((hidden (get-char-property 1 'display))
            (start (search-backward "* More")) new)
        (with-restriction start (point-max) :label 'test
          (search-forward "prose") (insert " ") (setq new (point))
          (insert "@@typst:$b$@@ tail")
          (sleep-for 0.6) ; Allow the debounce timer to attempt a refresh.
          (should-not (get-char-property new 'display)))
        (should (eq hidden (get-char-property 1 'display)))
        ;; Lifting the restriction is enough; no further edits are needed.
        (org-typst-preview-test--await (get-char-property new 'display))))))

(ert-deftest org-typst-preview-edit-across-restriction ()
  (skip-unless (fboundp 'with-restriction))
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start "#+begin_export typst\n$x$\n#+end_export\n\ntail\n")))
      (goto-char 1) (search-forward "$x")
      (with-restriction (1- (point)) (1+ (point)) :label 'test
        (insert "^2")
        (should-not (process-live-p job)))
      (goto-char (point-max))
      (org-typst-preview-test--await (get-char-property 1 'display)))))

(ert-deftest org-typst-preview-restricted-adjacent-timestamp ()
  (skip-unless (fboundp 'with-restriction))
  (dolist (rendered '(nil t))
    (org-typst-preview-test--with-buffer
      (let ((job (org-typst-preview-test--start)) image)
        (when rendered
          (process-send-eof job)
          (org-typst-preview-test--await (get-char-property 1 'display))
          (setq image (get-char-property 1 'display)))
        (goto-char 1)
        (let ((edge (search-forward "$@@")))
          (with-restriction 4 (point-max) :label 'test
            (org-insert-timestamp (encode-time 0 0 12 25 11 2026))
            (if rendered
                (progn (should (eq image (get-char-property (1- edge) 'display)))
                       (should-not (get-char-property edge 'display)))
              (should (process-live-p job)))))))))

(ert-deftest org-typst-preview-restricted-same-length-edit ()
  (skip-unless (fboundp 'with-restriction))
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start)))
      (insert "prose") ; The snapshot is dirty, but the snippet is unchanged.
      (goto-char 1) (search-forward "$x")
      (let ((x (1- (point))))
        (with-restriction x (1+ x) :label 'test
          (put-text-property x (1+ x) 'face 'bold)
          (should (process-live-p job))
          (subst-char-in-region x (1+ x) ?x ?y)
          (should-not (process-live-p job)))))))

(ert-deftest org-typst-preview-restricted-delete-at-right-edge ()
  (skip-unless (fboundp 'with-restriction))
  (dolist (rendered '(nil t))
    (org-typst-preview-test--with-buffer
      (let ((job (org-typst-preview-test--start "prefix @@typst:#missing()@@ tail\n")))
        (goto-char 1)
        (let* ((beg (search-forward "prefix ")) (end (search-forward ")@@")))
          (when rendered
            (process-send-eof job)
            (org-typst-preview-test--await (get-char-property beg 'after-string)))
          (with-restriction 2 end :label 'test
            (delete-region beg end)
            (should-not (process-live-p job)))
          ;; No timer has run.  An empty overlay could still display its diagnostic.
          (should-not (cl-some (lambda (ov) (overlay-get ov 'after-string))
                               (overlays-in (point-min) (point-max)))))))))

(ert-deftest org-typst-preview-comment-invalidates-fragment ()
  (dolist (rendered '(nil t))
    (org-typst-preview-test--with-buffer
      (let ((job (org-typst-preview-test--start "prefix @@typst:$x$@@ suffix\n")))
        (when rendered
          (process-send-eof job)
          (org-typst-preview-test--await (get-char-property 8 'display)))
        (goto-char (point-min)) (insert "# ")
        ;; The fragment's bytes are intact; its Org meaning changes at refresh.
        (org-typst-preview-test--await (not (process-live-p job)))
        (org-typst-preview-test--await (not (get-char-property 10 'display)))
        (delete-region 1 3)
        (org-typst-preview-test--await (get-char-property 8 'display))))))

(ert-deftest org-typst-preview-shared-text-notifies-each-owner ()
  (org-typst-preview-test--with-buffer
    (let ((job (org-typst-preview-test--start "prefix @@typst:$x$@@ suffix\n"))
          (view (org-get-indirect-buffer))
          (editor (org-get-indirect-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer view
              (org-typst-preview-mode 1)
              (org-typst-preview-test--await (get-char-property 8 'display)))
            (with-current-buffer editor
              (org-typst-preview-mode -1)
              (goto-char (point-min)) (insert "# "))
            (org-typst-preview-test--await (not (process-live-p job)))
            (with-current-buffer view
              (org-typst-preview-test--await (not (get-char-property 10 'display))))
            ;; Only the indirect view is still enabled.  Edit through the base.
            (org-typst-preview-mode -1)
            (delete-region 1 3)
            (with-current-buffer view
              (org-typst-preview-test--await (get-char-property 8 'display)))
            (should-not (get-char-property 8 'display)))
        (kill-buffer editor)
        (kill-buffer view)))))

(defun org-typst-preview-test--structural-edit (text edit)
  "Check that EDIT to TEXT removes both pending and displayed previews."
  (dolist (rendered '(nil t))
    (org-typst-preview-test--with-buffer
      (let ((job (org-typst-preview-test--start text))
            (pos (copy-marker (save-excursion (goto-char (point-min)) (search-forward "$x")))))
        (when rendered
          (process-send-eof job)
          (org-typst-preview-test--await (get-char-property pos 'display)))
        (funcall edit)
        (org-typst-preview-test--await (not (process-live-p job)))
        (org-typst-preview-test--await (not (get-char-property pos 'display)))))))

(ert-deftest org-typst-preview-damaged-verse-closer ()
  (org-typst-preview-test--structural-edit
   "* Verse\n#+begin_verse\n# a @@typst:$x$@@ z\n#+end_verse\n"
   (lambda () (goto-char (point-min)) (search-forward "#+end_verse") (delete-char -1))))

(ert-deftest org-typst-preview-snippet-becomes-table-caption ()
  (org-typst-preview-test--structural-edit
   "a @@typst:$x$@@ z\n| 1 |\n"
   (lambda () (goto-char (point-min)) (insert "#+caption: "))))

(ert-deftest org-typst-preview-footnote-auto-enable ()
  (org-typst-preview-test--with-buffer
    (let* ((parent (current-buffer))
           (parent-job (org-typst-preview-test--start "Note [fn:a].\n\n[fn:a] @@typst:$x$@@ here.\n"))
           (org-mode-hook (cons #'org-typst-preview-mode org-mode-hook)) child-job output)
      (save-window-excursion
        (switch-to-buffer parent)
        (goto-char (point-min)) (search-forward "[fn:a")
        (cl-letf (((symbol-function 'process-send-eof)
                   (lambda (p) (setq child-job p output (process-buffer p)))))
          (org-edit-special))
        ;; Org ran org-mode-hook, then copied the parent's locals into the editor.
        (should (processp child-job))
        (should-not org-typst-preview-mode)
        (org-edit-src-abort))
      (should-not (process-live-p child-job))
      (should-not (buffer-live-p output))
      (should (process-live-p parent-job))
      (org-typst-preview-mode -1)
      (should-not (process-live-p parent-job)))))

(ert-deftest org-typst-preview-reaps-suppressed-kill ()
  (org-typst-preview-test--with-buffer
    (let* ((job (org-typst-preview-test--start))
           (child (make-indirect-buffer (current-buffer) "suppressed-kill" t t)) child-job output)
      (with-current-buffer child
        (cl-letf (((symbol-function 'process-send-eof)
                   (lambda (p) (setq child-job p output (process-buffer p)))))
          (org-typst-preview-mode 1)))
      (should (process-live-p child-job))
      (kill-buffer child)
      (goto-char (point-min)) (search-forward "$x") (insert "^2")
      (should-not (process-live-p job))
      (org-typst-preview-test--await (not (process-live-p child-job)))
      (should-not (buffer-live-p output))
      (goto-char (point-max))
      (org-typst-preview-test--await (get-char-property 1 'display)))))

;;; org-typst-preview-test.el ends here
