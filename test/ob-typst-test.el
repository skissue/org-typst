;;; ob-typst-test.el --- Tests for Typst Babel support -*- lexical-binding: t -*-

;;; Commentary:

;; Run from the repository root:
;; emacs --batch -Q -L . -l test/ob-typst-test.el -f ert-run-tests-batch-and-exit
;;
;; Compiler integration tests require `typst' on PATH and skip if absent.
;; These tests assert desired behavior, including currently broken cases;
;; failures are intentionally not marked as expected failures.
;; Desired output contracts: default Babel results survive scratch cleanup,
;; and explicit {p} output patterns produce one image file per page.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ob-typst)

(defconst ob-typst-test--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro ob-typst-test--isolated (&rest body)
  "Run BODY in a disposable document directory with separate Babel scratch space."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "ob-typst-test-" t))
          (default-directory (file-name-as-directory directory))
          (org-babel-temporary-directory (expand-file-name "scratch" directory))
          (org-confirm-babel-evaluate nil)
          (org-typst-default-format "png")
          (org-typst-default-output-directory "typst-results/")
          (org-typst-babel-preamble
           '("#set page(width: auto, height: auto, margin: 0.3em)")))
     (make-directory org-babel-temporary-directory)
     (unwind-protect (progn ,@body)
       (delete-directory directory t))))

(defun ob-typst-test--contents (file)
  "Read FILE literally, preserving binary signatures."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun ob-typst-test--assert-format (file format)
  "Assert that FILE contains FORMAT data, not just a pathname or empty output."
  (let ((data (ob-typst-test--contents file)))
    (pcase format
      ("png" (should (string-prefix-p
                      (unibyte-string 137 80 78 71 13 10 26 10) data)))
      ("pdf" (should (string-prefix-p "%PDF-" data)))
      ("svg" (should (string-match-p "<svg[ >]" data)))
      (_ (error "Unknown test format: %s" format)))))

(defun ob-typst-test--block (headers body &optional name expected-file)
  "Execute HEADERS and BODY with optional NAME, checking EXPECTED-FILE's link."
  (with-temp-buffer
    (org-mode)
    (when name (insert "#+name: " name "\n"))
    (insert "#+begin_src typst " headers "\n" body "\n#+end_src\n")
    (goto-char (point-min))
    (when name (forward-line))
    (prog1 (org-babel-execute-src-block)
      (when expected-file
        (should (string-match-p
                 (regexp-quote (concat "[[file:" expected-file "]]"))
                 (buffer-string)))))))

(defun ob-typst-test--assert-value (value expected)
  "Compile an assertion that converted VALUE equals independent Typst EXPECTED."
  (unless (executable-find "typst") (ert-skip "Typst is not installed"))
  (ob-typst-test--isolated
    (ob-typst-test--assert-format
     (org-babel-execute:typst
      (format "#assert(%s == %s)\nValue checked"
              (org-typst--babel-convert-var value) expected)
      nil)
     "png")))

(defun ob-typst-test--fresh-emacs (form)
  "Evaluate FORM in a clean Emacs process and assert successful exit."
  (with-temp-buffer
    (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil "--batch" "-Q" "-L" ob-typst-test--root
                   "--eval" (prin1-to-string form))))
      (ert-info ((buffer-string))
        (should (equal status 0))))))

(ert-deftest ob-typst-load-direct ()
  (ob-typst-test--fresh-emacs
   '(progn
      (require 'ob-typst)
      (dolist (function '(org-babel-temp-file org-babel--get-vars
                         org-babel-expand-body:generic org-compile-file string-join))
        (unless (fboundp function) (error "Missing dependency: %s" function)))
      (unless (equal (org-babel-variable-assignments:typst '((:var . (n . 3))))
                     '("#let n = 3"))
        (error "Variable assignment failed")))))

(ert-deftest ob-typst-load-via-babel ()
  (ob-typst-test--fresh-emacs
   '(progn
      (require 'ob)
      (org-babel-do-load-languages 'org-babel-load-languages '((typst . t)))
      (unless (featurep 'ob-typst) (error "Backend was not loaded")))))

(ert-deftest ob-typst-package-metadata ()
  (require 'package)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "ob-typst.el" ob-typst-test--root))
    (let* ((package (package-buffer-info))
           (dependencies (package-desc-reqs package)))
      (should (eq (package-desc-name package) 'ob-typst))
      (should (equal (cadr (assq 'org dependencies)) '(9 6)))
      (should (equal (cadr (assq 'emacs dependencies)) '(26 1)))
      (should-not (assq 'org-mode dependencies)))))

(ert-deftest ob-typst-autoload-defaults-before-first-block ()
  (ob-typst-test--isolated
    (copy-file (expand-file-name "ob-typst.el" ob-typst-test--root) "ob-typst.el")
    (ob-typst-test--fresh-emacs
     `(progn
        (require 'autoload)
        (let ((generated-autoload-file ,(expand-file-name "typst-autoloads.el")))
          (update-directory-autoloads ,default-directory)
          (load generated-autoload-file nil t))
        (when (featurep 'ob-typst) (error "Backend loaded eagerly"))
        (unless (autoloadp (symbol-function 'org-babel-execute:typst))
          (error "Executor is not autoloaded"))
        (unless (equal org-babel-default-header-args:typst
                       '((:results . "file graphics raw")))
          (error "Defaults missing before backend load"))
        (require 'ob)
        (with-temp-buffer
          (org-mode)
          (insert "#+begin_src typst\nHello\n#+end_src\n")
          (goto-char (point-min))
          (unless (member "file" (cdr (assq :result-params
                                           (nth 2 (org-babel-get-src-block-info)))))
            (error "First block did not receive defaults")))
        ;; Resolve the real generated autoload without requiring the compiler.
        (autoload-do-load (symbol-function 'org-babel-execute:typst)
                          'org-babel-execute:typst)
        (unless (featurep 'ob-typst) (error "Autoload did not load backend"))))))

(ert-deftest ob-typst-value-numbers ()
  (ob-typst-test--assert-value '(-7 2.5 0) "(-7, 2.5, 0)"))

(ert-deftest ob-typst-value-string ()
  (ob-typst-test--assert-value "Hello λ #[]" "\"Hello λ #[]\""))

(ert-deftest ob-typst-value-unsupported-types ()
  (dolist (value '(unexpected [1 2] (1 unexpected)))
    (should-error (org-typst--babel-convert-var value) :type 'error)))

(ert-deftest ob-typst-value-empty-array ()
  (ob-typst-test--assert-value nil "()"))

(ert-deftest ob-typst-value-nested-empty-array ()
  (ob-typst-test--assert-value '(nil) "((),)"))

(ert-deftest ob-typst-value-table ()
  (ob-typst-test--assert-value '((1 "east") (9 "west"))
                             "((1, \"east\"), (9, \"west\"))"))

(ert-deftest ob-typst-value-singleton-array ()
  (ob-typst-test--assert-value '(7) "(7,)"))

(ert-deftest ob-typst-value-single-row-table ()
  (ob-typst-test--assert-value '((2 9)) "((2, 9),)"))

(ert-deftest ob-typst-value-single-column-table ()
  (ob-typst-test--assert-value '((2) (9)) "((2,), (9,))"))

(ert-deftest ob-typst-value-embedded-quote ()
  (ob-typst-test--assert-value "say \"hi\"" "\"say \\\"hi\\\"\""))

(ert-deftest ob-typst-value-backslash ()
  (ob-typst-test--assert-value "C:\\temp" "\"C:\\\\temp\""))

(ert-deftest ob-typst-value-mixed-escapes ()
  ;; Independent code points avoid duplicating the serializer's escape syntax.
  (ob-typst-test--assert-value "\\\"λ\\n\\"
                             "(92, 34, 955, 92, 110, 92).map(str.from-unicode).join()"))

(ert-deftest ob-typst-value-control-characters ()
  (ob-typst-test--assert-value "a\nb\tc\r\b\f\0\177"
                             "\"a\\nb\\tc\\r\\u{8}\\u{c}\\u{0}\\u{7f}\""))

(ert-deftest ob-typst-value-code-like-string ()
  (ob-typst-test--assert-value "\"; panic(\"injected\"); //"
                             "\"\\\"; panic(\\\"injected\\\"); //\""))

(ert-deftest ob-typst-value-hline ()
  (let ((org-typst-babel-hline-value "none"))
    (ob-typst-test--assert-value '(1 hline 8) "(1, none, 8)")))

(ert-deftest ob-typst-value-custom-hline ()
  (let ((org-typst-babel-hline-value "\"separator\""))
    (ob-typst-test--assert-value '(1 hline 8) "(1, \"separator\", 8)")))

(ert-deftest ob-typst-render-supported-formats ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (dolist (format '("png" "pdf" "svg"))
      (ert-info (format)
        (let ((file (concat "render." format)))
          (should-not (org-babel-execute:typst "Hello" `((:file . ,file))))
          (ob-typst-test--assert-format file format))))))

(ert-deftest ob-typst-render-default-format ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (let ((org-typst-default-format "svg"))
      (ob-typst-test--assert-format (org-babel-execute:typst "Hello" nil) "svg"))))

(ert-deftest ob-typst-render-output-path-with-spaces ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (make-directory "some images")
    (org-babel-execute:typst "Hello" '((:file . "some images/a b.png")))
    (ob-typst-test--assert-format "some images/a b.png" "png")))

(ert-deftest ob-typst-render-preamble-and-variables ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (let ((org-typst-babel-preamble '("#let base = 11")))
      (ob-typst-test--assert-format
       (org-babel-execute:typst "#assert(base + n == 14)\nHello"
                                '((:var . (n . 3))))
       "png"))))

(ert-deftest ob-typst-babel-file-header ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (should (equal (ob-typst-test--block ":file result.svg" "Hello" nil "result.svg")
                   "result.svg"))
    (ob-typst-test--assert-format "result.svg" "svg")))

(ert-deftest ob-typst-babel-file-results-override ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (should (equal (ob-typst-test--block ":file result.svg :results file" "Hello"
                                        nil "result.svg")
                   "result.svg"))
    (ob-typst-test--assert-format "result.svg" "svg")))

(ert-deftest ob-typst-babel-file-ext-and-output-dir ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (make-directory "images")
    (ob-typst-test--block ":file-ext svg :output-dir images" "Hello" "diagram"
                          "images/diagram.svg")
    (ob-typst-test--assert-format "images/diagram.svg" "svg")))

(ert-deftest ob-typst-babel-default-result-survives-scratch-cleanup ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (let ((file (ob-typst-test--block "" "Hello")))
      (ob-typst-test--assert-format file "png")
      ;; Simulate Babel's shutdown cleanup, without requiring a naming scheme
      ;; for the durable result file.
      (delete-directory org-babel-temporary-directory t)
      (ob-typst-test--assert-format file "png"))))

(ert-deftest ob-typst-babel-automatic-output-directory-and-links ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (dolist (headers '("" ":output-dir images" ":dir assets"))
      (make-directory "assets" t)
      (with-temp-buffer
        (org-mode)
        (insert "#+begin_src typst " headers "\nHello\n#+end_src\n")
        (goto-char (point-min))
        (let* ((file (org-babel-execute-src-block))
               (directory (pcase headers
                            ("" "typst-results")
                            (":output-dir images" "images")
                            (_ "assets/typst-results"))))
          (should (equal (file-name-directory file)
                         (file-name-as-directory (expand-file-name directory))))
          (should (string-match-p
                   (concat "\\[\\[file:[^]\n]*"
                           (regexp-quote (file-name-nondirectory file)) "\\]\\]")
                   (buffer-string)))
          (ob-typst-test--assert-format file "png"))))))

(ert-deftest ob-typst-render-custom-output-directory ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (dolist (org-typst-default-output-directory '("custom/" nil))
      (let ((file (org-babel-execute:typst "Hello" nil)))
        (should (equal (file-name-directory file)
                       (if org-typst-default-output-directory
                           (expand-file-name "custom/")
                         default-directory)))
        (ob-typst-test--assert-format file "png"))
      (let ((file (org-babel-execute:typst "Hello" '((:output-dir . "override")))))
        (should (equal (file-name-directory file) (expand-file-name "override/")))
        (ob-typst-test--assert-format file "png")))))

(ert-deftest ob-typst-render-automatic-output-lifecycle ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (let* ((first (org-babel-execute:typst "First" nil))
           (contents (ob-typst-test--contents first))
           (second (org-babel-execute:typst "Second" nil))
           (files (directory-files "typst-results")))
      (should-not (equal first second))
      (ob-typst-test--assert-format second "png")
      (should-error (org-babel-execute:typst "#let =" nil))
      (should (equal (directory-files "typst-results") files))
      (should (equal (ob-typst-test--contents first) contents)))))

(ert-deftest ob-typst-render-relative-read ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (with-temp-file "data.txt" (insert "local data"))
    (ob-typst-test--assert-format
     (org-babel-execute:typst "#assert(read(\"data.txt\") == \"local data\")\nHello" nil)
     "png")))

(ert-deftest ob-typst-render-relative-import ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (with-temp-file "helper.typ" (insert "#let answer = 37"))
    (ob-typst-test--assert-format
     (org-babel-execute:typst
      "#import \"helper.typ\": answer\n#assert(answer == 37)\nHello" nil)
     "png")))

(ert-deftest ob-typst-render-relative-image ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (with-temp-file "image.svg"
      (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"20\">"
              "<rect width=\"10\" height=\"20\" fill=\"red\"/></svg>"))
    (ob-typst-test--assert-format
     (org-babel-execute:typst "#image(\"image.svg\")" nil) "png")))

(ert-deftest ob-typst-render-babel-dir ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (make-directory "assets")
    (with-temp-file "assets/data.txt" (insert "chosen directory"))
    (ob-typst-test--assert-format
     (ob-typst-test--block
      ":dir assets" "#assert(read(\"data.txt\") == \"chosen directory\")\nHello")
     "png")))

(ert-deftest ob-typst-render-multipage-pdf ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (org-babel-execute:typst "First\n#pagebreak()\nSecond" '((:file . "pages.pdf")))
    (ob-typst-test--assert-format "pages.pdf" "pdf")
    (should (string-match-p "/Count 2\\b" (ob-typst-test--contents "pages.pdf")))))

(ert-deftest ob-typst-render-multipage-png ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (org-babel-execute:typst "First\n#pagebreak()\nSecond"
                            '((:file . "page-{p}.png")))
    (ob-typst-test--assert-format "page-1.png" "png")
    (ob-typst-test--assert-format "page-2.png" "png")
    (should-not (file-exists-p "page-3.png"))))

(ert-deftest ob-typst-render-multipage-svg ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (org-babel-execute:typst "First\n#pagebreak()\nSecond"
                            '((:file . "page-{p}.svg")))
    (ob-typst-test--assert-format "page-1.svg" "svg")
    (ob-typst-test--assert-format "page-2.svg" "svg")
    (should-not (file-exists-p "page-3.svg"))))

(ert-deftest ob-typst-error-missing-executable ()
  (ob-typst-test--isolated
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (org-babel-execute:typst "Hello" nil) :type 'user-error))))

(ert-deftest ob-typst-error-invalid-source-preserves-output ()
  (skip-unless (executable-find "typst"))
  (ob-typst-test--isolated
    (with-temp-file "existing.png" (insert "do not overwrite"))
    (should-error (org-babel-execute:typst "#let =" '((:file . "existing.png"))))
    (should (equal (ob-typst-test--contents "existing.png") "do not overwrite"))))

(defun ob-typst-test--reject-format (file)
  "Require invalid FILE formats to fail before entering the compiler shell path."
  (ob-typst-test--isolated
    ;; Never execute a shell payload, even if validation is absent.
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/mock/typst"))
              ((symbol-function 'org-compile-file)
               (lambda (&rest _) (ert-fail "Invalid format reached shell compiler"))))
      (should-error (org-babel-execute:typst "Hello" `((:file . ,file)))
                    :type 'user-error))))

(ert-deftest ob-typst-error-missing-extension ()
  (ob-typst-test--reject-format "output"))

(ert-deftest ob-typst-error-unsupported-format ()
  (ob-typst-test--reject-format "output.gif"))

(ert-deftest ob-typst-error-shell-metacharacters-in-format ()
  (ob-typst-test--reject-format "output.png; echo injected; #"))

(ert-deftest ob-typst-error-invalid-default-format ()
  (ob-typst-test--isolated
    (let ((org-typst-default-format "png; echo injected; #"))
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/mock/typst"))
                ((symbol-function 'org-compile-file)
                 (lambda (&rest _) (ert-fail "Invalid default reached shell compiler"))))
        (should-error (org-babel-execute:typst "Hello" nil) :type 'user-error)))))

;;; ob-typst-test.el ends here
