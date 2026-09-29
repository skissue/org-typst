;;; ob-typst.el --- Org Babel support for Typst -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ad

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: literate programming, tools
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

;; Evaluate Typst source blocks with Org Babel.  Requires the `typst'
;; executable on `exec-path'.
;;
;; Enable with:
;;
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                '((typst . t)))

;;; Code:

(require 'ob)
(require 'org-macs)
(require 'subr-x)
(require 'cl-lib)

(defgroup ob-typst nil
  "Evaluate Typst source blocks with Org Babel."
  :group 'org-babel
  :prefix "org-typst-")

;; Babel functionality is based on https://github.com/Cj-bc/ob-typst
(defcustom org-typst-default-format "png"
  "Default format to use when rendering Typst markup."
  :type 'string
  :group 'ob-typst)

(defcustom org-typst-default-output-directory "typst-results/"
  "Directory for automatic results when neither :file nor :output-dir is given.
Relative paths are resolved against the execution directory, normally the
Org file's directory, or :dir when supplied.  nil uses no subdirectory."
  :type '(choice (const :tag "Execution directory" nil) directory)
  :group 'ob-typst)

(defcustom org-typst-babel-preamble '("#set page(width: auto, height: auto, margin: 0.3em)")
  "List of strings that will be prepended to all Typst code.
Use to add packages, set rules, etc.

By default, contains a rule to appropriately size the output
image."
  :type '(repeat string)
  :group 'ob-typst)

(defcustom org-typst-babel-hline-value "none"
  "A string that controls what to replace the `hline' symbol in tables with.
Applies when using a table as a variable and horizontal lines are
included. By default, `hline' is replaced with the Typst value `none'.

Note that this is interpolated literally, so strings need quotes
around them!"
  :type 'string
  :group 'ob-typst)

;;;###autoload
(defvar org-babel-default-header-args:typst
  '((:results . "raw"))
  "Default arguments to use when evaluating a Typst source block.

Having \"raw\" outputs a raw link, which can be shown inline with
`org-toggle-inline-images'.")

(defsubst org-typst--escape-string-char (char)
  "Return the Typst string literal representation of CHAR."
  (pcase char
    (?\\ "\\\\")
    (?\" "\\\"")
    (?\n "\\n")
    (?\r "\\r")
    (?\t "\\t")
    (_ (if (or (< char 32) (= char 127))
           (format "\\u{%x}" char)
         (char-to-string char)))))

(defun org-typst--babel-convert-var (var)
  "Convert the value VAR to an appropriate representation in Typst."
  (cond
   ((listp var)
    (let ((list (mapconcat #'org-typst--babel-convert-var
                           var
                           ", ")))
      (format (if var "(%s,)" "()") list)))
   ((numberp var)
    (number-to-string var))
   ((eq 'hline var)
    org-typst-babel-hline-value)
   ((stringp var)
    (concat "\""
            (mapconcat #'org-typst--escape-string-char var "")
            "\""))
   (t
    (error "Unsupported Typst variable type: %S" (type-of var)))))

(defun org-babel-variable-assignments:typst (params)
  "Return Typst markup that sets all variables from PARAMS.
Values are converted with `org-typst--babel-convert-var'."
  (mapcar
   (lambda (var)
     (format "#let %s = %s"
             (car var)
             (org-typst--babel-convert-var (cdr var))))
   (org-babel--get-vars params)))

(defun org-typst--babel-create-image (body tofile)
  "Create an image from Typst source using external process.

Send the Typst markup BODY to the compiler on stdin, using the execution
directory as the project root for resource paths.

Compile to scratch output before copying to TOFILE, preserving existing
output if compilation fails.  TOFILE may contain page placeholders in its
basename.  Return the generated destination filenames in page order.

Generated file format is determined by TOFILE file extension. Supported file
formats are png, pdf, and svg."
  (unless (executable-find "typst")
    (user-error "No 'typst' executable found!"))
  (let* ((ext (file-name-extension tofile))
         (log-buf (get-buffer-create "*Org Typst Output*"))
         (tmp-dir (make-temp-file
                   (expand-file-name "ob-typst-" (org-babel-temp-directory)) t))
         (tmp-file (expand-file-name (file-name-nondirectory tofile) tmp-dir))
         (manifest (expand-file-name "outputs.json" tmp-dir)))
    (unwind-protect
        (with-temp-buffer
          (unless (member ext '("png" "pdf" "svg"))
            (user-error "Unsupported Typst output format %S; expected png, pdf, or svg" ext))
          (when (string-match-p "{\\(?:0?p\\|n\\|t\\)}" (or (file-name-directory tofile) ""))
            (user-error "Typst page placeholders are supported only in the filename"))
          (with-current-buffer log-buf
            (erase-buffer))
          (insert (string-join org-typst-babel-preamble "\n")
                  "\n\n"
                  body)
          (let* ((coding-system-for-write 'utf-8-unix)
                 (coding-system-for-read 'utf-8-unix)
                 (status (call-process-region
                          (point-min) (point-max) "typst" nil (list log-buf t) nil
                          "compile" "--root" default-directory
                          "--deps" manifest "--deps-format" "json"
                          "--format" ext "-" tmp-file)))
            (unless (equal status 0)
              (error "Typst compilation failed (%s); see *Org Typst Output*" status)))
          (erase-buffer)
          (insert-file-contents manifest)
          (goto-char (point-min))
          (cl-loop for file across (alist-get 'outputs
                                              (json-parse-buffer :object-type 'alist))
                   for destination = (expand-file-name (file-name-nondirectory file)
                                                       (file-name-directory tofile))
                   do (copy-file file destination 'replace)
                   collect (if (file-name-absolute-p tofile)
                               destination
                             (file-relative-name destination))))
      (delete-directory tmp-dir t))))

;;;###autoload
(defun org-babel-execute:typst (body params)
  "Execute a block BODY of Typst markup.
Write to :file in PARAMS.  If :file is not given, create a unique file in
:output-dir or `org-typst-default-output-directory'.
Return raw Org links, using a RESULTS drawer for multiple pages.
Explicit :results file is supported for single-file output only."
  (let* ((out-file (alist-get :file params))
         (file-result (member "file" (alist-get :result-params params)))
         (vars (org-babel-variable-assignments:typst params))
         (full-body (org-babel-expand-body:generic body params vars))
         reservation success)
    (unless out-file
      (unless (member org-typst-default-format '("png" "pdf" "svg"))
        (user-error "Unsupported Typst output format %S; expected png, pdf, or svg"
                    org-typst-default-format))
      (let ((directory (expand-file-name
                        (or (alist-get :output-dir params)
                            org-typst-default-output-directory
                            default-directory))))
        (make-directory directory t)
        (setq reservation (make-temp-file (expand-file-name "ob-typst-" directory)
                                          nil (concat "." org-typst-default-format)))
        (setq out-file
              (if (or file-result (equal org-typst-default-format "pdf"))
                  reservation
                (concat (file-name-sans-extension reservation)
                        "-{p}." org-typst-default-format)))))
    (unwind-protect
        (progn
          (when (and file-result (string-match-p "{\\(?:0?p\\|n\\|t\\)}" out-file))
            (user-error "Use :results raw for patterned Typst output"))
          (let* ((files (org-typst--babel-create-image full-body out-file))
                 (links (unless file-result
                          (mapconcat #'org-babel-result-to-file files "\n"))))
            (setq success t)
            (cond
             (file-result
              (unless (alist-get :file params) (car files)))
             ((and (cdr files)
                   (not (member "drawer" (alist-get :result-params params))))
              (concat ":results:\n" links "\n:end:"))
             (t links))))
      (when (and reservation (not (and success (equal reservation out-file))))
        (delete-file reservation)))))

(provide 'ob-typst)

;;; ob-typst.el ends here
