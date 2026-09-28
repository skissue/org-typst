;;; ob-typst.el --- Org Babel support for Typst -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ad

;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.6"))
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
  '((:results . "file graphics raw"))
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

The Typst markup BODY is saved to a temporary Typst file, then converted to an
image file using the typst compile command.

The generated image file is eventually moved to TOFILE.

Generated file format is determined by TOFILE file extension. Supported file
formats are png, pdf, and svg."
  (unless (executable-find "typst")
    (user-error "No 'typst' executable found!"))
  (let* ((tmpfile (org-babel-temp-file "ob-typst-src"))
         (ext (file-name-extension tofile))
         (log-buf (get-buffer-create "*Org Typst Output*")))
    (unless (member ext '("png" "pdf" "svg"))
      (user-error "Unsupported Typst output format %S; expected png, pdf, or svg" ext))
    (with-temp-file tmpfile
      (insert
       (string-join org-typst-babel-preamble "\n")
       "\n\n"
       body))
    (copy-file (org-compile-file
                tmpfile
                (list (format "typst compile --format %s %%F %%O" ext))
                ext "" log-buf)
               tofile 'replace)))

;;;###autoload
(defun org-babel-execute:typst (body params)
  "Execute a block BODY of Typst markup.
Write to :file in PARAMS.  If :file is not given, create a unique file in
:output-dir or `org-typst-default-output-directory'."
  (let* ((out-file (alist-get :file params))
         (vars (org-babel-variable-assignments:typst params))
         (full-body (org-babel-expand-body:generic body params vars))
         success)
    (unless out-file
      (unless (member org-typst-default-format '("png" "pdf" "svg"))
        (user-error "Unsupported Typst output format %S; expected png, pdf, or svg"
                    org-typst-default-format))
      (let ((directory (expand-file-name
                        (or (alist-get :output-dir params)
                            org-typst-default-output-directory
                            default-directory))))
        (make-directory directory t)
        (setq out-file (make-temp-file (expand-file-name "ob-typst-" directory)
                                       nil (concat "." org-typst-default-format)))))
    (unwind-protect
        (progn
          (org-typst--babel-create-image full-body out-file)
          (setq success t)
          (unless (alist-get :file params) out-file))
      (unless (or success (alist-get :file params))
        (delete-file out-file)))))

(provide 'ob-typst)

;;; ob-typst.el ends here
