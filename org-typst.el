;;; org-typst.el --- Typst integration for Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ad

;; Version: 0.1.0
;; Package-Requires: ((org-mode "9.6"))
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

;; (WIP) Typst integration for Org Mode

;;; Code:

;; Babel functionality is based on https://github.com/Cj-bc/ob-typst
(defcustom org-typst-babel-preamble '("#set page(width: auto, height: auto, margin: 0.3em)")
  "List of strings that will be prepended to all Typst code. Use
 to add packages, set rules, etc.

By default, contains a rule to appropriately size the output
image."
  :type '(repeat string))

(defun org-typst--babel-create-image (body tofile)
  "Create an image from Typst source using external process.

The Typst markup BODY is saved to a temporary Typst file, then converted to an
image file using 'typst compile'.

The generated image file is eventually moved to TOFILE.

Generated file format is determined by TOFILE file extension. Supported file
formats are png, pdf, and svg."
  (unless (executable-find "typst")
    (user-error "No 'typst' executable found!"))
  (let* ((tmpfile (org-babel-temp-file "ob-typst-src"))
         (ext (file-name-extension tofile))
         (log-buf (get-buffer-create "*Org Typst Output*")))
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
  "Execute a block of Typst markup."
  (let* ((out-file (or (alist-get :outfile params)
                       (org-babel-temp-file "ob-typst-out" (format ".%s" ob-typst/default-format)))))
    (org-typst--babel-create-image body out-file)
    out-file))

(provide 'org-typst)

;;; org-typst.el ends here
