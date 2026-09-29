;;; ox-typst.el --- Org to Typst exporter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ad

;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (org "9.8"))
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

;; Export Org to Typst: direct text, heading, code, table and external-link
;; mappings. Unsupported elements are omitted.

;;; Code:
(require 'ox)
(require 'json)

(org-export-define-backend 'typst
                           '((template . (lambda (contents _info) contents))
                             (section . org-typst-contents)
                             (paragraph . org-typst-contents)
                             (plain-text . org-typst-plain-text)
                             (headline . org-typst-headline)
                             (bold . org-typst-emphasis)
                             (italic . org-typst-emphasis)
                             (underline . org-typst-emphasis)
                             (strike-through . org-typst-emphasis)
                             (code . org-typst-raw)
                             (verbatim . org-typst-raw)
                             (src-block . org-typst-raw)
                             (example-block . org-typst-raw)
                             (export-block . org-typst-export-block)
                             (export-snippet . org-typst-export-snippet)
                             (link . org-typst-link)
                             (plain-list . org-typst-list)
                             (item . org-typst-item)
                             (table . org-typst-table)
                             (table-row . org-typst-table-row)
                             (table-cell . org-typst-table-cell))
                           :options-alist '((:with-toc nil nil nil)
                                            (:section-numbers nil nil nil)
                                            (:with-sub-superscript nil nil nil))
                           :menu-entry '(?y "Export to Typst"
                                            ((?y "To .typ file" org-typst-export-to-typst)
                                             (?p "To .pdf file" org-typst-export-to-pdf))))

(defun org-typst-contents (_element contents _info)
  "Pass CONTENTS through unchanged."
  contents)

(defun org-typst-plain-text (text _info)
  "Escape Typst markup characters in TEXT."
  (replace-regexp-in-string
   (rx (any "\\#[]{}*_`$<>@=+-/.~"))
   (lambda (char) (concat "\\" char)) text t t))

(defun org-typst-headline (headline contents info)
  "Export HEADLINE and CONTENTS using export context INFO."
  (unless (org-element-property :footnote-section-p headline)
    (format "#heading(level: %d)[%s]\n%s"
            (org-export-get-relative-level headline info)
            (org-export-data (org-element-property :title headline) info)
            (or contents ""))))

(defun org-typst-emphasis (object contents _info)
  "Wrap CONTENTS in the Typst equivalent of OBJECT's emphasis."
  (format "#%s[%s]"
          (alist-get (org-element-type object)
                     '((bold . "strong")
                       (italic . "emph")
                       (underline . "underline")
                       (strike-through . "strike")))
          contents))

(defun org-typst-raw (element _contents _info)
  "Export ELEMENT as raw text, without delimiter collisions."
  (format "#raw(%s, block: %s%s)"
          (json-encode-string (org-element-property :value element))
          (if (memq (org-element-type element) '(src-block example-block))
              "true" "false")
          (if-let* ((lang (org-element-property :language element)))
              (concat ", lang: " (json-encode-string lang)) "")))

(defun org-typst-export-block (block _contents _info)
  "Pass Typst export BLOCK through without escaping or wrapping."
  (when (equal (org-element-property :type block) "TYPST")
    (org-remove-indentation (org-element-property :value block))))

(defun org-typst-export-snippet (snippet _contents _info)
  "Pass inline Typst SNIPPET through unchanged."
  (when (eq (org-export-snippet-backend snippet) 'typst)
    (org-element-property :value snippet)))

(defun org-typst-link (link description info)
  "Export an image or external LINK with DESCRIPTION and context INFO."
  (cond
   ((org-export-inline-image-p link '(("file" . "\\.\\(?:png\\|jpe?g\\|gif\\|svg\\|webp\\)\\'")))
    (org-typst-image link info))
   ((member (org-element-property :type link) '("http" "https" "mailto"))
    (let ((url (concat (org-element-property :type link) ":"
                       (org-element-property :path link))))
      (format "#link(%s)[%s]" (json-encode-string url)
              (or description (org-typst-plain-text url info)))))
   (t description)))

(defun org-typst-image (link info)
  "Export local image LINK, captioning standalone images using INFO."
  (let* ((parent (org-element-parent-element link))
         (attr (org-export-read-attribute :attr_typst parent))
         (image (concat
                 "image(" (json-encode-string (org-element-property :path link))
                 (when-let* ((width (plist-get attr :width)))
                   (format ", width: %s" width))
                 (when-let* ((height (plist-get attr :height)))
                   (format ", height: %s" height))
                 ")"))
         (caption (org-export-get-caption parent)))
    (if (and caption (eq (org-element-type parent) 'paragraph)
             (cl-every (lambda (child)
                         (or (eq child link)
                             (and (stringp child) (not (org-string-nw-p child)))))
                       (org-element-contents parent)))
        (format "#figure(%s, caption: [%s])" image
                (org-export-data caption info))
      (concat "#" image))))

(defun org-typst-list (list contents _info)
  "Export bullet, numbered or description LIST CONTENTS."
  (when-let* ((kind (alist-get
                     (org-element-property :type list)
                     '((unordered . "list")
                       (ordered . "enum")
                       (descriptive . "terms")))))
    (format "#%s(\n%s)\n" kind contents)))

(defun org-typst-item (item contents info)
  "Export ITEM and CONTENTS as one list argument using context INFO."
  (if (eq (org-element-property :type (org-element-parent item)) 'descriptive)
      (format "terms.item([%s], [%s]),\n"
              (org-export-data (org-element-property :tag item) info)
              (or contents ""))
    (format "[%s],\n" (or contents ""))))

(defun org-typst-table (table contents info)
  "Export a basic Org TABLE with CONTENTS and export context INFO."
  (when (eq (org-element-property :type table) 'org)
    (format "#table(columns: %d,\n%s)\n"
            (cdr (org-export-table-dimensions table info)) contents)))

(defun org-typst-table-row (row contents info)
  "Export standard ROW CONTENTS, skipping rules and special rows."
  (when (and (eq (org-element-property :type row) 'standard)
             (not (org-export-table-row-is-special-p row info)))
    (concat (when (org-export-table-row-starts-header-p row info) "table.header(\n")
            contents
            (when (org-export-table-row-ends-header-p row info) "),\n"))))

(defun org-typst-table-cell (_cell contents _info)
  "Export cell CONTENTS as a Typst content argument."
  (format "[%s],\n" (or contents "")))

;;;###autoload
(defun org-typst-export-to-typst (&optional async subtreep visible-only body-only ext-plist)
  "Export to a .typ file using the standard Org export arguments.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY and EXT-PLIST are passed to Org."
  (interactive)
  (org-export-to-file 'typst (org-export-output-file-name ".typ" subtreep)
    async subtreep visible-only body-only ext-plist))

;;;###autoload
(defun org-typst-export-to-pdf (&optional async subtreep visible-only body-only ext-plist)
  "Export to .typ, then compile to PDF, keeping both files.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY and EXT-PLIST are passed to Org.
Return the PDF filename, or use the export stack when ASYNC is non-nil."
  (interactive)
  (org-export-to-file 'typst (org-export-output-file-name ".typ" subtreep)
    async subtreep visible-only body-only ext-plist #'org-typst-compile))

(defun org-typst-compile (file)
  "Compile Typst FILE to a sibling PDF and return its filename.
Keep FILE and capture compiler diagnostics in `*Org Typst PDF Output*'.
Requires the typst executable on `exec-path'."
  (let* ((file (expand-file-name file))
         (pdf (concat (file-name-sans-extension file) ".pdf"))
         (log (get-buffer-create "*Org Typst PDF Output*")))
    (with-current-buffer log (erase-buffer))
    (unless (equal (call-process "typst" nil (list log t) nil
                                 "compile" file pdf)
                   0)
      (error "Typst compilation failed; see *Org Typst PDF Output*"))
    pdf))

(provide 'ox-typst)

;;; ox-typst.el ends here
