;;; ox-typst-test.el --- Focused export tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'ox-typst)

(defun org-typst-test-export (source)
  (org-export-string-as source 'typst t '(:with-broken-links t)))

(ert-deftest org-typst-full-document ()
  (should (equal (org-export-string-as "* Hello\nWorld.\n" 'typst)
                 "#heading(level: 1)[Hello]\nWorld\\.\n")))

(ert-deftest org-typst-text-and-headings ()
  (should (equal (org-typst-test-export "* One\n** Two\n*bold /nested/* _u_ +s+\n")
                 "#heading(level: 1)[One]\n#heading(level: 2)[Two]\n#strong[bold #emph[nested]] #underline[u] #strike[s]\n"))
  (should (equal (org-typst-plain-text "#x [y] $5 @z \\ 1. a_b" nil)
                 "\\#x \\[y\\] \\$5 \\@z \\\\ 1\\. a\\_b")))

(ert-deftest org-typst-raw-and-links ()
  (should (equal (org-typst-test-export "#+begin_src python\nprint(\"```\\n\")\n#+end_src\n")
                 "#raw(\"print(\\\"```\\\\n\\\")\\n\", block: true, lang: \"python\")\n"))
  (should (equal (org-typst-test-export "[[https://example.org/a?q=1&b=2][*site*]]\n")
                 "#link(\"https://example.org/a?q=1&b=2\")[#strong[site]]\n")))

(ert-deftest org-typst-tables ()
  (should (equal (org-typst-test-export "| A | B | C |\n|---+---+---|\n| 1 |   | 3 |\n")
                 "#table(columns: 3,\ntable.header(\n[A],\n[B],\n[C],\n),\n[1],\n[],\n[3],\n)\n"))
  (should (equal (org-typst-test-export "| a | b |\n| c | d |\n| e | f |\n")
                 "#table(columns: 2,\n[a],\n[b],\n[c],\n[d],\n[e],\n[f],\n)\n")))

(ert-deftest org-typst-lists ()
  (should (equal (org-typst-test-export "- *Fruit*\n  1. Apple\n  2. Pear\n- Bread\n")
                 "#list(\n[#strong[Fruit]\n#enum(\n[Apple\n],\n[Pear\n],\n)\n],\n[Bread\n],\n)\n"))
  (should (equal (org-typst-test-export "1. First\n\n   Second paragraph.\n2. Last\n")
                 "#enum(\n[First\n\nSecond paragraph\\.\n],\n[Last\n],\n)\n")))

(ert-deftest org-typst-description-lists ()
  (should (equal (org-typst-test-export
                  "- *Term* [x] :: /Description/\n  - Detail\n- Other :: More\n")
                 "#terms(\nterms.item([#strong[Term] \\[x\\]], [#emph[Description]\n#list(\n[Detail\n],\n)\n]),\nterms.item([Other], [More\n]),\n)\n")))

(ert-deftest org-typst-export-hatches ()
  (should (equal (org-typst-test-export
                  "#+begin_export typst\n  #set text(size: 12pt)\n  #align(center)[*Hello*]\n#+end_export\nAfter\n")
                 "#set text(size: 12pt)\n#align(center)[*Hello*]\nAfter\n"))
  (should (equal (org-typst-test-export
                  "A @@typst:#text(fill: red)[red]@@ word.\n")
                 "A #text(fill: red)[red] word\\.\n"))
  (should (equal (org-typst-test-export
                  "#+begin_export latex\n\\LaTeX\n#+end_export\nA@@html:<b>hidden</b>@@B\n")
                 "AB\n"))
  (should (equal (org-typst-test-export
                  "#+begin_src typst\n#text[code]\n#+end_src\n")
                 "#raw(\"#text[code]\\n\", block: true, lang: \"typst\")\n")))

(ert-deftest org-typst-images ()
  (should (equal (org-typst-test-export "[[file:sample image.PNG]]\n")
                 "#image(\"sample image.PNG\")\n"))
  (should (equal (org-typst-test-export
                  "#+CAPTION: A *bold* caption.\n#+ATTR_TYPST: :width 60% :height 25mm\n[[file:sample.svg]]\n")
                 "#figure(image(\"sample.svg\", width: 60%, height: 25mm), caption: [A #strong[bold] caption\\.])\n"))
  (should (equal (org-typst-test-export
                  "#+CAPTION: Not a standalone image\n#+ATTR_TYPST: :height 1em\nBefore [[file:sample.svg]] after\n")
                 "Before #image(\"sample.svg\", height: 1em) after\n"))
  (should (equal (org-typst-test-export
                  "#+CAPTION: Not a single image\n[[file:a.png]] [[file:b.png]]\n")
                 "#image(\"a.png\") #image(\"b.png\")\n"))
  (should (equal (org-typst-test-export "[[file:sample.svg][Description]]\n")
                 "Description\n"))
  (should (equal (org-typst-test-export "https://example.org/sample.png\n")
                 "#link(\"https://example.org/sample.png\")[https:\\/\\/example\\.org\\/sample\\.png]\n")))

(ert-deftest org-typst-unsupported ()
  (let ((out (org-typst-test-export
              "Text[fn:1].\n\n[fn:1] Hidden footnote.\n\n#+caption: Hidden caption\n[[file:missing.txt]]\n\n#+begin_quote\nHidden quote.\n#+end_quote\n")))
    (should (string-match-p "Text\\\\\\." out))
    (should-not (string-match-p "Hidden\\|missing\\|footnote\\|image" out))))
