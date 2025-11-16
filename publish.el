;;; publish.el --- Build Beamer PDF presentations from Org files -*- lexical-binding: t; -*-

;;; Commentary:

;; This script builds Beamer PDF presentations from Org-mode files using
;; org-beamer and LaTeX.  It's designed to be run both interactively in Emacs
;; and non-interactively as part of a build pipeline (e.g., Makefile, Nix).
;; Usage:
;;
;;   Interactive (from within Emacs):
;;     M-x build-presentation
;;     M-x build-presentation-direct
;;
;;   Non-interactive (command line):
;;     make build
;;
;; Build Requirements:
;;   - Emacs with org-mode and ox-beamer
;;   - LaTeX distribution (TeX Live recommended) with:
;;     * lualatex or pdflatex
;;     * latexmk
;;     * Beamer document class
;;     * biblatex (for bibliography support)
;;   - citeproc-el package (for citation processing)
;;   - [Optional] Nix, but it makes all of the deps above irrelevant.
;;
;; Output:
;;   - PDF file(s) in either $(pwd)/public/ or $(pwd)/result
;;   - LaTeX intermediate files (cleaned up by org-latex-remove-logfiles)

;;; Code:

;; ==========================
;; DEPENDENCIES
;; ==========================

(require 'citeproc)
(require 'find-lisp)
(require 'ox-beamer)
(require 'ox-latex)
(require 'seq)

;; ==========================
;; UTILITY FUNCTIONS
;; ==========================

(defun patch-list-with-prefix (prefix strings)
  "Prepend PREFIX to each string in STRINGS list."
  (mapcar (lambda (s) (concat prefix s)) strings))

;; ==========================
;; PATH CONFIGURATION
;; ==========================

;; Root directory: base path for all relative paths in the project
;; Uses PWD environment variable or defaults to current directory
(setq-default root-dir (concat (or (getenv "PWD") default-directory) "/"))

;; TeX directory: location of LaTeX style files and templates
(setq-default tex-dir (concat root-dir "tex"))

;; Log directory paths for debugging
(message (format "SETTING ROOT DIR: %s" root-dir))
(message (format "SETTING TEX DIR: %s" tex-dir))

;; ==========================
;; LATEX CONFIGURATION
;; ==========================

(setq org-latex-pdf-process
      (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f %f"))

;; LaTeX compiler: can be "pdflatex", "lualatex", or "xelatex"
;; Note: This setting may be overridden by org-latex-pdf-process above
(setq org-latex-compiler "pdflatex")

;; Automatically remove LaTeX auxiliary files after successful compilation
;; Keeps the build directory clean
(setq org-latex-remove-logfiles t)

;; Extensions of files to remove during cleanup
;; These are intermediate files created during LaTeX compilation
(setq org-latex-logfiles-extensions
      '("aux"
        "bcf"
        "blg"
        "fdb_latexmk"
        "fls"
        "figlist"
        "idx"
        "log"
        "nav"
        "out"
        "ptc"
        "run.xml"
        "snm"
        "toc"
        "vrb"
        "xdv"))

;; ==========================
;; BIBLIOGRAPHY CONFIGURATION
;; ==========================

;; List of bibliography files (relative to root directory)
;; Add more .bib files here as needed
(setq-default org-cite-refs-list '("references.bib"))

;; Convert relative paths to absolute paths
(setq-default org-cite-refs-path
              (patch-list-with-prefix root-dir org-cite-refs-list))

;; Global bibliography files used by org-cite
(setq org-cite-global-bibliography org-cite-refs-path)

(setq org-cite-export-processors
      '((latex biblatex)
        (moderncv basic)
        (html csl)
        (t csl)))

;; ==========================
;; BABEL CONFIGURATION
;; ==========================

;; Disable confirmation prompts when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

;; Enable Babel support for various languages
;; Code blocks in these languages can be executed within Org documents
(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (dot . t)
   (emacs-lisp . t)
   (eshell . t)
   (latex . t)
   (org . t)
   (shell . t)))

;; ==========================
;; ORG-PUBLISH CONFIGURATION
;; ==========================

;; Org-publish project definition for Beamer presentations
;; This allows batch processing of multiple Org files
(setq org-publish-project-alist
      '(("beamer-presentation"
         ;; Source directory containing .org files
         :base-directory "./"
         
         ;; Only process files with .org extension
         :base-extension "org"
         
         ;; Output directory for generated PDFs
         :publishing-directory "./public"

         :publishing-function org-beamer-publish-to-pdf
         :section-numbers nil
         :with-toc nil
         :exclude-tags ("noexport")
         :auto-sitemap nil)))

;; ==========================
;; BUILD FUNCTIONS
;; ==========================

(defun build-presentation ()
  "Build the Beamer presentation to PDF using org-publish."
  (interactive)
  (condition-case err
      (progn
        ;; Force rebuild (t argument) to ensure fresh compilation
        (org-publish-project "beamer-presentation" t)
        (message "✓ Presentation built successfully!")
        t)
    (error
     (message "✗ Error building presentation: %s" (error-message-string err))
     ;; In non-interactive mode, exit with error code
     (when noninteractive
       (kill-emacs 1))
     nil)))

(defun build-presentation-direct ()
  "Build presentation directly from the current Org buffer.
The PDF will be created in the same directory as the .org file"
  (interactive)
  (if (derived-mode-p 'org-mode)
      (condition-case err
          (progn
            (org-beamer-export-to-pdf)
            (message "✓ Direct export completed!")
            t)
        (error
         (message "✗ Direct export failed: %s" (error-message-string err))
         (when noninteractive
           (kill-emacs 1))
         nil))
    (message "✗ Not in an org-mode buffer!")
    nil))

;; ==========================
;; AUTO-EXECUTION
;; ==========================

;; When running as a non-interactive batch script (e.g., from Makefile),
;; automatically trigger the build process
(when noninteractive
  (build-presentation))

;;; publish.el ends here
