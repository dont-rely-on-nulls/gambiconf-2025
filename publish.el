;;; publish.el --- Build Beamer PDF from Org file

;; Load required packages
(require 'citeproc)
(require 'find-lisp)
(require 'ox-beamer)
(require 'ox-latex)
(require 'seq)

;; Functions
(defun patch-list-with-prefix (prefix strings)
  "Prepend PREFIX to every string in STRINGS."
  (mapcar (lambda (s) (concat prefix s)) strings))

;; Paths
(setq-default root-dir (concat (getenv "PWD") "/"))

;; Configure LaTeX export settings
(setq org-latex-to-pdf-process 
  '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Set up LaTeX compiler
(setq org-latex-compiler "pdflatex")

;; Enable verbose output for debugging
(setq org-latex-logfiles-extensions 
      '("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))

;;;; Fix bibliography
(setq-default bibtex-dir (concat root-dir "refs"))
(setq org-cite-refs-list '("refs.bib"))
(setq org-cite-refs-path (patch-list-with-prefix (concat bibtex-dir "/") org-cite-refs-list))
(setq org-cite-global-bibliography org-cite-refs-path)
(setq org-cite-export-processors '((latex biblatex)
                                   (moderncv basic)
                                   (html csl)
                                   (t csl)))

;; Ensure beamer class is available
(unless (assoc "beamer" org-latex-classes)
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; Define the publishing project
(setq org-publish-project-alist
      '(("beamer-presentation"
         :base-directory "./"
         :base-extension "org"
         :publishing-directory "./public"
         :publishing-function org-beamer-publish-to-pdf
         :section-numbers nil
         :with-toc nil
         :exclude-tags ("noexport")
         :auto-sitemap nil)))

;; Function to publish the presentation
(defun build-presentation ()
  "Build the beamer presentation to PDF."
  (interactive)
  ;; Try to publish with error handling
  (condition-case err
      (progn
        (org-publish-project "beamer-presentation" t)
        (message "Presentation built successfully!"))
    (error 
     (message "Error building presentation: %s" (error-message-string err))
     nil)))

;; Alternative direct export function
(defun build-presentation-direct ()
  "Build presentation directly from current org file."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (condition-case err
        (progn
          (org-beamer-export-to-pdf)
          (message "Direct export completed!"))
      (error
       (message "Direct export failed: %s" (error-message-string err))))))

;; If running as script, build immediately
(when noninteractive
  (build-presentation))

;;; publish.el ends here
