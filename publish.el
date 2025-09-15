;; package --- Summary
;; publish.el --- Build the Beamer PDF from an org file
;;; Commentary:
;;;
;;; How to build this:
;;;    make build

;; Required packages
(require 'citeproc)
(require 'find-lisp)
(require 'ox-beamer)
(require 'ox-latex)
(require 'seq)

;;; CODE:
;; ===================
;; UTIL FUNCTIONS
;; ===================
(defun patch-list-with-prefix (prefix strings)
  "Prepend PREFIX to every string in STRINGS."
  (mapcar (lambda (s) (concat prefix s)) strings))

;; ===================
;; SETTINGS
;; ===================
;;; Paths
(setq-default root-dir (concat (getenv "PWD") "/"))
(setq-default tex-dir (concat root-dir "tex"))

(message (format "SETTING ROOT DIR: %s" root-dir))
(message (format "SETTING TEXT DIR: %s" tex-dir))

;;; Configure LaTeX export settings
(setq org-latex-pdf-process (list
   "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

;;; Set up LaTeX compiler
(setq org-latex-compiler "pdflatex")
(setq org-latex-remove-logfiles t)
(setq org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls"
	                                  "figlist" "idx" "log" "nav" "out" "ptc"
	                                  "run.xml" "snm" "toc" "vrb" "xdv"))

;;; Setup bibliography with Bibtex
(setq-default org-cite-refs-list '("references.bib"))
(setq-default org-cite-refs-path (patch-list-with-prefix (concat root-dir "/") org-cite-refs-list))
(setq org-cite-global-bibliography org-cite-refs-path)
(setq org-cite-export-processors '((latex biblatex)
                                   (moderncv basic)
                                   (html csl)
                                   (t csl)))

;;; Babel settings
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (dot . t)
   (emacs-lisp . t)
   (eshell . t)
   (latex . t)
   (org . t)
   (shell   . t)))

;; ===================
;; MAIN
;; ===================
;;; Org-publish settings
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

;;; If running as script, build immediately
(when noninteractive
  (build-presentation))

;;; publish.el ends here
