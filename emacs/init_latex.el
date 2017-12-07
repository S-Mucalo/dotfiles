    (use-package latex
      :mode ("\\.tex\\'" . latex-mode)
      :commands (latex-mode LaTeX-mode plain-tex-mode)
      :bind (:map LaTeX-mode-map
                  ("C-c C-r" . reftex-query-replace-document)
                  ("C-c C-g" . reftex-grep-document))
      :init
      (progn
        (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
        (add-hook 'LaTeX-mode-hook #'flyspell-mode)
        (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
        (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
        (add-hook 'LaTex-mode-hook #'LaTex-math-mode)
        (add-hook 'text-mode-hook #'turn-on-auto-fill)
        (setq TeX-auto-save t
              TeX-save-query nil
              TeX-show-compilation t
              TeX-parse-self t
              TeX-source-correlate-start-server t
              TeX-save-query nil
              TeX-PDF-mode t)
        (setq-default TeX-master nil))
      :config
      (add-to-list 'TeX-command-list
                   '("Sage" "sage %s.sagetex.sage" TeX-run-command nil t :help "Run SAGE.") t)
      (add-to-list 'TeX-command-list
                   '("Wordcount" "texcount %t" 'TeX-run-shell nil t :help "Run texcount.") t)
      (add-to-list 'TeX-command-list
                   '("Pythontex" "python /usr/share/texmf-dist/scripts/pythontex/pythontex.py %t" 'TeX-run-shell nil t :help "Run pythontex.") t)
      (add-to-list 'TeX-command-list
                   '("Depythontex" "python /usr/share/texmf-dist/scripts/pythontex/depythontex.py %t" 'TeX-run-shell nil t :help "Run depythontex.") t)
      (add-to-list 'TeX-command-list
                   '("Mk" "latexmk -pdf %s" 'TeX-run-TeX nil t :help "Run Latexmk on file") t))

    (use-package preview
      :commands LaTeX-preview-setup
      :init
      (progn
        (setq-default preview-scale 1.4
              preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))))

    (use-package reftex
      :commands turn-on-reftex
      :init
      (progn
        (setq reftex-plug-into-AUCTeX t
              reftex-extra-bindings t)))

    (use-package bibtex
      :mode ("\\.bib" . bibtex-mode)
      :init
      (progn
        (setq bibtex-align-at-equal-sign t)
        (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

(setq-default TeX-master nil);
