(use-package org
  :mode
  ("\\.org$" . org-mode)
  :init
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :config
  (org-babel-do-load-languages 'org-babel-do-load-languages '((python . t)))
  (setq org-log-done t)
  (setq org-startup-indented t)
  (setq org-agenda-files (list  "~/org/work.org"
                                "~/org/school.org"
                                "~/org/home.org"))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))
