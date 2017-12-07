(use-package cc-mode
  :config
  (setq c-default-style "ellemtel")
  (setq c-basic-offset 4)
  ;; (setq c-toggle-hungry-state)
  )

(use-package flymake
  :config
  (flymake-mode t)
  :bind
  ("<f5>" . flymake-display-err-menu-for-current-line)
  ("<f6>" . flymake-goto-next-error))
