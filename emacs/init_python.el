(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq indent-tabs-mode nil)
  (setq default-tab-width 4)
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "--simple-prompt -i"))


(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode)))

(use-package elpy
  :defer t
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable))
  )
