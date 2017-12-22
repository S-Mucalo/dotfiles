(use-package dired+
  :ensure t
  :config (require 'dired+)
  (setq diredp-hide-details-propagate-flag t)
  (setq diredp-hide-details-initially-flag t)
  )

;; Go to first real file in dired M-<
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

;; Go to last real file in dired M->
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
