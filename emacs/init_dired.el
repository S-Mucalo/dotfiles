(use-package dired+
  :ensure t
  :config (require 'dired+)
  )


;; Make dired less verbose, toggle with ( )
(use-package dired-details
  :ensure t
  )
;; (require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Go to first real file in dired M-<
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

;; Go to last real file in dired M->
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
