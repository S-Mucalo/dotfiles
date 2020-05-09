(let ((file-name-handler-alist nil))
  ;; temporarily increase garbage collection threshold and turn off file name handler regexp
  (setq gc-cons-threshold-original gc-cons-threshold)
  (setq gc-cons-threshold (* 1024 1024 100))
  (require 'org)
  (setq vc-follow-symlinks t)                  ; Just do it
  (org-babel-load-file (expand-file-name "settings.org"
                                         user-emacs-directory))

  (run-with-idle-timer
   5 nil
   (lambda ()
     (setq gc-cons-threshold gc-cons-threshold-original)
     (makunbound 'gc-cons-threshold-original)))
  )
