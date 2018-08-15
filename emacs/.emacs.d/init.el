(require 'org)
(setq vc-follow-symlinks t)                  ; Just do it
(org-babel-load-file (expand-file-name "settings.org"
                    user-emacs-directory))
