(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))

(setq custom-file (dot-emacs "custom.el"))
(load custom-file 'noerror)

(load (dot-emacs "init_general.el") 'noerror)
(load (dot-emacs "init_latex.el") 'noerror)
(load (dot-emacs "init_python.el") 'noerror)
(load (dot-emacs "init_mail.el") 'noerror)
(load (dot-emacs "init_theme.el") 'noerror)
(load (dot-emacs "init_R.el") 'noerror)
(load (dot-emacs "init_org.el") 'noerror)
(load (dot-emacs "init_dired.el") 'noerror)
(load (dot-emacs "init_c.el") 'noerror)


;; (require 'load-directory)
;; (load-directory (dot-emacs "init"))

(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory (dot-emacs "init"))
