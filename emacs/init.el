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

(defun dotfiles (relative-path)
  "Return the full path of a file in the user's dotfiles directory."
  (substitute-in-file-name (concat "$HOME/.dotfiles/emacs/" relative-path)))


(setq custom-file (dot-emacs "custom.el"))
(load custom-file 'noerror)

(load (dotfiles "init_general.el"))
(load (dotfiles "init_latex.el"))
(load (dotfiles "init_python.el"))
(load (dotfiles "init_mail.el"))
(load (dotfiles "init_R.el"))
(load (dotfiles "init_org.el"))
(load (dotfiles "init_dired.el"))
;; (load (dotfiles "init_exwm.el"))

;; (load (dot-emacs "init/init_general.el"))
;; (load (dot-emacs "init/init_latex.el"))
;; (load (dot-emacs "init/init_python.el"))
;; (load (dot-emacs "init/init_mail.el"))
;; (load (dot-emacs "init/init_R.el"))
;; (load (dot-emacs "init/init_org.el"))
;; (load (dot-emacs "init/init_dired.el"))
;; (load (dot-emacs "init/init_c.el"))
;; (load (dot-emacs "init/init_exwm.el"))

;; (defun load-directory (dir)
;;   (let ((load-it (lambda (f)
;;            (load-file (concat (file-name-as-directory dir) f)))
;;          ))
;;     (mapc load-it (directory-files dir nil "\\.el$"))))
;; (load-directory (dot-emacs "init"))
