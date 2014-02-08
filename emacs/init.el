(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(global-linum-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '(((output-dvi style-pstricks)
                                    "dvips and gv")
                                   (output-dvi "xdvi")
                                   (output-pdf "Evince")
                                   (output-html "xdg-open")))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'TeX-correlate-mode)

(setq TeX-source-correlate-start-server t)
(setq reftex-plug-into-auctex t)

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
		       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))
		       ))
(real-global-auto-complete-mode t)
(define-key ac-complete-mode-map [tab] 'ac-expand)
(ac-flyspell-workaround)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra")) ;; --sug-mode=fast,normal
(setq ispell-local-dictionary "en_GB")

(setq global-visual-line-mode t)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(require 'org-install)
;; (require 'org-src) ;; edit src inline

(add-hook 'org-mode-hook
	  (lambda()
	    (toggle-truncate-lines)))


;; PDFs visited in Org-mode are opened in Evince (and not in the default choice) http://stackoverflow.com/a/8836108/789593
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-startup-indented t)
;; (setq org-hide-leading-stars nil)
(setq org-agenda-files (list "~/org/work.org"
			     "~/org/school.org"
			     "~/org/home.org"))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq custom-file "~/.emacs.d/custom.el")

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>")) 
     
;; Goto-line shortcut key
(global-set-key "\M-l" 'goto-line)

(setq column-number-mode t)

;; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            default-tab-width 4))))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

(scroll-bar-mode -1)
(tool-bar-mode -1)

(load custom-file 'noerror)

;; SageTeX setup
;; This adds the command sage when in LaTeX mode (to invoke type C-C C-c sage)
(eval-after-load "tex"
'(setq TeX-command-list
(append TeX-command-list
(list
(list "sage" "sage %s.sagetex.sage" 'TeX-run-command nil t :help "Run SAGE on the SAGE file corresponding to this LaTeX file (run latex first).")))))

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                               auto-mode-alist))

;; Global minor mode eval-after-load
(eval-after-load "yasnippet-autoloads" 
 '(progn
    (if (require 'yasnippet nil t)
        (progn
          (yas-global-mode 1) 
	   )
       (warn "yasnippet not found."))))

(eval-after-load "window-number-autoloads" 
 '(progn
    (if (require 'window-number nil t)
        (progn
          (window-number-mode 1) 
	  (window-number-meta-mode 1)
	   )
       (warn "window-number not found."))))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)
