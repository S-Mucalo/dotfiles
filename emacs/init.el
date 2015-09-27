(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))

; list the packages you want
(setq package-list '(lua-mode 
		     pkgbuild-mode 
		     smartparens 
		     window-number 
		     yasnippet 
		     magit))


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(global-linum-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
;; (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
;; (setq TeX-view-program-list '(("Zathura" "zathura --page-index=%(outpage) %o")))


;; (setq TeX-view-program-list
;;       '(("zathura" "zathura"
;; 		   (mode-io-correlate "--page %(outpage)")
;; 		   " %o")))
;; (setq TeX-view-program-list '(("Zathura" "zathura %o")))
;; (setq TeX-view-program-selection '(((output-dvi style-pstricks)
;;                                     "dvips and gv")
;;                                    (output-dvi "xdvi")
;;                                    (output-pdf "zathura")
;;                                    (output-html "xdg-open")))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-start-server t)
(setq reftex-plug-into-auctex t)

;; So that RefTeX finds my bibliography
(setq reftex-default-bibliography '("~/projects/Reference_Library/references.bib"))
;; So that RefTeX also recognizes \addbibresource. not reliable.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
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
;; (menu-bar-mode -1)

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
;; (add-hook 'org-mode-hook
;;       '(lambda ()
;;          (delete '("\\.pdf\\'" . default) org-file-apps)
;;          (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-startup-indented t)
;; (setq org-hide-leading-stars nil)
(setq org-agenda-files (list "~/org/work.org"
			     "~/org/school.org"
			     "~/org/home.org"))

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; (setq auto-save-file-name-transforms
;;       '((".*" , temporary-file-directory t)))

;; (setq auto-save-file-name-transforms 
;;       '((".*", "~/.emacs.d/autosaves)))
;; (make-directory "~/.emacs.d/autosaves/" t)


(setq custom-file "~/.emacs.d/custom.el")
(setq smartparens-init "~/.emacs.d/smartparens-init.el")
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)
(put 'overwrite-mode 'disabled t)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>")) 
     
;; Goto-line shortcut key
(global-set-key "\M-l" 'goto-line)

(setq column-number-mode t)
(require 'cc-mode)

(c-toggle-hungry-state)

(setq c-default-style "ellemtel"
      c-basic-offset 4)
(require 'flymake)
(add-hook 'c-mode-common-hook
	  (lambda()
	    (flymake-mode t)
	    (global-set-key [f5] 'flymake-display-err-menu-for-current-line)
	    (global-set-key [f6] 'flymake-goto-next-error)))


;; (require 'pymacs)
;; (eval-after-load "python"
;;   '(progn
;;      (pymacs-load "ropemacs" "rope-")))

;; Python Hook
(add-hook 'python-hook
          #'(lambda ()
	      (setq indent-tabs-mode nil
		    default-tab-width 4)))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)

(scroll-bar-mode -1)
(tool-bar-mode -1)

(load custom-file 'noerror)

;; SageTeX setup
;; This adds the command sage when in LaTeX mode (to invoke type C-C C-c sage)
(eval-after-load "tex"
'(setq TeX-command-list
(append TeX-command-list
(list
 (list "Sage" "sage %s.sagetex.sage" 'TeX-run-command nil t :help "Run SAGE.")
 (list "Wordcount" "texcount %t" 'TeX-run-shell nil t :help "Run texcount.")
 (list "Pythontex" "python /usr/share/texmf-dist/scripts/pythontex/pythontex.py %t" 'TeX-run-shell nil t :help "Run pythontex.")
 (list "Depythontex" "python /usr/share/texmf-dist/scripts/pythontex/depythontex.py %t" 'TeX-run-shell nil t :help "Run depythontex.")
;; (list "Latexmk" "latexmk -pdf %s" 'TeX-run-TeX nil t :help "Run Latexmk on file")
))))

(require 'ein)

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


;; load color theme only if frame started as x window

;; last t is for NO-ENABLE
(load-theme 'wheatgrass t t)
;; (load-theme 'terminal-theme t t)

(defun mb/pick-color-theme (frame)
  (select-frame frame)
  (if (window-system frame)
      (progn 
	;; (disable-theme 'terminal-theme) ; in case it was active
	(enable-theme 'wheatgrass) 
	)
    (progn
      ;; (enable-theme 'terminal-theme) ; in case it was active
      (disable-theme 'wheatgrass))))
(add-hook 'after-make-frame-functions 'mb/pick-color-theme)

;; For when emacs or emacs -nw rather than emacs --daemon
(if window-system
    (enable-theme 'wheatgrass)
  ; (enable-theme 'terminal-theme)
  )

(load smartparens-init 'noerror)
