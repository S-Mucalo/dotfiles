

(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq mouse-yank-at-point t)
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=fast")) ;; --sug-mode=fast,normal
(setq ispell-local-dictionary "en_GB")
(setq global-visual-line-mode t)
(setq column-number-mode t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq mouse-autoselect-window t)

;; Stop doing bad things
(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)
(put 'overwrite-mode 'disabled t)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ;; ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Goto-line shortcut key
(global-set-key "\M-l" 'goto-line)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80)
(global-hl-line-mode t)

;; Highlight current region
(transient-mark-mode t)

;; Add the system clipboard to the kill ring
(setq save-interprogram-paste-before-kill t)


(setq custom-file (dot-emacs "custom.el"))
(load custom-file 'noerror)


(use-package lua-mode
    :mode ("\\.lua\\'" . lua-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package monokai-theme
  :ensure t
  :disabled t)

(use-package grandshell-theme
  :ensure t)

(use-package cyberpunk-theme
  :ensure t
  :disabled t)

(use-package company
  :ensure t
  :init
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package ido
  :init (progn
      (ido-mode 1)
      ;; "~" adds the "/" automatically in find file, etc.
      (add-hook 'ido-setup-hook
            (lambda ()
              ;; Go straight home
              (define-key ido-file-completion-map
            (kbd "~")
            (lambda ()
              (interactive)
              (if (looking-back "/")
                  (insert "~/")
                (call-interactively 'self-insert-command)))))))

  :config
  (progn (setq ido-enable-prefix nil)
     (setq ido-enable-flex-matching t)
     (setq ido-create-new-buffer 'always)
     (setq ido-use-filename-at-point 'guess)
     (setq ido-max-prospects 10)))

(use-package which-key
  :config
  (which-key-mode t)
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-." . ace-jump-mode))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :ensure t)

(use-package visual-regexp
  :ensure t
  :bind
  ("M-%" . vr/query-replace))

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command))

(use-package window-number
  :ensure t
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1))

(use-package comint
  :bind (:map comint-mode-map
              ("<up>" . comint-previous-matching-input-from-input) ;; Untested
              ("<down>" . comint-next-matching-input-from-input)  ;; Untested
              ("M-p" . comint-previous-matching-input-from-input)
              ("M-n" . comint-next-matching-input-from-input)
              ;; ("C-up" . comint-previous-matching-input-from-input)
              ;; ("C-down" . comint-next-matching-input-from-input)
              ))


;; Remember the last visited line in a file
(setq-default save-place t)
(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/places"))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(setq recentf-max-saved-items 50)
(add-to-list 'recentf-exclude "/\\.git/.*\\")         ; ignore git contents
(add-to-list 'recentf-exclude ".*/elpa/.*\\")           ; package files
(add-to-list 'recentf-exclude "/el-get/.*\\")           ; package files
(add-to-list 'recentf-exclude "/auto-save-list/.*\\")   ; auto-save junk
(add-to-list 'recentf-exclude "TAGS")
(add-to-list 'recentf-exclude ".*-autoloads\\.el\\'")
(add-to-list 'recentf-exclude ".*\\.gz\\'")
(add-to-list 'recentf-exclude "ido.last")
(add-to-list 'recentf-exclude "session\\.[a-f0-9]*$")
(add-to-list 'recentf-exclude "\\.aux$")
(add-to-list 'recentf-exclude "/COMMIT_EDITMSG$")
(recentf-cleanup)

(show-paren-mode 1)
(setq show-paren-delay 0)



(use-package org
  :mode
  ("\\.org$" . org-mode)
  :init
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  :config
  (org-babel-do-load-languages 'org-babel-do-load-languages '((python . t)))
  (setq org-log-done t)
  (setq org-startup-indented t)
  (setq org-agenda-files (list  "~/org/work.org"
                "~/org/school.org"
                "~/org/home.org"))

  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package cc-mode
  :config
  (setq c-default-style "ellemtel"
    c-basic-offset 4
    c-toggle-hungry-state))

(use-package flymake
  :config
  (flymake-mode t)
  :bind
  ("<f5>" . flymake-display-err-menu-for-current-line)
  ("<f6>" . flymake-goto-next-error))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq indent-tabs-mode nil)
  (setq default-tab-width 4)
  (setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "-i"))


(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode)))

(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable))
  )

;; LaTeX
;; Basic settings
(use-package latex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :bind (:map LaTeX-mode-map
              ("C-c C-r" . reftex-query-replace-document)
              ("C-c C-g" . reftex-grep-document))
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
    (add-hook 'LaTex-mode-hook #'LaTex-math-mode)
    (add-hook 'text-mode-hook #'turn-on-auto-fill)
    (setq TeX-auto-save t
          TeX-save-query nil
          TeX-show-compilation t
          TeX-parse-self t
          TeX-source-correlate-start-server t
          TeX-save-query nil
          TeX-PDF-mode t)
    (setq-default TeX-master nil))
  :config
  (add-to-list 'TeX-command-list
               '("Sage" "sage %s.sagetex.sage" TeX-run-command nil t :help "Run SAGE.") t)
  (add-to-list 'TeX-command-list
               '("Wordcount" "texcount %t" 'TeX-run-shell nil t :help "Run texcount.") t)
  (add-to-list 'TeX-command-list
               '("Pythontex" "python /usr/share/texmf-dist/scripts/pythontex/pythontex.py %t" 'TeX-run-shell nil t :help "Run pythontex.") t)
  (add-to-list 'TeX-command-list
               '("Depythontex" "python /usr/share/texmf-dist/scripts/pythontex/depythontex.py %t" 'TeX-run-shell nil t :help "Run depythontex.") t)
  (add-to-list 'TeX-command-list
               '("Mk" "latexmk -pdf %s" 'TeX-run-TeX nil t :help "Run Latexmk on file") t))

(use-package preview
  :commands LaTeX-preview-setup
  :init
  (progn
    (setq-default preview-scale 1.4
          preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))))

(use-package reftex
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t
          reftex-extra-bindings t)))

(use-package bibtex
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))



(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)))
(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))

;; (use-package ess
;;   :ensure t
;;   :init (use-package 'ess-site)
;;   :bind (:map ess-mode-map
;;               ([(shift return)] . my-ess-eval)
;;               ("C-up"

;;   :config
;;   (setq ess-local-process-name "R"
;;         ansi-color-for-comint-mode 'filter
;;         comint-scroll-to-bottom-on-input t
;;         comint-scroll-to-bottom-on-output t
;;         comint-move-point-for-output t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))


(setq message-kill-buffer-on-exit t)
(setq mail-envelope-from (quote header))
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from (quote header))
(setq send-mail-function (quote sendmail-send-it))

(use-package notmuch
  :ensure t)
;; (require 'notmuch)
;; (autoload 'notmuch "notmuch" "notmuch mail" t)


(define-key notmuch-show-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-deleted"))
      (notmuch-show-tag (list "+deleted")))))

(define-key notmuch-search-mode-map "d"
  (lambda ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-show-tag (list "-deleted"))
      (notmuch-show-tag (list "+deleted")))))

(setq message-directory "~/.mail/")
(setq notmuch-fcc-dirs nil)
(setq notmuch-saved-searches
      (quote
       (
        (:name "UC_mail-recent" :query "tag:UC_mail-inbox and date:week.." :key "r")
        (:name "UC_mail-unread" :query "tag:UC_mail-inbox and tag:unread")
        (:name "UC_mail-inbox" :query "tag:UC_mail-inbox" :key "i" :search-type tree)
        (:name "UC_mail-sent" :query "tag:UC_mail-sent" :key "s")
        (:name "UC_mail-drafts" :query "tag:UC_mail-drafts" :key "d")
        (:name "gmail-unread" :query "tag:gmail-inbox and tag:unread")
        (:name "gmail-inbox" :query "tag:gmail-inbox" :search-type tree)
        (:name "gmail-sent" :query "tag:gmail-sent")
        (:name "yahoo-unread" :query "tag:yahoo-inbox and tag:unread")
        (:name "yahoo" :query "tag:yahoo-inbox")
        (:name "Yahoo-sent" :query "tag:yahoo-sent")
        (:name "Petsc" :query "tag:petsc-users" :search-type tree)
        (:name "fipy" :query "tag:fipy-users" :search-type tree)
        (:name "all mail" :query "*" :key "a"))))
;; (add-hook 'notmuch-search-hook '(lambda () (goto-char(point-max))) 1 nil)


(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

;; Define two identities, "home" and "work"
(setq gnus-alias-identity-alist
      '(("UC_mail"
         nil ;; Does not refer to any other identity
         "Shaun Mucalo <shaun.mucalo@pg.canterbury.ac.nz>" ;; Sender address
         nil ;; No organization header
         nil ;; No extra headers
         nil ;; No extra body text
         nil) ;; "~/.signature")
        ("gmail"
         nil
         "Shaun Mucalo <shaunmucalo@gmail.com>"
         nil ;; "Example Corp."
         nil ;; (("Bcc" . "john.doe@example.com"))
         nil
         nil) ;; "~/.signature.work")
        ("yahoo"
         nil
         "Shaun Mucalo <s_mucalo@yahoo.co.nz>"
         nil ;; "Example Corp."
         nil ;; (("Bcc" . "john.doe@example.com"))
         nil
         nil) ;; "~/.signature.work")
        ))
;; Use "UC_mail" identity by default
(setq gnus-alias-default-identity "UC_mail")
;; Define rules to match work identity
                                        ; (setq gnus-alias-identity-rules)
                                        ; '(("UC_mail" ("any" "shaun.mucalo@\\(example\\.com\\|help\\.example.com\\)" both) "gmail"))
;; Determine identity when message-mode loads
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)


;; (use-package mu4e
;;   :ensure t)

(require 'mu4e)

(setq  mu4e-maildir "~/.mail"
       mu4e-sent-folder "/UC_mail/Sent Items"
       mu4e-drafts-folder "/UC_mail/Drafts"
       mu4e-trash-folder "/UC_mail/Deleted Items"
       user-mail-address "shaun.mucalo@pg.canterbury.ac.nz")

;; (setq mu4e-sent-messages-behavior 'delete)


(defvar my-mu4e-account-alist
  '(
    ("UC_mail"
     (mu4e-sent-folder "/UC_mail/Sent Items")
     (mu4e-drafts-folder "/UC_mail/Drafts")
     (mu4e-trash-folder "/UC_mail/Deleted Items")
     (user-mail-address "shaun.mucalo@pg.canterbury.ac.nz"))
    ("gmail"
     (mu4e-sent-folder "/gmail_mail/[Gmail].Sent Mail")
     (mu4e-trash-folder "/gmail_mail/[Gmail].Trash")
     (mu4e-drafts-folder "/gmail_mail/[Gmail].Drafts")
     (user-mail-address "shaunmucalo@gmail.com"))
    ("yahoo"
     (mu4e-sent-folder "/yahoo_mail/Sent")
     (mu4e-drafts-folder "/yahoo_mail/Drafts")
     (mu4e-trash-folder "/yahoo_mail/Trash")
     (user-mail-address "s_mucalo@yahoo.co.nz"))
    ))


(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))




(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)


(defun mu4e-UC-mail()
  (interactive)
  (message "UC-mail account")
  (setq user-mail-address "shaun.mucalo@pg.canterbury.ac.nz"
        mu4e-sent-folder "/UC_mail/Sent Items"
        mu4e-drafts-folder "/UC_mail/Drafts"
        mu4e-trash-folder "/UC_mail/Deleted Items")
  (setq  mu4e-maildir-shortcuts
        '( ("/UC_mail/INBOX"        . ?i)
           ("/UC_mail/Sent Items"   . ?s)
           ("/UC_mail/Deleted Items". ?t)
           ("/UC_mail/Drafts"       . ?d))))

(defun mu4e-gmail-mail()
  (interactive)
  (message "gmail-mail account")
  (setq
     mu4e-sent-folder "/gmail_mail/[Gmail].Sent Mail"
     mu4e-trash-folder "/gmail_mail/[Gmail].Trash"
     mu4e-drafts-folder "/gmail_mail/[Gmail].Drafts"
     user-mail-address "shaunmucalo@gmail.com"
     mu4e-maildir-shortcuts
     '( ("/gmail_mail/INBOX"                . ?i)
         ("/gmail_mail/[Gmail].Sent Mail"   . ?s)
         ("/gmail_mail/[Gmail].Trash"       . ?t)
         ("/gmail_mail/[Gmail].Drafts"      . ?d)
         )
))

(defun mu4e-yahoo-mail()
  (interactive)
  (message "yahoo-mail account")
  (setq mu4e-sent-folder "/yahoo_mail/Sent"
        mu4e-drafts-folder "/yahoo_mail/Drafts"
        mu4e-trash-folder "/yahoo_mail/Trash"
        user-mail-address "s_mucalo@yahoo.co.nz"
        mu4e-maildir-shortcuts
        '( ("/yahoo_mail/Inbox"  . ?i)
           ("/yahoo_mail/Sent"   . ?s)
           ("/yahoo_mail/Trash"  . ?t)
           ("/yahoo_mail/Drafts" . ?d)
           ))
  )

(define-key mu4e-main-mode-map (kbd "<f12>") 'mu4e-UC-mail)
(define-key mu4e-main-mode-map (kbd "<f11>") 'mu4e-gmail-mail)
(define-key mu4e-main-mode-map (kbd "<f10>") 'mu4e-yahoo-mail)


;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads")

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)

(setq mu4e-headers-date-format "%d-%m-%Y %H:%M")

;; Allow org-mode stuff in mu4e
(require 'org-mu4e)


;; (use-package bbdb
;;   :ensure t
;;   :config
;;   (autoload 'bbdb-insinuate-mu4e "bbdb-mu4e")
;;   (bbdb-initialize 'message 'mu4e))

(setq bbdb-mail-user-agent (quote message-user-agent))
(setq mu4e-view-mode-hook (quote (bbdb-mua-auto-update visual-line-mode)))
(setq mu4e-compose-complete-addresses nil)
(setq bbdb-mua-pop-up t)
(setq bbdb-mua-pop-up-window-size 5)



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

;; Remove line break at end of line
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(defun toggle-window-split()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x 4") 'toggle-window-split)

;; Rotate windows because you opened it in the wrong one
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(global-set-key (kbd "C-x 5") 'rotate-windows)


(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Make dired less verbose, toggle with ( )
(use-package dired-details
  :ensure t
  )
;; (require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
