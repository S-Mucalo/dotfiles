(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

(defun dot-emacs (relative-path)
  "Return the full path of a file in the user's emacs directory."
  (expand-file-name (concat user-emacs-directory relative-path)))

;; list the packages you want
(setq package-list '(lua-mode
                     pkgbuild-mode
                     window-number
                     yasnippet
                     magit
                     monokai-theme
                     grandshell-theme
                     cyberpunk-theme
                     company
                     ido-vertical-mode
                     visual-regexp
                     smex
                     ))



(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



(setq custom-file (dot-emacs "custom.el"))
(load custom-file 'noerror)

;; load color theme only early
(load-theme 'monokai t t); last t is for NO-ENABLE
(load-theme 'grandshell t t)
(load-theme 'wheatgrass t t)
(load-theme 'cyberpunk)
;; (enable-theme 'monokai)

;; Auto-completion COMPlete-ANY
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "C-x g") 'magit-status)

;; LaTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-start-server t)
(setq reftex-plug-into-auctex t)

;; So that RefTeX finds my bibliography - doesn't seem to be reliable
(setq reftex-default-bibliography '("~/projects/Reference_Library/references.bib"))
;; So that RefTeX also recognizes \addbibresource. not reliable.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))


(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=fast")) ;; --sug-mode=fast,normal
(setq ispell-local-dictionary "en_GB")

(setq global-visual-line-mode t)

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


(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

(require 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(require 'visual-regexp)
(define-key global-map (kbd "M-%") 'vr/query-replace)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)



(require 'org-install)
(require 'org-src) ;; edit src inline

(add-hook 'org-mode-hook
          (lambda()
            (toggle-truncate-lines)))


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



;; Stop doing bad things
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


(add-hook 'python-hook
          '(lambda ()
             (setq indent-tabs-mode nil
                   default-tab-width 4)))



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

;; (require 'ein)

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




(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))


(setq message-kill-buffer-on-exit t)
(setq mail-envelope-from (quote header))
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from (quote header))
(setq send-mail-function (quote sendmail-send-it))


(require 'notmuch)
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

                                        ; Automatically go home when pressing ~ in find file command
(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

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
(require 'dired-details)
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
