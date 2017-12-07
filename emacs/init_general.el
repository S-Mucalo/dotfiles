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

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))


;; Remove line break at end of line
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))




;; (defun dot-emacs (relative-path)
;;       "Return the full path of a file in the user's emacs directory."
;;       (expand-file-name (concat user-emacs-directory relative-path)))

(setq package-archives '(;; ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
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

                                        ; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(setq auto-revert-verbose nil) ;; or this
(global-set-key (kbd "<f5>") 'revert-buffer)

                                        ; mark and edit all copies of the marked region simultaniously.
(use-package iedit
  :ensure t)

                                        ; if you're windened, narrow to the region, if you're narrowed, widen
                                        ; bound to C-x n
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
    Intelligently means: region, org-src-block, org-subtree, or defun,
    whichever applies first.
    Narrowing to org-src-block actually calls `org-edit-src-code'.

    With prefix P, don't widen, just narrow even if buffer is already
    narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-." . ace-jump-mode))



(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config)
  (use-package smartparens-html)
  (use-package smartparens-python)
  (use-package smartparens-latex)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :bind
  ( ("C-<down>" . sp-down-sexp)
    ("C-<up>"   . sp-up-sexp)
    ("M-<down>" . sp-backward-down-sexp)
    ("M-<up>"   . sp-backward-up-sexp)
    ("C-M-a" . sp-beginning-of-sexp)
    ("C-M-e" . sp-end-of-sexp)))


(use-package simple-mpc
  :ensure t)
(use-package mingus
  :ensure t)


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

(use-package counsel
:ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))


(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; (use-package avy
;; :ensure t
;; :bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))
