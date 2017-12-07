
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
