(set-register ?d '(file . "/laheadle@drive.stinkless.org:/home/laheadle"))
(set-register ?r '(file . "/root@drive.stinkless.org:/etc"))
(set-register ?c '(file . "/sudo::/etc"))
(defun remote-flash-shell ()
  (interactive)
  (let ((od default-directory))
    (setq default-directory "/laheadle@flash.stinkless.org:/")
    (unwind-protect
        (call-interactively 'shell)
      (setq default-directory od))))

(global-set-key "\C-c\C-f" 'remote-flash-shell)

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))
