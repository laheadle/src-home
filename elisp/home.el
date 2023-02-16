(set-register ?d '(file . "/laheadle@drive.stinkless.org:/home/laheadle"))
(set-register ?d '(file . "/ssh:laheadle@drive.stinkless.org:/home/laheadle"))
(set-register ?r '(file . "/ssh:root@drive.stinkless.org:/etc"))
;; new stinkless. password is on the website.
(set-register ?s '(file . "/ssh:root@45.14.114.21:/etc"))
;;  this works with regular old ssh
(set-register ?t '(file . "/ssh:laheadle@45.14.114.21:/home/laheadle"))
(set-register ?a '(file . "/ssh:root@192.168.86.22#2222:/data/data/org.galexander.sshd/files"))
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

(when window-system
      (set-frame-position (selected-frame) 50 40)
      (set-frame-size (selected-frame) 131 42))