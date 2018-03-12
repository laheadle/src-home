(set-register ?d '(file . "/laheadle@drive.stinkless.org:/home/laheadle"))
(set-register ?r '(file . "/root@drive.stinkless.org:/etc"))
(defun remote-flash-shell ()
  (interactive)
  (let ((od default-directory))
    (setq default-directory "/laheadle@flash.stinkless.org:/")
    (unwind-protect
        (call-interactively 'shell)
      (setq default-directory od))))

(global-set-key "\C-c\C-f" 'remote-flash-shell)
