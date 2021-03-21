(defun my-voice-copy (from into)
  (shell-command (format "rsync -avz %s %s" from into)))

(defun my-voice-installation ()
  (format "%s/.talon/user/knausj_talon" (getenv "HOME")))
(defun my-voice-back-up ()
  (format "src/home/elisp" (getenv "HOME")))
(defun my-voice-install-configuration ()
  (interactive)
  (my-voice-copy (my-voice-back-up) (my-voice-installation)))

(defun my-voice-save-configuration ()
  (interactive)
  (my-voice-copy (my-voice-installation) (my-voice-back-up)))
