(use-package elfeed)

(use-package elfeed-org
  :init (progn
          ;; Load elfeed-org
          (require 'elfeed-org)

          ;; Initialize elfeed-org
          ;; This hooks up elfeed-org to read the configuration when elfeed
          ;; is started with =M-x elfeed=
          (elfeed-org)

          (setq rmh-elfeed-org-files (list "~/doc/org/non-agenda/elfeed.org"))))

(setq org-user-agenda-files (quote ("~/doc/org/1" "~/doc/org"
                                    "~/doc/org/2")))
(setq org-directory "~/doc/org")

(setq org-default-notes-file "~/doc/org/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/doc/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/doc/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/doc/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/doc/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/doc/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/doc/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/doc/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/doc/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-global-properties (quote (("Effort_ALL" . "0:07 0:15 0:30 1:00 2:00 4:00 6:00 10:00 16:00 0:00")
                                    ("STYLE_ALL" . "habit"))))


(setq org-tag-alist (quote (("FLAGGED" . ??)
                            ("MYWORK" . ?w)
                            ("FAMILY" . ?f)
                            ("SELF" . ?s)
                            ("ADRI" . ?a)
                            ("META" . ?m)
                            ("FRIENDS" . ?r)
                            ("READING" . ?d)
                            )))

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
