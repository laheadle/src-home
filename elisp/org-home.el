(josh/make-org-refile-hydra josh/org-refile-hydra-file-a
                            "~/doc/org/1/family.org"
			    (("f" . "Family")
                             ("p" . "Personal")
                             ("m" . "Meta")
                             ("a" . "Adri")
                             ("t" . "Travel")
                             ("h" . "House")
                             ("c" . "children")
                             ("r" . "Friends")
                             ))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-b
                            "~/doc/org/diary.org"
			    (("e" . "MyEvents")))

(defhydra josh/org-refile-hydra (:foreign-keys run) 
  "Refile"
  ("f" josh/org-refile-hydra-file-a/body "Family" :exit t)
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(bind-key "r" 'josh/org-refile-hydra/body 'my-map)

(setq org-directory "~/doc/org")

(setq org-default-notes-file "~/doc/org/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/doc/org/refile.org")
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
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
