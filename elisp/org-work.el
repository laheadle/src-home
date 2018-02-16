(setq org-tag-alist '(
                      ("FLAGGED" . ??)
                      ("MATT" . ?m)
                      ("NOTE" . ?n)
                      ("RETRO" . ?r)
                      (:startgrouptag)
                      ("GOALS")
                      (:grouptags)
                      ("STRETCH")
                      ("CORPORATE")
                      ("PERSONAL")
                      (:endgrouptag)
                      (:startgrouptag)
                      ("STRETCH")
                      (:grouptags)
                      ("INTELLECTION")
                      ("CONTEXT")
                      ("STRATEGIC")
                      ("LEARNER")
                      ("INPUT")
                      (:endgrouptag)
                      (:startgrouptag)
                      ("PERSONAL")
                      (:grouptags)
                      ("TESTING")
                      ("ENGINEERING")
                      ("COLLABORATION")
                      (:startgrouptag)
                      ("CORPORATE")
                      (:grouptags)
                      ("WORKING_OUTSIDE_IN")
                      ("DRIVING_WITH_DATA")
                      ("EXECUTING_WITH_AGILITY")
                      ("PARTNERING_ACROSS_TEAMS")
                      (:endgrouptag)))


(setq org-directory "~/Workspace/docs/org")
(setq org-default-notes-file "~/Workspace/docs/org/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
;; U - inactive timestamp
;; a - link
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "review" entry (file+olp "~/Workspace/docs/org/work.org" "Review")
               "* NEXT [#A] Review: %? \nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)
              ("e" "email" entry (file+olp "~/Workspace/docs/org/work.org" "Work" "Email")
               "* MEETING Email\n%U\n" :clock-in t :clock-keep t :immediate-finish t)
              ("b" "bio" entry (file+olp "~/Workspace/docs/org/diary.org" "Bio")
               "* MEETING Bio \n%U\n" :clock-in t :clock-keep t :immediate-finish t)
              ("s" "self" entry (file+olp "~/Workspace/docs/org/diary.org" "Self")
               "* MEETING Self \n%U\n" :clock-in t :clock-keep t :immediate-finish t)
              ("u" "avalon ui" entry (file+olp "~/Workspace/docs/org/work.org" "Avalon" "avui")
               "* NEXT AVUI: %? [%]\nSCHEDULED: %t\n%U\n%a\n** NEXT \n** NEXT \n** NEXT commit message\n** NEXT consider testing\n" :clock-in t :clock-resume t)
              ("h" "achieve" entry (file+olp "~/Workspace/docs/org/work.org" "Avalon" "avach")
               "* NEXT ACHIEVE: %? [%]\nSCHEDULED: %t\n%U\n%a\n** NEXT \n** NEXT \n** NEXT commit message\n** NEXT consider testing\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/Workspace/docs/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Workspace/docs/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/Workspace/docs/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/Workspace/docs/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Workspace/docs/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("x" "Habit" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-a
                            "~/Workspace/docs/org/work.org"
			    (("w" . "Work")
			     ("a" . "Avalon")
			     ("u" . "avui")
			     ("h" . "avach")                             
                             ))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-b
                            "~/Workspace/docs/org/diary.org"
			    (("e" . "MyEvents")))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-c
			    "~/Workspace/docs/org/computing.org"
			    (("e" . "Emacs")
			     ("c" . "Config")))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-d
			    "~/Workspace/docs/org/accounts.org"
			    (("a" . "Accounts")))

(defhydra josh/org-refile-hydra (:foreign-keys run) 
  "Refile"
  ("w" josh/org-refile-hydra-file-a/body "Work" :exit t)
  ("d" josh/org-refile-hydra-file-b/body "Diary" :exit t)
  ("c" josh/org-refile-hydra-file-c/body "Computing" :exit t)
  ("a" josh/org-refile-hydra-file-d/body "Accounts" :exit t)  
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(bind-key "r" 'josh/org-refile-hydra/body 'my-map)
