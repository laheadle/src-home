(defvar l-org-context :family)

(josh/make-org-refile-hydra josh/org-refile-hydra-file-a
                            "~/doc/org/1/family.org"
			    (("f" . "Family")
                             ("p" . "Personal")
                             ("m" . "Meta")
                             ("a" . "Adri")
                             ("b" . "Bookmarks")
                             ("t" . "Travel")
                             ("h" . "House")
                             ("c" . "Children")
                             ("r" . "Friends")
                             ))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-b
                            "~/doc/org/diary.org"
			    (("e" . "MyEvents")))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-c
                            "~/doc/org/non-agenda/1/cooler.org"
                            (("c" . "Cooler")
                             ))

(defhydra josh/org-refile-family-hydra (:foreign-keys run) 
  "Refile"
  ("f" josh/org-refile-hydra-file-a/body "Family" :exit t)
  ("c" josh/org-refile-hydra-file-c/body "Cooler" :exit t)
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(defun set-family-refiles ()
  (bind-key "r" 'josh/org-refile-family-hydra/body 'my-map))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-aa
                            "~/doc/org/2/mywork.org"
			    (
                             ("d" . "Darwin")
                             ))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-cc
                            "~/doc/org/non-agenda/2/cooler.org"
                            (("c" . "Cooler")
                             ))

(defhydra josh/org-refile-mywork-hydra (:foreign-keys run) 
  "Refile"
  ("f" josh/org-refile-hydra-file-aa/body "Mywork" :exit t)
  ("c" josh/org-refile-hydra-file-cc/body "Cooler" :exit t)
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(defun set-mywork-refiles ()
  (bind-key "r" 'josh/org-refile-mywork-hydra/body 'my-map))

(defun set-family-misc ()
  (setq org-directory "~/doc/org")

  (setq org-default-notes-file "~/doc/org/1/refile.org")

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/doc/org/1/refile.org")
                 "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("v" "Event" entry (file+olp "~/doc/org/1/family.org" "Events")
                 "*  %? :EVENT:\n%U\n")
                ("f" "reflection" entry (file+olp "~/doc/org/1/family.org" "Misc-Family" "Reflection")
                 "* MEETING Reflection\n%U\n" :clock-in t :clock-keep t :immediate-finish t)
                ("n" "note" entry (file "~/doc/org/1/refile.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("m" "Meeting" entry (file "~/doc/org/1/refile.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/doc/org/1/refile.org")
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
                              ))))

(defun set-mywork-misc ()
  (setq org-directory "~/doc/org")

  (setq org-default-notes-file "~/doc/org/2/refile.org")

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/doc/org/2/refile.org")
                 "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("v" "Event" entry (file+olp "~/doc/org/2/mywork.org" "Events")
                 "*  %? :EVENT:\n%U\n")
                ("f" "reflection" entry (file+olp "~/doc/org/2/mywork.org" "Misc-Mywork" "Reflection")
                 "* MEETING Reflection\n%U\n" :clock-in t :clock-keep t :immediate-finish t)
                ("n" "note" entry (file "~/doc/org/2/refile.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("m" "Meeting" entry (file "~/doc/org/2/refile.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/doc/org/2/refile.org")
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  (setq org-global-properties (quote (("Effort_ALL" . "0:07 0:15 0:30 1:00 2:00 4:00 6:00 10:00 16:00 0:00")
                                      ("STYLE_ALL" . "habit"))))


  (setq org-tag-alist (quote (
                              ("READING" . ?d)
                              ))))

(defun set-family-context ()
  (set-family-refiles)
  (set-family-agenda-files)
  (set-family-misc))

(defun set-mywork-context ()
  (set-mywork-refiles)
  (set-mywork-agenda-files)
  (set-mywork-misc))

(defun l-switch-org-context ()
  (interactive)
  (setq l-org-context (if (eq l-org-context :family) :mywork :family))
  (if (eq l-org-context :family)
      (set-family-context)
    (set-mywork-context))
  (message "%s" l-org-context))

(set-family-context)
(bind-key "o" #'l-switch-org-context my-map)
