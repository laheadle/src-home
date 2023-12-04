(defvar l-org-context :family)

(defmacro josh/make-org-refile-family-hydra (hydraname file keyandheadline)
  "Make a hydra named HYDRANAME with refile targets to FILE.
KEYANDHEADLINE should be a list of cons cells of the form (\"key\" . \"headline\")"
  `(defhydra ,hydraname (:color blue :after-exit (unless (or hydra-deactivate
							     current-prefix-arg) ;If we're just jumping to a location, quit the hydra
						   (josh/org-refile-family-hydra/body)))
     ,file
     ,@(cl-loop for kv in keyandheadline
		collect (list (car kv) (list 'josh/refile file (cdr kv) 'current-prefix-arg) (cdr kv)))
     ("q" nil "cancel")))

(josh/make-org-refile-family-hydra josh/org-refile-hydra-file-a
                            "~/doc/org/1/family.org"
			    (
                             ("i" . "Misc-Family")
                             ))

(josh/make-org-refile-family-hydra josh/org-refile-hydra-file-b
                            "~/doc/org/diary.org"
			    (("e" . "MyEvents")))

(josh/make-org-refile-family-hydra josh/org-refile-hydra-file-c
                            "~/doc/org/1/non-agenda/cooler.org"
                            (("c" . "Cooler")
                             ))

(josh/make-org-refile-family-hydra josh/org-refile-hydra-file-d
                            "~/doc/org/1/tickler.org"
                            (("t" . "Tickler")
                             ))

(josh/make-org-refile-family-hydra josh/org-refile-hydra-file-e
                            "~/doc/org/1/non-agenda/family-notes.org"
                            (("n" . "Notes")
                             ))

(defhydra josh/org-refile-family-hydra (:foreign-keys run) 
  "Refile"
  ("f" josh/org-refile-hydra-file-a/body "Family" :exit t)
  ("c" josh/org-refile-hydra-file-c/body "Cooler" :exit t)
  ("t" josh/org-refile-hydra-file-d/body "Tickler" :exit t)
  ("n" josh/org-refile-hydra-file-e/body "Notes" :exit t)
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(defun set-family-refiles ()
  ;; (bind-key "r" 'josh/org-refile-family-hydra/body 'my-map)
  )

(defmacro josh/make-org-refile-mywork-hydra (hydraname file keyandheadline)
  "Make a hydra named HYDRANAME with refile targets to FILE.
KEYANDHEADLINE should be a list of cons cells of the form (\"key\" . \"headline\")"
  `(defhydra ,hydraname (:color blue :after-exit (unless (or hydra-deactivate
							     current-prefix-arg) ;If we're just jumping to a location, quit the hydra
						   (josh/org-refile-mywork-hydra/body)))
     ,file
     ,@(cl-loop for kv in keyandheadline
		collect (list (car kv) (list 'josh/refile file (cdr kv) 'current-prefix-arg) (cdr kv)))
     ("q" nil "cancel")))

(josh/make-org-refile-mywork-hydra josh/org-refile-hydra-file-aa
                            "~/doc/org/2/mywork.org"
			    (
                             ("d" . "Darwin")
                             ("i" . "Misc-Mywork")
                             ))

(josh/make-org-refile-mywork-hydra josh/org-refile-hydra-file-cc
                            "~/doc/org/2/non-agenda/cooler.org"
                            (("c" . "Cooler")
                             ))

(defhydra josh/org-refile-mywork-hydra (:foreign-keys run) 
  "Refile"
  ("f" josh/org-refile-hydra-file-aa/body "Mywork" :exit t)
  ("c" josh/org-refile-hydra-file-cc/body "Cooler" :exit t)
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(defun set-mywork-refiles ()
  ;; (bind-key "r" 'josh/org-refile-mywork-hydra/body 'my-map)
  )

(defun my-find-all-links ()
  ""
  (interactive)
  (require 'org-archive)
  (let ((organ-files (rx (seq ".org" eol))))
    (setq org-id-extra-files (append (directory-files-recursively "/home/laheadle/doc/org" organ-files)
                                     (directory-files-recursively  "/home/laheadle/src/home" organ-files)))
(org-id-update-id-locations)))

(defun set-family-misc ()
  (setq org-directory "~/doc/org")

  (setq org-default-notes-file "~/doc/org/1/refile.org")

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
        (quote (("t" "todo" entry (file+olp "~/doc/org/1/family.org" "Misc-Family")
                 "* NEXT [#B] %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("v" "todo. add deadline." entry (file+olp "~/doc/org/1/family.org" "Misc-Family")
                 "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
                 ("p" "Protocol" entry (file "~/doc/org/1/non-agenda/web-notes.org")
                  "* %u %:annotation\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                ("m" "Meeting" entry (file "~/doc/org/1/refile.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-keep t))))

  (setq org-global-properties (quote (("Effort_ALL" . "0:07 0:15 0:30 1:00 2:00 4:00 6:00 10:00 16:00 0:00")
                                      ("STYLE_ALL" . "habit"))))


  (setq org-tag-alist (quote (("FLAGGED" . ??)
                              ("GROOM" . ?g)
                              ("ADRI" . ?a)
                              ("CHORE" . ?c)
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
                ("p" "Protocol" entry (file "~/doc/org/2/non-agenda/web-notes.org")
                 "* %u %:annotation\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                ("f" "reflection" entry (file "~/doc/org/2/refile.org")
                 "* MEETING Reflection: %?\n%U\n" :clock-in t :clock-keep t)
                ("n" "note" entry (file "~/doc/org/2/refile.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("m" "Meeting" entry (file "~/doc/org/2/refile.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-keep t)
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
  (interactive)
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

(set-mywork-context)
(set-family-context)
;; (bind-key "o" #'l-switch-org-context my-map)
