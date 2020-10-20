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
                      ("INTELLECTION" . ?i)
                      ("CONTEXT" . ?c)
                      ("STRATEGIC" . ?s)
                      ("LEARNER" . ?l)
                      ("INPUT" . ?p)
                      (:endgrouptag)
                      (:startgrouptag)
                      ("PERSONAL")
                      (:grouptags)
                      ("EDUCATION")
                      ("TESTING")
                      ("ENGINEERING")
                      ("COLLABORATION")
                      ("MONITORING")
                      (:endgrouptag)
                      (:startgrouptag)
                      ("CORPORATE")
                      (:grouptags)
                      ("WORKING_OUTSIDE_IN")
                      ("DRIVING_WITH_DATA")
                      ("EXECUTING_WITH_AGILITY")
                      ("PARTNERING_ACROSS_TEAMS")
                      (:endgrouptag)))


(setq org-directory (expand-file-name "~/Workspace/docs/org"))
(setq org-default-notes-file "~/Workspace/docs/org/refile.org")

;; Capture templates
;; U - inactive timestamp
;; a - link
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "review" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT [#A] Review: %? :REVIEW: \nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)
              ("b" "bio" entry (file+olp "~/Workspace/docs/org/work.org" "Bio")
               "* MEETING Bio \n%U\n" :clock-in t :clock-keep t :immediate-finish t)
              ("e" "email" entry (file+olp "~/Workspace/docs/org/work.org" "Misc-Work" "Email")
               "* MEETING Email\n%U\n" :clock-in t :clock-keep t :immediate-finish t)
              ("f" "reflection" entry (file "~/Workspace/docs/org/refile.org")
               "* MEETING Reflection: %?\n%U\n" :clock-in t :clock-keep t)
              ("s" "self" entry (file+olp "~/Workspace/docs/org/work.org" "Self")
               "* MEETING Self \n%U\n" :clock-in t :clock-keep t :immediate-finish t)
              ("u" "avalon ui" entry (file+olp "~/Workspace/docs/org/work.org" "Avalon" "avui")
               "* NEXT  %? [%]\nSCHEDULED: %t\n%U\n%a\n** \n** commit message\n** consider testing\n" :clock-in t :clock-resume t)
              ("h" "achieve" entry (file+olp "~/Workspace/docs/org/work.org" "Avalon" "avach")
               "* NEXT ACHIEVE: %? [%]\nSCHEDULED: %t\n%U\n%a\n** NEXT \n** NEXT \n** NEXT consider testing\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/Workspace/docs/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Workspace/docs/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-keep t)
              ("v" "Event" entry (file+olp "~/Workspace/docs/org/work.org" "Events")
               "*  %? :EVENT:\n%U\n")
              ("x" "Habit" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(defmacro josh/make-org-refile-hydra (hydraname file keyandheadline)
  "Make a hydra named HYDRANAME with refile targets to FILE.
KEYANDHEADLINE should be a list of cons cells of the form (\"key\" . \"headline\")"
  `(defhydra ,hydraname (:color blue :after-exit (unless (or hydra-deactivate
							     current-prefix-arg) ;If we're just jumping to a location, quit the hydra
						   (josh/org-refile-hydra/body)))
     ,file
     ,@(cl-loop for kv in keyandheadline
		collect (list (car kv) (list 'josh/refile file (cdr kv) 'current-prefix-arg) (cdr kv)))
     ("q" nil "cancel")))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-a
                            "~/Workspace/docs/org/work.org"
			    (("a" . "Avalon")
			     ("u" . "avui")
                             ("g" . "Glossary")
                             ("h" . "avach")
			     ("c" . "Career")
                             ("m" . "Meetings")
                             ("i" . "Misc-Work")
                             ("e" . "Events")
                             ("s" . "Sketchpad")
                             ("w" . "Work")
                             ("y" . "Sasysaw-MHE")
                             ))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-widgets
                            "~/Workspace/docs/org/work.org"
			    (("y" . "Table-Type-Entry")
                             ("i" . "Widgets")
                             ("c" . "Choose-Your-Path")
                             ))

(josh/make-org-refile-hydra josh/org-refile-hydra-file-b
                            "~/Workspace/docs/org/diary.org"
			    (("e" . "MyEvents")))

(defhydra josh/org-refile-hydra (:foreign-keys run) 
  "Refile"
  ("w" josh/org-refile-hydra-file-a/body "Work" :exit t)
  ("i" josh/org-refile-hydra-file-widgets/body "Widgets" :exit t)
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(bind-key "r" 'josh/org-refile-hydra/body 'my-map)

(defun l-work-goto-next-project-to-clean ()
  (interactive)
  (let* ((headline (om-parse-this-headline))
         ;; (match (om-match '(headline section plain-list) headline))
         (match (om-match '(headline section) headline)))
    match))

(defun l-work-goto-next-project-to-clean-headline ()
  (interactive)
  (let* ((headline (om-parse-this-headline)))
    headline))

;; todo: keep querying. toplevel structure is (headline (section . . (plain-list ))

(eval-when-compile (require 'cl-lib))

(defun l-om-link-text (link)
  (cl-assert (listp link))
  (car (om-get-children link)))

(defun l-get-jira-title ()
  (-as-> (om-parse-this-headline) $
                       (om-get-property :title $)
                       (list (l-om-link-text (car $))
                             (cadr $))))

(defun l-insert-jira-title ()
  (interactive)
  (pcase-let ((`(,jira-item ,title) (l-get-jira-title)))
    (insert (concat jira-item " " title "\nnotes"))))
