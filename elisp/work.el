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
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "review" entry (file+olp "~/Workspace/docs/org/work.org" "Review")
               "* NEXT [#A] Review: %? \nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)
              ("u" "avalon ui" entry (file+olp "~/Workspace/docs/org/work.org" "Avalon" "avui")
               "* NEXT AVUI: %? [%]\nSCHEDULED: %t\n%U\n%a\n** NEXT \n** NEXT \n** NEXT commit message\n** NEXT consider testing\n" :clock-in t :clock-resume t)
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
              ("h" "Habit" entry (file "~/Workspace/docs/org/refile.org")
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

(defun l-getenv (var)
  (shell-command-to-string (concat ". ~/.bashrc; echo -n $" var)))

(setq org-user-agenda-files (quote ("~/Workspace/docs/org")))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; lines at a time
    
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(set-register ?w '(file . "~/Workspace/Sketchpad/Web"))
(set-register ?t `(file . ,(l-getenv "tk")))
(set-register ?g '(file . "/Users/lyn_headley/Workspace/glossary/g"))
(set-register ?a '(file . "/Users/lyn_headley/Workspace/asset-registry/a"))

(defun l-show-make-var ()
  (interactive)
    (cl-flet ((my-word-at-point ()
                             (buffer-substring
                              (+ 1 (save-excursion (search-backward "(" nil t)))
                              (- (save-excursion (search-forward ")" nil t)) 1))))

  (let ((cmd-str  (concat "make print-"
                          (my-word-at-point))))
                                   ;(buffer-substring (region-beginning) (region-end)))))
    (let ((default-directory "/Users/lyn_headley/Workspace/WebGSP/"))
      (message "%s" (shell-command-to-string cmd-str))))))

;;

;; fix stupid mousedown sets mark
;; http://stackoverflow.com/questions/13986605/how-to-make-emacs-mouse-drag-not-highlight-or-set-mark

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(set-register ?d '(file . "/lheadley@dn.kcptech.com:/home/lheadley"))

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.mheducation.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.mheducation\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))
