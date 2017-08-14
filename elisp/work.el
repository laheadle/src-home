(setq org-tag-alist (quote (
                            ("FLAGGED" . ??)
                            ("MATT" . ?m)
                            ("RETRO" . ?r)
                            ("CONFIG" . ?c)
                            ("WORK" . ?w)
                            )))


(setq org-directory "~/Workspace/docs/org")
(setq org-default-notes-file "~/Workspace/docs/org/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/Workspace/docs/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
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
