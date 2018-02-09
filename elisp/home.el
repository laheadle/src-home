(josh/make-org-refile-hydra josh/org-refile-hydra-file-a
                            "~/doc/org/1/family.org"
			    (("f" . "Family")
                             ("p" . "Personal")
                             ("m" . "Meta")
                             ("a" . "Adri")
                             ("t" . "Travel")
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

(setq org-user-agenda-files (quote ("~/doc/org/1" "~/doc/org"
                                    "~/doc/org/2")))
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

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(progn
  (setq load-path (cons  "/usr/lib/erlang/lib/tools-2.9.1/emacs"
                         load-path))
      (setq erlang-root-dir "/usr/lib/erlang")
      (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
      (require 'erlang-start))

;; (add-to-list 'load-path "/Users/lyn_headley/.emacs.d/site-lisp/distel/elisp")
;; (require 'distel)
;; (distel-setup)
;; (defconst distel-shell-keys
;;   '(("\C-\M-i"   erl-complete)
;;     ("\M-?"      erl-complete)
;;     ("\M-."      erl-find-source-under-point)
;;     ("\M-,"      erl-find-source-unwind)
;;     ("\M-*"      erl-find-source-unwind)
;;     )
;;   "Additional keys to bind when in Erlang shell.")

;; (add-hook 'erlang-shell-mode-hook
;;           (lambda ()
;;             ;; add some Distel bindings to the Erlang shell
;;             (dolist (spec distel-shell-keys)
;;               (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; (defvar l-ebin-epgsql "/Users/lyn_headley/Workspace/trails/erlang/sg/deps/epgsql/ebin")
;; (defvar l-ebin-sg "/Users/lyn_headley/Workspace/trails/erlang/sg/ebin")
;; (defconst l-include-sg "/Users/lyn_headley/Workspace/trails/erlang/sg/include")
;; (defvar l-ebin-yaws "/Users/lyn_headley/Workspace/trails/erlang/sg/deps/yaws/ebin")
;; (defvar l-include-yaws "/Users/lyn_headley/Workspace/trails/erlang/sg/deps/yaws/include")
;; (defvar l-ebin-poolboy "/Users/lyn_headley/Workspace/trails/erlang/sg/deps/poolboy/ebin")

;; (setq erlang-compile-extra-opts (list `(i . ,l-include-yaws)
;;                                       `(i . ,l-include-sg)
;;                                       ))

;; (setq inferior-erlang-machine-options
;;      (list "-name" "dev@localhost"
;;             "-pa" l-ebin-sg
;;            "-pa" l-ebin-yaws
;;            "-pa" l-ebin-epgsql
;;            "-pa" l-ebin-poolboy))
