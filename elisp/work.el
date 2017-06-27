(setq org-tag-alist (quote (
                            ("FLAGGED" . ??)
                            ("MATT" . ?m)
                            )))


(setq org-directory "~/Workspace/docs/org")
(setq org-default-notes-file "~/Workspace/docs/org/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Workspace/docs/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
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


;; (progn
;;   (setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.9/emacs"
;;                          load-path))
;;       (setq erlang-root-dir "/usr/local/lib/erlang")
;;       (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
;;       (require 'erlang-start))

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

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))
