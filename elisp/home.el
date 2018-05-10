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
;; test
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
