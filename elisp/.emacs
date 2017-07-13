(defvar l-elisp-home (file-truename "~/src/home/elisp/"))
(add-to-list 'load-path (concat l-elisp-home "lib"))

(defvar l-env-file (concat l-elisp-home "environment.el"))

(defun read-file (filePath)
  "Return filePath's file content as lisp object"
  (with-temp-buffer
    (insert-file-contents filePath)
    (read (buffer-string))))

(setq l-env (read-file l-env-file))

(load-file (concat l-elisp-home l-env ".el"))

(add-to-list 'load-path "~/extern/org-mode/lisp")
(setq org-mode-user-lisp-path "~/extern/org-mode/lisp")

(setq
 package-enable-at-startup nil
 package-archives
 '(("melpa-stable" . "https://stable.melpa.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("gnu" . "https://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;(load (expand-file-name "~/packages.el"))
(use-package bbdb)

(use-package free-keys)
(use-package bind-key)
(define-prefix-command 'my-map)
(bind-key "C-1" 'my-map)

(setq org-mode-user-lisp-path "~/extern/org-mode/lisp")
(setq org-mode-user-contrib-lisp-path "~/extern/org-mode/contrib/lisp")

(load (concat l-elisp-home "org-mode.el"))

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

(defun l-beginning-of-block ()
  (interactive)
  (org-babel-mark-block)
  (exchange-point-and-mark))

  (setq org-M-RET-may-split-line '((default . nil)))

(defvar l-dot-emacs (concat l-elisp-home "dot-emacs.org"))

(defun my/tangle-on-save-emacs-config-org-file ()
  (when (string= buffer-file-name l-dot-emacs)
    (org-babel-tangle)))

(add-hook 'after-save-hook 'my/tangle-on-save-emacs-config-org-file)

(use-package hydra)

(require 'breadcrumb)

  (defhydra hydra-breadcrumb
    (my-map "b")
    "
Breadcrumb bookmarks:
  _<up>_:   prev   _S-<up>_:   local prev
  _<down>_: next   _S-<down>_: local next
  _s_: set  _c_: clear  _l_: list  _q_: quit
"
    ("<down>" bc-next nil :exit nil)
    ("<up>" bc-previous nil :exit nil)
    ("S-<down>" bc-local-next nil :exit nil)
    ("S-<up>" bc-local-previous nil :exit nil)
    ("l" bc-list nil)
    ("s" bc-set nil)
    ("c" bc-clear nil)
    ("q" nil nil))

(use-package company
  :init
  (global-company-mode))

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)

    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-mode-fuzzy-match t
          helm-completion-in-region-fuzzy-match t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s"
          helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
    (helm-mode)
    (helm-adaptive-mode t)
    (require 'grep)
    (add-to-list 'grep-find-ignored-directories "t2k-applet")
    (add-to-list 'grep-find-ignored-directories "node_modules")
    )
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ;("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-org-rifle
  :bind ("C-c g" . helm-org-rifle-agenda-files))

(use-package helm-ls-git)

(use-package helm-git-grep)

(use-package helm-descbinds
  :init (helm-descbinds-mode))

(use-package counsel
  :bind ("C-b" . ivy-switch-buffer)
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-wrap t)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    ;(global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))
  (use-package swiper)

(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'endless/toggle-map)
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "l" #'toggle-truncate-lines)
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "m" #'transient-mark-mode)
(define-key endless/toggle-map "t" #'transient-mark-mode)


;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

(put 'narrow-to-region 'disabled nil)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(define-key endless/toggle-map "n"
  #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

(use-package origami
  :init
  (progn
    (global-origami-mode)
    (defhydra hydra-folding (my-map "f" :color red)
      "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
      ("o" origami-open-node)
      ("c" origami-close-node)
      ("n" origami-next-fold)
      ("p" origami-previous-fold)
      ("f" origami-forward-toggle-node)
      ("a" origami-toggle-all-nodes))))

(put 'upcase-region 'disabled nil)

(define-key global-map (kbd "M-k")
  (lambda () (interactive)
    (kill-ring-save (point) (line-end-position))))

(global-set-key "\C-p" 'beginning-of-line)

;; Shift the selected region right if distance is positive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one 
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)


(defun backward-some ()
  (interactive)
  (forward-line -10))
(defun forward-some ()
  (interactive)
  (forward-line 10))

(define-key global-map (kbd "M-<up>") 'backward-some)
(define-key global-map (kbd "M-<down>") 'forward-some)


(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
    'interactive)

  (global-set-key "\M-z" 'zap-to-char)

(defadvice kill-region (before slick-cut activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

;; watch the file system
(global-auto-revert-mode nil)

(setq make-backup-files nil)


(setq dired-guess-shell-alist-user
      (list
       (list "\\.zip$" "munzip")))


(prefer-coding-system 'utf-8)

(desktop-save-mode 1)

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

(defun l-reload() (interactive) 
  (let ((m (point)))
    (find-alternate-file (buffer-file-name))
    (goto-char m)))

(defun l-backups ()
  (shell-command "backups.sh&"))

(defvar one-hour (* 60 60))
(run-with-timer one-hour (* 24 one-hour) 'l-backups)

(use-package helm-chrome)

(use-package with-editor)

(fset 'yes-or-no-p 'y-or-n-p)

;; irc
(setq circe-reduce-lurker-spam t)

(server-start)

(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(column-number-mode)

(setq debug-on-error t)

(use-package magit
  :init (setq magit-popup-use-prefix-argument nil
              magit-commit-show-diff nil
              magit-revert-buffers 1
              magit-visit-ref-create t)
  :bind (("C-." . magit-status)))

(use-package emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (setq emmet-move-cursor-between-quotes t))) ;; default nil

(use-package cider
  :init

  (setq cider-prompt-for-symbol nil
        cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

(use-package clojure-cheatsheet)
(use-package helm-cider
  :init (helm-cider-mode 1))

(use-package cider-hydra)

(add-hook 'cider-mode-hook #'cider-hydra-mode)

(add-hook 'js-mode-hook (lambda () (abbrev-mode 1)))

(setq js-indent-level 2)
(setq-default indent-tabs-mode nil)

(use-package js2-mode
  :init (progn
          (setq js2-mode-hook
                '(lambda ()
                   (progn
                     (show-paren-mode 1)
                     (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                     (define-key js2-mode-map "@" 'js-doc-insert-tag)
                     (electric-indent-local-mode -1)
                     (set-variable 'indent-tabs-mode nil))))

          (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))))

(use-package nvm)

(defun do-nvm-use (version)
  (interactive "sVersion: ")
  (nvm-use version))

(defun l-show-nvm-installed-versions ()
  (interactive)
  (message "%s" (nvm--installed-versions)))

(defun run-node (cwd)
  (interactive "DDirectory: ")
  (unless (executable-find "node")
    (call-interactively 'do-nvm-use))
  (let ((default-directory cwd))
        (pop-to-buffer (make-comint (format "node-repl-%s" cwd) "node" nil "--interactive"))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq typescript-indent-level 2)

  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :init (progn
          ;; aligns annotation to the right hand side
          (setq company-tooltip-align-annotations t)
          (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

          (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

          (add-hook 'typescript-mode-hook #'setup-tide-mode)))

(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (add-hook 'html-mode-hook #'smartparens-mode)
    (add-hook 'js-mode-hook #'smartparens-mode)
    (add-hook 'typescript-mode-hook #'smartparens-mode)))


(defhydra hydra-learn-sp (my-map "p" :hint nil)
  "
  _B_ backward-sexp            -----
  _F_ forward-sexp               _s_ splice-sexp
  _L_ backward-down-sexp         _df_ splice-sexp-killing-forward
  _H_ backward-up-sexp           _db_ splice-sexp-killing-backward
^^------                         _da_ splice-sexp-killing-around
  _k_ down-sexp                -----
  _j_ up-sexp                    _C-s_ select-next-thing-exchange
-^^-----                         _C-p_ select-previous-thing
  _n_ next-sexp                  _C-n_ select-next-thing
  _p_ previous-sexp            -----
  _a_ beginning-of-sexp          _C-f_ forward-symbol
  _z_ end-of-sexp                _C-b_ backward-symbol
--^^-                          -----
  _t_ transpose-sexp             _c_ convolute-sexp
-^^--                            _g_ absorb-sexp
  _x_ delete-char                _q_ emit-sexp
  _dw_ kill-word               -----
  _dd_ kill-sexp                 _,b_ extract-before-sexp
-^^--                            _,a_ extract-after-sexp
  _S_ unwrap-sexp              -----
-^^--                            _AP_ add-to-previous-sexp
  _C-h_ forward-slurp-sexp       _AN_ add-to-next-sexp
  _C-l_ forward-barf-sexp      -----
  _C-S-h_ backward-slurp-sexp    _ join-sexp
  _C-S-l_ backward-barf-sexp     _|_ split-sexp
"
  ;; TODO: Use () and [] - + * | <space>
  ("B" sp-backward-sexp );; similiar to VIM b
  ("F" sp-forward-sexp );; similar to VIM f
  ;;
  ("L" sp-backward-down-sexp )
  ("H" sp-backward-up-sexp )
  ;;
  ("k" sp-down-sexp ) ; root - towards the root
  ("j" sp-up-sexp )
  ;;
  ("n" sp-next-sexp )
  ("p" sp-previous-sexp )
  ;; a..z
  ("a" sp-beginning-of-sexp )
  ("z" sp-end-of-sexp )
  ;;
  ("t" sp-transpose-sexp )
  ;;
  ("x" sp-delete-char )
  ("dw" sp-kill-word )
  ;;("ds" sp-kill-symbol ) ;; Prefer kill-sexp
  ("dd" sp-kill-sexp )
  ;;("yy" sp-copy-sexp ) ;; Don't like it. Pref visual selection
  ;;
  ("S" sp-unwrap-sexp ) ;; Strip!
  ;;("wh" sp-backward-unwrap-sexp ) ;; Too similar to above
  ;;
  ("C-h" sp-forward-slurp-sexp )
  ("C-l" sp-forward-barf-sexp )
  ("C-S-h" sp-backward-slurp-sexp )
  ("C-S-l" sp-backward-barf-sexp )
  ;;
  ;;("C-[" (bind (sp-wrap-with-pair "[")) ) ;;FIXME
  ;;("C-(" (bind (sp-wrap-with-pair "(")) )
  ;;
  ("s" sp-splice-sexp )
  ("df" sp-splice-sexp-killing-forward )
  ("db" sp-splice-sexp-killing-backward )
  ("da" sp-splice-sexp-killing-around )
  ;;
  ("C-s" sp-select-next-thing-exchange )
  ("C-p" sp-select-previous-thing )
  ("C-n" sp-select-next-thing )
  ;;
  ("C-f" sp-forward-symbol )
  ("C-b" sp-backward-symbol )
  ;;
  ;;("C-t" sp-prefix-tag-object)
  ;;("H-p" sp-prefix-pair-object)
  ("c" sp-convolute-sexp )
  ("g" sp-absorb-sexp )
  ("q" sp-emit-sexp )
  ;;
  (",b" sp-extract-before-sexp )
  (",a" sp-extract-after-sexp )
  ;;
  ("AP" sp-add-to-previous-sexp );; Difference to slurp?
  ("AN" sp-add-to-next-sexp )
  ;;
  ("_" sp-join-sexp ) ;;Good
  ("|" sp-split-sexp ))

(show-paren-mode)

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
