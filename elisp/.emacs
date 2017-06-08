(defvar l-elisp-home (file-truename "~/src/home/elisp/"))

(defvar l-env-file (concat l-elisp-home "environment.el"))

(defun read-file (filePath)
  "Return filePath's file content as lisp object"
  (with-temp-buffer
    (insert-file-contents filePath)
    (read (buffer-string))))

(setq l-env (read-file l-env-file))

(load-file (concat l-elisp-home l-env ".el"))

(setq
 package-enable-at-startup nil
 package-archives
 '(("melpa-stable" . "https://stable.melpa.org/packages/")
   ("melpa" . "https://melpa.org/packages/")))

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
(bind-key "M-c" 'my-map)

(add-to-list 'load-path "~/extern/org-mode/lisp")

(setq org-mode-user-lisp-path "~/extern/org-mode/lisp")
(setq org-mode-user-contrib-lisp-path "~/extern/org-mode/contrib/lisp")

(load (concat l-elisp-home "org-mode.el"))

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

(defvar l-dot-emacs (concat l-elisp-home "dot-emacs.org"))

(defun my/tangle-on-save-emacs-config-org-file ()
  (when (string= buffer-file-name l-dot-emacs)
    (org-babel-tangle)))

(add-hook 'after-save-hook 'my/tangle-on-save-emacs-config-org-file)

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
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-org-rifle)

(use-package helm-ls-git)

(use-package helm-git-grep)

(use-package helm-descbinds
  :init (helm-descbinds-mode))

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

(put 'upcase-region 'disabled nil)

(define-key global-map (kbd "M-k")
  (lambda () (interactive)
    (kill-ring-save (point) (line-end-position))))

(global-set-key "\C-p" 'beginning-of-line)
(global-set-key "\C-cg" 'goto-line)


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

  (global-set-key "\M-z" 'zap-up-to-char)

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
(define-key global-map (kbd "C-b") 'l-reload)


(defun l-backups ()
  (shell-command "backups.sh&"))

(defvar one-hour (* 60 60))
(run-with-timer one-hour (* 24 one-hour) 'l-backups)

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
              magit-visit-ref-create t)
  :bind (("C-." . magit-status)))

(use-package emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (setq emmet-move-cursor-between-quotes t))) ;; default nil

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(package-selected-packages
   (quote
    (helm-descbinds helm-git-grep helm-ls-git emmet-mode helm-org-rifle helm yaml-mode with-editor use-package smex php-mode markdown-mode js2-mode free-keys circe bbdb))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-context ((t (:inherit ##))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))))
 '(secondary-selection ((t (:background "gray96")))))
