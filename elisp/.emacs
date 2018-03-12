;;;
;;; Org Mode
;;;
(add-to-list 'load-path (expand-file-name "~/extern/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

(setq
 package-enable-at-startup nil
 package-archives
 '(("melpa-stable" . "https://stable.melpa.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("org" . "http://orgmode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

;(setq debug-on-quit t)

(defvar l-src-home (file-truename "~/src/home/"))
(defvar l-elisp-home (file-truename "~/src/home/elisp/"))
(add-to-list 'load-path (concat l-elisp-home "lib"))

;; Adapted from https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location
(defun my/refile (file headline &optional arg)
  "Refile to a specific location. 

With a 'C-u' ARG argument, we jump to that location (see
`org-refile').

Use `org-agenda-refile' in `org-agenda' mode."
  (let* ((pos (with-current-buffer (or (get-buffer file)	;Is the file open in a buffer already?
				       (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
		(or (org-find-exact-headline-in-buffer headline)
		    (error "Can't find headline `%s'" headline))))
	 (filepath (buffer-file-name (marker-buffer pos)));If we're given a relative name, find absolute path
	 (rfloc (list headline filepath nil pos)))
    (if (and (eq major-mode 'org-agenda-mode) (not (and arg (listp arg)))) ;Don't use org-agenda-refile if we're just jumping
	(org-agenda-refile nil rfloc)
      (org-refile arg nil rfloc))))

(defun josh/refile (file headline &optional arg)
  "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.

With a `C-u` ARG, just jump to the headline."
  (interactive "P")
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
    (cond
     ((and arg (listp arg))	    ;Are we jumping?
      (my/refile file headline arg))
     ;; Are we in org-capture-mode?
     (is-capturing      	;Minor mode variable that's defined when capturing
      (josh/org-capture-refile-but-with-args file headline arg)) 
     (t
      (my/refile file headline arg)))
    (when (or arg is-capturing)
      (setq hydra-deactivate t))))

(defun josh/org-capture-refile-but-with-args (file headline &optional arg)
  "Copied from `org-capture-refile' since it doesn't allow passing arguments. This does."
  (unless (eq (org-capture-get :type 'local) 'entry)
    (error
     "Refiling from a capture buffer makes only sense for `entry'-type templates"))
  (let ((pos (point))
	(base (buffer-base-buffer (current-buffer)))
	(org-capture-is-refiling t)
	(kill-buffer (org-capture-get :kill-buffer 'local)))
    (org-capture-put :kill-buffer nil)
    (org-capture-finalize)
    (save-window-excursion
      (with-current-buffer (or base (current-buffer))
	(org-with-wide-buffer
	 (goto-char pos)
	 (my/refile file headline arg))))
    (when kill-buffer (kill-buffer base))))

(use-package free-keys :defer t)
(use-package bind-key  :defer t)
(define-prefix-command 'my-map)
(bind-key "C-1" 'my-map)

(defvar l-env-file (concat l-elisp-home "environment.el"))

(defun read-file (filePath)
  "Return filePath's file content as lisp object"
  (with-temp-buffer
    (insert-file-contents filePath)
    (read (buffer-string))))

(setq l-env (read-file l-env-file))

(defun load-env ()
  (load-file (concat l-elisp-home l-env ".el")))

(defun load-from-env (code-name)
  (load-file (concat l-elisp-home code-name "-" l-env ".el")))

(load-env)

(use-package swiper)
(use-package counsel
  :bind (("C-b" . ivy-switch-buffer)
         ("C-s" . swiper)
         :map my-map
         ("q" . counsel-rg))
  :config (ivy-mode 1)
  :init
  (progn
    (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
    (setq ivy-wrap t)
    (setq ivy-use-virtual-buffers t
          ivy-height 25)
    (setq enable-recursive-minibuffers t)

    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-r") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
                                        ;    (global-set-key (kbd "C-x l") 'counsel-locate)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package hydra)

(load-from-env "org-user-agenda-files")

(load (concat l-elisp-home "org-mode.el"))

(load-from-env "org")

(defun l-org-jump-to-dir ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (if-let ((headers (org-entry-get (point) "header-args" t)))
          (let* ((props (org-babel-parse-header-arguments headers))
                (dir (cdr (assoc :dir props))))
            (if dir (progn
                      (find-file dir)
                      (magit-status)
                      (delete-other-windows))
              (magit-status)))
        (magit-status))
    (magit-status)))

(bind-key "C-." 'l-org-jump-to-dir org-mode-map)

(fset 'l-org-goto-clean-agenda
      "\C-\\ g.ro")

(bind-key "w" 'l-org-goto-clean-agenda my-map)

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

(defun l-beginning-of-block ()
  (interactive)
  (org-babel-mark-block)
  (exchange-point-and-mark))

  (setq org-M-RET-may-split-line '((default . nil)))

(fset 'l-org-new-line-at-end
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 14 left return] 0 "%d")) arg)))

(bind-key "e" 'l-org-new-line-at-end my-map)

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )

(run-with-idle-timer 300 t 'jump-to-org-agenda)

(use-package org-super-agenda
  :config
  (progn
    (org-super-agenda-mode)
    (setq org-super-agenda-groups
          '((:todo "HOLD" :todo "WAITING" :order 300)
            (:name "STATUS" :and (:habit t :regexp "status"))
            (:name "HABITS" :habit t)
            (:name "A" :priority "A")
            (:name "B" :priority "B")
            (:name "DEADLINES" :deadline t :order 250)
            (:name "C" :priority "C" :order 200)))))

(defvar l-dot-emacs (concat l-elisp-home "dot-emacs.org"))

(defun my/tangle-on-save-emacs-config-org-file ()
  (when (string= buffer-file-name l-dot-emacs)
    (org-babel-tangle)))

(add-hook 'after-save-hook 'my/tangle-on-save-emacs-config-org-file)
