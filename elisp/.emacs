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

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))
(use-package god-mode
  :config (progn
            (global-set-key (kbd "<escape>") 'god-mode-all)
            (setq god-exempt-major-modes
                  '(org-agenda-mode magit-mode dired-mode grep-mode vc-annotate-mode git-commit-mode magit-status-mode magit-popup-mode))
                  (define-key god-local-mode-map (kbd "z") 'repeat)
                  (define-key god-local-mode-map (kbd "i") 'god-local-mode)
                  (global-set-key (kbd "C-x C-1") 'delete-other-windows)
                  (global-set-key (kbd "C-x C-2") 'split-window-below)
                  (global-set-key (kbd "C-x C-3") 'split-window-right)
                  (global-set-key (kbd "C-x C-0") 'delete-window)
                  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
                  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
                  ;;            (setq god-exempt-predicates nil)
                  ))

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
          ivy-height 20)
    (setq enable-recursive-minibuffers t)
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s .")
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

(setq org-habit-preceding-days 27
      org-habit-following-days 1)

(require 'ox-md)

(use-package company
  :init
  (global-company-mode))

(use-package helm-org-rifle
  :bind ("C-c g" . helm-org-rifle-agenda-files))

(use-package helm-ls-git)

;(use-package helm-git-grep)

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
(define-key endless/toggle-map "s" #'org-super-agenda-mode)
(define-key endless/toggle-map "t" #'org-indent-mode)
(define-key endless/toggle-map "i" 'ido-mode)
(define-key endless/toggle-map "v" 'ivy-mode)

;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

(define-key endless/toggle-map "h" (lambda () (interactive) (setq org-use-tag-inheritance (not org-use-tag-inheritance))))

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

(defun l-set-tab-width ()
  (setq tab-width (if (eq major-mode 'emacs-lisp-mode) 8 4)))
(add-hook 'emacs-lisp-mode-hook 'l-set-tab-width)
(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (l-set-tab-width)))

(use-package wgrep
:config (setq wgrep-auto-save-buffer t))

(put 'upcase-region 'disabled nil)

(delete-selection-mode 1)

(use-package expand-region
  :bind ("C-@" . er/expand-region))

(define-key global-map (kbd "M-k")
  (lambda () (interactive)
    (kill-ring-save (point) (line-end-position))))


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

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo)))

(setq make-backup-files nil)

(defun l-backups ()
  (shell-command "backups.sh&"))

(defvar one-hour (* 60 60))
(run-with-timer one-hour (* 24 one-hour) 'l-backups)

(ffap-bindings)
(setq ffap-require-prefix t)

;; watch the file system
(global-auto-revert-mode nil)

(setq dired-guess-shell-alist-user
      (list
       (list "\\.zip$" "munzip")))


(prefer-coding-system 'utf-8-unix)

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

(defun l-reload() (interactive) 
  (let ((m (point)))
    (find-alternate-file (buffer-file-name))
    (goto-char m)))

(tool-bar-mode -1)
(menu-bar-mode -1)

(use-package ace-window
  :bind ("M-p" . ace-window)
  :config
  (progn
    (setq aw-dispatch-always t)

    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame")
      ("n" ns-next-frame "next frame"))
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t)
    (ace-window-display-mode t)))



(defun l-kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(bind-key "C-x k" 'l-kill-this-buffer global-map)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(bind-key "C-!" 'eshell-here)

(defun eshell/x ()
  (delete-window)
  (eshell/exit))

(global-eldoc-mode -1)
 
(use-package with-editor)

(fset 'yes-or-no-p 'y-or-n-p)

;; irc
(setq circe-reduce-lurker-spam t)

(server-start)

(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(column-number-mode)

(setq debug-on-error t)

(use-package ob-http)

(use-package csv-mode)

(defun start-org-drill ()
  (interactive)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-maximum-duration 4)
  (require 'org-drill)
  (org-drill))

(use-package xah-lookup)
(defun my-lookup-rae (&optional *word)
  "lookup rae doc of word under cursor"
  (interactive)
  (require 'xah-lookup)
  (xah-lookup-word-on-internet
   *word
   (get 'my-lookup-rae 'xah-lookup-url )
   (get 'my-lookup-rae 'xah-lookup-browser-function )))

(put 'my-lookup-rae 'xah-lookup-url "http://dle.rae.es/?w=word02051")
(put 'my-lookup-rae 'xah-lookup-browser-function 'browse-url)

;(use-package dictionary)

(use-package gnuplot)

(use-package magit
  :init (setq magit-popup-use-prefix-argument nil
              magit-commit-show-diff nil
              magit-completing-read-function 'ivy-completing-read
              magit-revert-buffers 1
              magit-diff-refine-hunk 'all
              magit-visit-ref-create t)
  :config (progn
            (magit-define-popup-action 'magit-merge-popup ?u
                                       "Catch Up To Upstream"
                                       (lambda ()
                                         (interactive)
                                         (magit-merge "@{upstream}"))))
  :bind (("C-." . magit-status)
         :map magit-mode-map
         ("C-c v" . endless/visit-pull-request-url)))



(use-package git-gutter
  :config (global-git-gutter-mode +1))

(use-package emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
    (setq emmet-move-cursor-between-quotes t))) ;; default nil


;;; hl-tags-mode --- Highlight the current SGML tag context

;; Copyright (c) 2011 Mike Spindel <deactivated@gmail.com>
;; Modified by Amit J Patel <amitp@cs.stanford.edu> for nxml-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; hl-tags-mode is a minor mode for SGML and XML editing that
;; highlights the current start and end tag.
;;
;; To use hl-tags-mode, add the following to your .emacs:
;;
;;   (require 'hl-tags-mode)
          
;;; Code:

(eval-when-compile (require 'cl))

(defgroup hl-tags nil
  "Highlight the current tag pair in XML and SGML modes."
  :group 'convenience)

(defface hl-tags-face
  '((t :inherit highlight))
  "Face used to highlight matching tags."
  :group 'hl-tags)


(defvar hl-tags-start-overlay nil)
(make-variable-buffer-local 'hl-tags-start-overlay)

(defvar hl-tags-end-overlay nil)
(make-variable-buffer-local 'hl-tags-end-overlay)


(defun hl-tags-sgml-get-context ()
  (save-excursion (car (last (sgml-get-context)))))

(defun hl-tags-sgml-pair (ctx)
  (if ctx (cons (sgml-tag-start ctx) (sgml-tag-end ctx))
    '(1 . 1)))

(defun hl-tags-context-sgml-mode ()
  (save-excursion
    (when (looking-at "<") (forward-char 1))
    (let* ((ctx (hl-tags-sgml-get-context))
           (boundaries
            (and ctx (case (sgml-tag-type ctx)
                       ('empty (cons ctx nil))
                       ('close
                        (goto-char (sgml-tag-start ctx))
                        (cons (hl-tags-sgml-get-context) ctx))
                       ('open 
                        (goto-char (sgml-tag-start ctx))
                        (sgml-skip-tag-forward 1)
                        (backward-char 1)
                        (cons ctx (hl-tags-sgml-get-context)))))))
      (when boundaries
        (cons (hl-tags-sgml-pair (car boundaries))
              (hl-tags-sgml-pair (cdr boundaries)))))))

(defun hl-tags-context-nxml-mode ()
  (condition-case nil
      (save-excursion
        (let (start1 end1 start2 end2)
          (when (looking-at "<") (forward-char))
          (nxml-up-element 1)
          (setq end2 (point))

          (nxml-backward-single-balanced-item)
          (setq start2 (point))

          (nxml-up-element -1)
          (setq end1 (point))

          (nxml-forward-single-balanced-item)
          (setq start1 (point))

          (cons (cons start1 end1) (cons start2 end2))))
    (error nil)))

(defun hl-tags-context ()
  "Return a pair ((start . end) . (start . end)) containing the
boundaries of the current start and end tag , or nil."
  (if (eq major-mode 'nxml-mode)
      (hl-tags-context-nxml-mode)
    (hl-tags-context-sgml-mode)))

(defun hl-tags-update ()
  (let ((ctx (hl-tags-context)))
    (if (null ctx)
        (hl-tags-hide)
      (hl-tags-show)
      (move-overlay hl-tags-start-overlay (caar ctx) (cdar ctx))
      (move-overlay hl-tags-end-overlay (cadr ctx) (cddr ctx)))))

(defun hl-tags-show ()
  (unless hl-tags-start-overlay
    (setq hl-tags-start-overlay (make-overlay 1 1)
          hl-tags-end-overlay (make-overlay 1 1))
    (overlay-put hl-tags-start-overlay 'face 'hl-tags-face)
    (overlay-put hl-tags-end-overlay 'face 'hl-tags-face)))

(defun hl-tags-hide ()
  (when hl-tags-start-overlay
    (delete-overlay hl-tags-start-overlay)
    (delete-overlay hl-tags-end-overlay)))

(define-minor-mode hl-tags-mode
  "Toggle hl-tags-mode."
  nil "" nil
  (if hl-tags-mode
      (progn 
        (add-hook 'post-command-hook 'hl-tags-update nil t)
        (add-hook 'change-major-mode-hook 'hl-tags-hide nil t))
    (remove-hook 'post-command-hook 'hl-tags-update t)
    (remove-hook 'change-major-mode-hook 'hl-tags-hide t)
    (hl-tags-hide)))


(provide 'hl-tags-mode)

(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

(use-package tuareg)
(use-package merlin)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))

(add-hook 'merlin-mode-hook 'company-mode)

(defun l-config-merlin () '((logfile . "/tmp/merlin")))
(setq merlin-debug t)

(setq merlin-configuration-function #'l-config-merlin)

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(use-package reason-mode

  :config (progn
            (let* ((refmt-bin (or (shell-cmd "refmt ----where")
                                  (shell-cmd "which refmt")))
                   (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
                                   (shell-cmd "which ocamlmerlin")))
                   (merlin-base-dir (when merlin-bin
                                      (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
              ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
              (when merlin-bin
                (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
                (setq merlin-command merlin-bin))

              (when refmt-bin
                (setq refmt-command refmt-bin)))
            (require 'merlin)
            (add-hook 'reason-mode-hook (lambda ()
                                          (add-hook 'before-save-hook 'refmt-before-save)
                                          (merlin-mode)))

            (setq merlin-ac-setup t)))

(use-package cider
  :defer t
  :init
  (setq cider-prompt-for-symbol nil
        cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

(use-package clojure-cheatsheet :defer t)

;(use-package cider-hydra :defer t)

;(add-hook 'cider-mode-hook #'cider-hydra-mode)

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
  (bind-key "C-c r" 'tide-references typescript-mode-map)
  (setq typescript-indent-level 2)
  (abbrev-mode 1)
  (company-mode +1))

(use-package tide
  :init (progn
          ;; aligns annotation to the right hand side
          (setq company-tooltip-align-annotations t)
          (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

          (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

          (add-hook 'typescript-mode-hook #'setup-tide-mode)))

;(use-package ng2-mode)

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

(use-package lispy :defer t
  :bind (
         :map lispy-mode-map
         ("C-1" . 'my-map)))

  ;; :init
(dolist (hook '(emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1))))

(use-package lua-mode)

(use-package php-mode)

(use-package markdown-mode :defer t)

(require 'ob-gnuplot)
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)
                             (gnuplot . t)
                             (emacs-lisp . t)
                             (http . t)
                             (org . t)))

(setq org-babel-default-header-args:sh
  '((:prologue . "exec 2>&1") (:epilogue . ":")))

(desktop-save-mode -1)
(setq desktop-restore-eager 10)

(use-package yasnippet)
(use-package yankpad)

(use-package multiple-cursors
  :config
  (progn
    ;; This is globally useful, so it goes under `C-x', and `m'
    ;; for "multiple-cursors" is easy to remember.
    (define-key ctl-x-map "\C-m" #'mc/mark-all-dwim)

    ;; Remember `er/expand-region' is bound to M-2!
    (global-set-key (kbd "M-3") #'mc/mark-next-like-this)
    (global-set-key (kbd "M-4") #'mc/mark-previous-like-this)

    ;; These vary between keyboards. They're supposed to be
    ;; Shifted versions of the two above.
    (global-set-key (kbd "M-#") #'mc/unmark-next-like-this)
    (global-set-key (kbd "M-$") #'mc/unmark-previous-like-this)

    (define-prefix-command 'endless/mc-map)
    ;; C-x m is usually `compose-mail'. Bind it to something
    ;; else if you use this command.
    (define-key ctl-x-map "m" 'endless/mc-map)

    ;; Really really nice!
    (define-key endless/mc-map "i" #'mc/insert-numbers)
    (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
    (define-key endless/mc-map "a" #'mc/mark-all-like-this)

    ;; Occasionally useful
    (define-key endless/mc-map "d"
      #'mc/mark-all-symbols-like-this-in-defun)
    (define-key endless/mc-map "r" #'mc/reverse-regions)
    (define-key endless/mc-map "s" #'mc/sort-regions)
    (define-key endless/mc-map "l" #'mc/edit-lines)
    (define-key endless/mc-map "\C-a"
      #'mc/edit-beginnings-of-lines)
    (define-key endless/mc-map "\C-e"
      #'mc/edit-ends-of-lines)

    ;; Usually, both `C-x C-m' and `C-x RET' invoke the
    ;; `mule-keymap', but that's a waste of keys. Here we put it
    ;; _just_ under `C-x RET'.
    (define-key ctl-x-map (kbd "<return>") mule-keymap)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'org-src-mode-hook 'remove-dos-eol)
(add-hook 'org-mode-hook 'remove-dos-eol)

(use-package xml-rpc)
(use-package metaweblog)
(use-package htmlize)
(use-package org2blog)

(require 'xml-rpc)
(require 'org2blog-autoloads)
(require 'netrc)


(setq asyncthinking (netrc-machine (netrc-parse "~/.netrc") "asyncthinking" t))
 
;; Setting your Blog configuration.
(setq org2blog/wp-blog-alist
      `(("asyncthinking"
         :url "https://asyncthinking.wordpress.com/xmlrpc.php"
         :username ,(netrc-get asyncthinking "login")
         :password ,(netrc-get asyncthinking "password")
         :default-title "Hello World"
         :default-categories ("")
         :tags-as-categories nil)))


(progn
  ;; implemented as HTML styling. Your pick!
  (setq org2blog/wp-use-sourcecode-shortcode 't)
  
  ;; removed light="true"
  (setq org2blog/wp-sourcecode-default-params nil)
  
  ;; target language needs to be in here
  (setq org2blog/wp-sourcecode-langs
        '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
          "erlang" "fsharp" "diff" "groovy" "html" "javascript" "java" "javafx" "matlab"
          "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
          "vb" "xml"
          "sh" "emacs-lisp" "lisp" "lua"))
  
  (setq org-src-fontify-natively t))

(require 'dired+)

(add-to-list 'load-path (concat l-elisp-home "lib/bookmark+"))
(require 'bookmark+)

(use-package vagrant-tramp)
(use-package counsel-tramp)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
