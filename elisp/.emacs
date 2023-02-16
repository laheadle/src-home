(defvar l-src-home (file-truename "~/src/home/"))
(defvar l-elisp-home (file-truename "~/src/home/elisp/"))

(prefer-coding-system 'utf-8-unix)

(setq custom-file (expand-file-name "custom.el" l-elisp-home))

(when (file-exists-p custom-file)
  (load custom-file))

(setq
 package-enable-at-startup nil
 package-archives
 '(("melpa-stable" . "https://stable.melpa.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
   ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities '(("melpa-stable" . 100)))

(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

;(setq debug-on-quit t)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; broken notifications -- workaround [2020-02-14 Fri]
(setq org-show-notification-handler (lambda (msg) (message "%s" msg)))

(use-package org :pin "gnu")
(use-package org-contrib :pin "nongnu")

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

;; refile in reverse-chron
(setq org-reverse-note-order t)

(use-package dash)
(use-package ht)

(defmacro my-save-excursion (&rest forms)
  (let ((old-point (gensym "old-point"))
        (old-buff (gensym "old-buff")))
    `(let ((,old-point (point))
           (,old-buff (current-buffer)))
       (prog1
           (progn ,@forms)
         (unless (eq (current-buffer) ,old-buff)
           (switch-to-buffer ,old-buff))
         (goto-char ,old-point)))))

(use-package free-keys :defer t)
(use-package bind-key  :defer t)
(define-prefix-command 'my-map)

(use-package which-key)

(defvar l-env-file (concat l-elisp-home "environment.el"))

(defun read-file (filePath)
  "Return filePath's file content as lisp object"
  (with-temp-buffer
    (insert-file-contents filePath)
    (read (buffer-string))))

(setq l-env (read-file l-env-file))

(defun load-shared (personal-library-name)
  (load-file (concat l-elisp-home personal-library-name ".el")))

(defun load-env ()
  (load-file (concat l-elisp-home l-env ".el")))

(defun load-from-env (code-name)
  (load-file (concat l-elisp-home code-name "-" l-env ".el")))

(load-env)

;; fix me. examples
;; attach org attachment using move method
(defun l-c-o () (interactive)
       (global-set-key (kbd "C-x C-f") 'counsel-find-file)
       (setq read-file-name-function 'read-file-name-default))

(use-package swiper)
(use-package counsel
  :bind (("C-b" . ivy-switch-buffer)
         ("C-s" . swiper))
  :config (ivy-mode 1)
  :init
  (progn
    (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
    (setq ivy-wrap t)
    (setq ivy-use-virtual-buffers t
          ivy-height 20)
    (setq enable-recursive-minibuffers t)
    (setq counsel-rg-base-command
          "rg -M 180 --no-heading --line-number --color never %s .")
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

(use-package company)

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

;; acme css indent with tabs

(defun l-indent-with-tabs ()
  (interactive)
  (setq-local css-indent-offset 4)
  (setq-local indent-tabs-mode t))

(defun l-set-tab-width ()
  (setq tab-width (if (eq major-mode 'emacs-lisp-mode) 8 4)))
(add-hook 'emacs-lisp-mode-hook 'l-set-tab-width)
(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (l-set-tab-width)))

(require 'highlight-indentation)

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)

(use-package smart-shift
  :config (global-smart-shift-mode 1))

(use-package wgrep
:config (setq wgrep-auto-save-buffer t))

(use-package avy)
(global-set-key (kbd "M-z") 'avy-goto-char)
(global-set-key (kbd "M-h") 'avy-goto-word-1)
(global-set-key (kbd "M-'") 'avy-goto-line)

(bind-key "M-h" 'avy-goto-word-1 org-mode-map)
(bind-key "M-'" 'avy-goto-line org-mode-map)
;; (use-package ace-jump-mode
;;   :bind ("M-h" . ace-jump-mode)
;;   ("M-z" . ace-jump-char-mode))

;; (bind-key "M-h" 'ace-jump-mode org-mode-map)

(put 'upcase-region 'disabled nil)

(delete-selection-mode 1)

(use-package expand-region
  :bind ("C-@" . er/expand-region))

(define-key global-map (kbd "M-k")
  (lambda () (interactive)
    (kill-ring-save (point) (line-end-position))))

(set-face-attribute 'region nil :background "#bbb")
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

(defadvice kill-region (before slick-cut activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo)))

(use-package s)

(setq recentf-max-saved-items 100)

(setq make-backup-files nil)

(defun l-backups ()
  (shell-command "backups.sh&"))

(defvar one-hour (* 60 60))

(run-with-timer one-hour (* 24 one-hour) 'l-backups)

(ffap-bindings)
(setq ffap-require-prefix t)

;; watch the file system

(defun l-copy-current-directory ()
  (interactive)
  (kill-new default-directory))

(global-auto-revert-mode nil)

(setq dired-guess-shell-alist-user
      (list
       (list "\\.zip$" "munzip")))


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



(defhydra hydra-windows-nav (:color red)
    ("s" shrink-window-horizontally "shrink horizontally" :column "Sizing")
    ("e" enlarge-window-horizontally "enlarge horizontally")
    ("a" shrink-window "shrink vertically" :column "Sizing")
    ("c" enlarge-window "enlarge vertically")
    ("b" balance-windows "balance window height")
    ("m" maximize-window "maximize current window")
    ("M" minimize-window "minimize current window")
    
    ("w" split-window-below "split horizontally" :column "Split management")
    ("v" split-window-right "split vertically")
    ("d" delete-window "delete current window")
    ("x" delete-other-windows "delete-other-windows")

   
    ("z" ace-window "ace window" :color blue :column "Navigation")
    ("h" windmove-left "← window")
    ("j" windmove-down "↓ window")
    ("k" windmove-up "↑ window")
    ("l" windmove-right "→ window")
    ("r" toggle-window-split "rotate windows") ; Located in utility functions
    ("q" nil "quit menu" :color blue :column nil))

(use-package ibuffer-vc)


(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

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

(use-package smartparens :pin "melpa")
(require 'smartparens-config)



(global-eldoc-mode -1)
 
(use-package with-editor)

(fset 'yes-or-no-p 'y-or-n-p)

;; irc
(setq circe-reduce-lurker-spam t)

(server-start)

(use-package yaml-mode
  :hook ((yaml-mode . highlight-indentation-mode))
  :init (progn (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
               (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))))

(column-number-mode)

(setq debug-on-error t)

(use-package ob-http)

(use-package csv-mode
  :defer 60)

(add-hook 'csv-mode-hook #'csv-align-mode)

(use-package gnuplot)

(defun my-vc-dir-delete-marked-files ()
  "Delete all marked files in a `vc-dir' buffer."
  (interactive)
  (let ((files (vc-dir-marked-files)))
    (if (not files)
        (message "No marked files.")
      (when (yes-or-no-p (format "%s %d marked file(s)? "
                                 (if delete-by-moving-to-trash "Trash" "Delete")
                                 (length files)))
        (unwind-protect
            (mapcar
             (lambda (path)
               (if (and (file-directory-p path)
                        (not (file-symlink-p path)))
                   (when (or (not (directory-files
                                   path nil directory-files-no-dot-files-regexp))
                             (y-or-n-p
                              (format "Directory `%s' is not empty, really %s? "
                                      path (if delete-by-moving-to-trash
                                               "trash" "delete"))))
                     (delete-directory path t t))
                 (delete-file path t)))
             files)
          (revert-buffer))))))

(eval-after-load 'vc-dir
  '(define-key vc-dir-mode-map (kbd "k") 'my-vc-dir-delete-marked-files))

(defun l-merge-upstream ()
  (interactive)
  (magit-merge "@{upstream}"))

(use-package magit
  :init (setq 
         magit-commit-show-diff nil
         magit-completing-read-function 'ivy-completing-read
         magit-revert-buffers 1
         magit-diff-refine-hunk 'all
         magit-visit-ref-create t)
  :config (progn
            (transient-append-suffix 'magit-merge "m"
              '("u" "Catch Up To Upstream" l-merge-upstream)))
  :bind (("C-." . magit-status)
         :map magit-mode-map
         ("C-c v" . endless/visit-pull-request-url)))



(use-package git-gutter
  :config (global-git-gutter-mode +1))

(fset 'l-jump-to-repo-and-show-branches
   [?\C-e ?\C-a ?\C-  ?\C-e ?\M-w ?\C-u ?\C-c ?\C-w ?  ?\C-y return ?\C-. ?f ?a ?y])

(defun my-display-buffer (buffer)
  (display-buffer
   buffer '(display-buffer-same-window)))
(setq magit-display-buffer-function #'my-display-buffer)

(use-package flycheck :pin "melpa")

(use-package go-mode :mode ("\\.go$" . go-mode)
  :bind (;; ("C-c r" . lsp-find-references)
         ;; ("C-c e" . lsp-treemacs-errors-list)
         :map go-mode-map)
  
    
  :config (progn 
            (add-hook 'go-mode-hook #'lsp-deferred)
            (add-hook 'go-mode-hook #'yas-minor-mode)))

(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :pin "melpa"
  :config (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-ui :pin "melpa")
;; (use-package lsp-treemacs :pin "melpa")

(use-package helm-lsp :pin "melpa")

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

(use-package rainbow-delimiters)


(use-package cider :pin "melpa-stable"
  :defer t
  :config
  (setq cider-repl-history-file (expand-file-name "~/.emacs.d/cider-history"))
  (setq cider-repl-display-help-banner nil)
  (global-set-key (kbd "C-c s") #'cider-selector)
  (setq cider-format-code-options
        '(("remove-consecutive-blank-lines?" t)
          ("remove-multiple-non-indenting-spaces?" t)))
  (setq cider-save-file-on-load t)
  (setq cider-print-quota (* 3 (* 1000 1000)))
  (setq cider-stacktrace-default-filters '(project))
  (setq cider-prompt-for-symbol nil)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  )
(use-package clj-refactor)

(defun my-cider-history-hook ()
  (lispy-mode -1))
(add-hook 'cider-repl-history-mode-hook #'my-cider-history-hook)

(use-package zprint-mode :pin "melpa")

(defun my-clojure-mode-hook ()
  ;; (clj-refactor-mode 1)
  ;; (zprint-mode 1)
  (hs-minor-mode 1)
  (rainbow-delimiters-mode)
  ;; (yas-minor-mode 1)
                                        ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (lsp)
  (which-key-mode)
  (company-mode)
  (lispy-mode)
  (define-key lispy-mode-map (kbd "C-2") lsp-command-map)
  (define-key lsp-mode-map (kbd "C-2") lsp-command-map))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(use-package cljr-helm :pin "melpa"
  :bind (:map clojure-mode-map
              ("C-c C-r" . cljr-helm)))

;; ;(use-package clojure-cheatsheet :defer t)

;; (defun my-cider--nrepl-format-code-request-map (original-function &optional arguments)
;;   '(dict
;;     "remove-consecutive-blank-lines?"
;;     "false"
;;     "split-keypairs-over-multiple-lines?"
;;     "true"
;;     "remove-multiple-non-indenting-spaces?"
;;     "true"))

;; (advice-add 'cider--nrepl-format-code-request-map :around #'my-cider--nrepl-format-code-request-map)

;(use-package cider-hydra :defer t)

;(add-hook 'cider-mode-hook #'cider-hydra-mode)

;; seems to work despite this error:
;; clomacs.el:626:1:Error: Symbol’s function definition is void: clomacs-prepare-vars
(use-package clomacs)

(add-hook 'js-mode-hook (lambda () (abbrev-mode 1)))

(setq js-indent-level 2)
(setq-default indent-tabs-mode nil)

(use-package js2-mode
  :config (progn
          (setq js2-mode-hook
                (lambda ()
                   (progn
                     (show-paren-mode 1)
                     (add-hook 'js-mode-hook #'smartparens-strict-mode)
                     (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                     (define-key js2-mode-map "@" 'js-doc-insert-tag)
                     (electric-indent-local-mode -1)
                     (set-variable 'indent-tabs-mode nil))))

          (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))))


;; (use-package indium)

;; (use-package nvm)

;; (defun do-nvm-use (version)
;;   (interactive "sVersion: ")
;;   (nvm-use version))

;; (defun l-show-nvm-installed-versions ()
;;   (interactive)
;;   (message "%s" (nvm--installed-versions)))

;; (defun run-node (cwd)
;;   (interactive "DDirectory: ")
;;   (unless (executable-find "node")
;;     (call-interactively 'do-nvm-use))
;;   (let ((default-directory cwd))
;;         (pop-to-buffer (make-comint (format "node-repl-%s" cwd) "node" nil "--interactive"))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (bind-key "C-c r" 'tide-references typescript-mode-map)
  (setq typescript-indent-level 4)
  (abbrev-mode 1)
  (company-mode +1))


(defun l-switch-to-partner-file ()
  (interactive)
  (let* ((buff buffer-file-name)
         (elts (split-string buff "\\."))
         (suffix (car (last elts)))
         (nsuffix (cond ((equal suffix "ts") "scss")
                            ((equal suffix "scss") "html")
                            ((equal suffix "html") "ts")
                            (t (error buff))))
         (nfile (concat (string-join (butlast elts) ".")
                        "."
                        nsuffix)))
    (find-file nfile)))

(bind-key "C-x d" 'l-switch-to-partner-file global-map)

(use-package tide :pin "melpa"
  :init (progn
          ;; aligns annotation to the right hand side
          (setq company-tooltip-align-annotations t)
          (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

          ;; (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

          (add-hook 'typescript-mode-hook #'setup-tide-mode)))

;(use-package ng2-mode)

(show-paren-mode)

(use-package lispy :pin "melpa" :defer t
  :config
  (setq lispy-compat
        '(edebug cider))
  :bind (:map lispy-mode-map
              ("C-1" . 'my-main-hydra/body)
              ("M-k" . 'lispy-new-copy)
              ( "M-h" . 'avy-goto-word-1 )
              ("M-'" . 'avy-goto-line )))

  ;; :init
(dolist (hook '(emacs-lisp-mode-hook))
  (add-hook hook (lambda ()
                   (lispy-mode 1)
                   (which-key-mode)
                   (company-mode))))

(use-package php-mode
  :bind (
         :map php-mode-map
         ("C-." . magit-status)))

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

(add-hook 'scss-mode-hook (lambda () (setq css-indent-offset 2)))

;; PROBLEM
;; need to use java eleven for this, but my emacs can only use java eight for some reason
;; which java prints out one thing, while M-! which java prints out something else

;; relevant sdk commands
;; sdk list java
;; sdk use java 11.0.17-amzn
;; sdk default java 8.0.292.hs-adpt

;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))


;; this does not seem to have any effect
;; (setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
;; 					       :path "/Users/lyn_headley/.sdkman/candidates/java/8.0.292.hs-adpt")
;; 					(:name "JavaSE-11"
;; 					       :path "/Users/lyn_headley/.sdkman/candidates/java/11.0.17-amzn/"
;; 					       :default t)])
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)

(use-package  color-theme-sanityinc-tomorrow)

(load-theme 'sanityinc-tomorrow-bright t)

;; (use-package  spacemacs-theme :pin "melpa")
;; (load-theme 'spacemacs-theme t)

(desktop-save-mode -1)
(setq desktop-restore-eager 10)

(use-package yasnippet)
(use-package yankpad)

(setq yas-snippet-dirs (list (concat l-elisp-home "snippets")))
(yas-global-mode 1)

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

;; (use-package org2blog)

;; (require 'xml-rpc)
;; (require 'org2blog-autoloads)
;; (require 'netrc)


;; (setq asyncthinking (netrc-machine (netrc-parse "~/.netrc") "asyncthinking" t))
 
;; ;; Setting your Blog configuration.
;; (setq org2blog/wp-blog-alist
;;       `(("asyncthinking"
;;          :url "https://asyncthinking.wordpress.com/xmlrpc.php"
;;          :username ,(netrc-get asyncthinking "login")
;;          :password ,(netrc-get asyncthinking "password")
;;          :default-title "Hello World"
;;          :default-categories ("")
;;          :tags-as-categories nil)))


;; (progn
;;   ;; implemented as HTML styling. Your pick!
;;   (setq org2blog/wp-use-sourcecode-shortcode 't)
  
;;   ;; removed light="true"
;;   (setq org2blog/wp-sourcecode-default-params nil)
  
;;   ;; target language needs to be in here
;;   (setq org2blog/wp-sourcecode-langs
;;         '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
;;           "erlang" "fsharp" "diff" "groovy" "html" "javascript" "java" "javafx" "matlab"
;;           "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql"
;;           "vb" "xml"
;;           "sh" "emacs-lisp" "lisp" "lua"))
  
;;   (setq org-src-fontify-natively t))

;; (require 'dired+)

(use-package docker-tramp)

(let ((bookmarkplus-dir "~/.emacs.d/custom/bookmark-plus/")
      (emacswiki-base "https://www.emacswiki.org/emacs/download/")
      (bookmark-files '("bookmark+.el" "bookmark+-mac.el" "bookmark+-bmu.el" "bookmark+-key.el" "bookmark+-lit.el" "bookmark+-1.el")))
  (require 'url)
  (add-to-list 'load-path bookmarkplus-dir)
  (make-directory bookmarkplus-dir t)
  (mapcar (lambda (arg)
            (let ((local-file (concat bookmarkplus-dir arg)))
              (unless (file-exists-p local-file)
                (url-copy-file (concat emacswiki-base arg) local-file t))))
          bookmark-files)
  (byte-recompile-directory bookmarkplus-dir 0)
  (require 'bookmark+))

;; (add-to-list 'load-path (concat l-elisp-home "lib/bookmark+"))
;; (require 'bookmark+)

;; (use-package vagrant-tramp)
(use-package counsel-tramp)

(add-to-list 'load-path (concat l-elisp-home "lib/graph.el"))
(require 'graph)

(defun l-remove-spaces-dashes ()
  (interactive)
  (query-replace "- " "" nil (point-min) (point-max)))

(defun l-publish-darwin ()
  (interactive)
  (org-publish-project (quote (#("darwin" 0 1 (idx 2)) :components ("darwin-docs" "darwin-imgs"))) nil))

(defalias 'hw 'hydra-windows-nav/body)
(defalias 'aj 'aj-toggle-fold)
(defalias 'tf 'tide-fix)
(defalias 'jpp 'json-pretty-print)
(defalias 'lcka 'lh-copy-to-kill-ring--avalon-proj1)
(defalias 'lckg 'lh-copy-to-kill-ring--graphiql)
(defalias 'rss 'l-remove-spaces-dashes)
(defalias 'hd 'helm-lsp-diagnostics)
(defalias 'opp 'l-publish-darwin)

(load-shared "personal-utilities")

(global-so-long-mode 1)

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

(eval-after-load 'org-src
  '(define-key org-src-mode-map
     "\C-x\C-s" #'org-edit-src-exit))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("javascript" . js2)))

(defun l-beginning-of-block ()
  (interactive)
  (org-babel-mark-block)
  (exchange-point-and-mark))

  (setq org-M-RET-may-split-line '((default . nil)))

(fset 'l-org-new-line-at-end
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 14 left return] 0 "%d")) arg)))

(defun create-shell ()
  "creates a shell with a given name"
  (interactive) ;; "Prompt\n shell name:")
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(use-package org-super-agenda
  :config
  (progn
    (org-super-agenda-mode)
    (setq org-super-agenda-groups
          '((:todo "WAITING" :order 500)
            (:name "STATUS" :order 100 :and (:habit t :regexp "status"))
            (:priority "A" :order 150)
            (:name "HABITS" :order 200 :habit t)
            (:order 300 :anything t)))))

(setq org-startup-folded 'content)

(defvar l-dot-emacs (concat l-elisp-home "dot-emacs.org"))

(defun my/tangle-on-save-emacs-config-org-file ()
  (when (string= buffer-file-name l-dot-emacs)
    (org-babel-tangle)))

;(add-hook 'after-save-hook 'my/tangle-on-save-emacs-config-org-file)

(setq org-habit-preceding-days 27
      org-habit-following-days 1)

(require 'ox-md)

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(setq org-publish-project-alist '(("darwin-docs" :base-directory "~/doc/org/2/non-agenda/project-darwin"
                                   :publishing-directory "~/doc/published-projects/darwin"
                                   :base-extension "org"
                                   :recursive t
                                   :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/project-styles.css\" />"
                                   :html-head-include-default-style nil
                                   :html-head-extra ""
                                   :publishing-function org-html-publish-to-html
                                   :auto-sitemap t
                                   :makeindex t)
                                  ("darwin-imgs" :base-directory "~/doc/org/2/non-agenda/project-darwin"
                                   :publishing-directory "~/doc/published-projects/darwin"
                                   :base-extension "png\\|jpg\\|jpeg"
                                   :recursive t
                                   :publishing-function org-publish-attachment)
                                  ("darwin" :components ("darwin-docs" "darwin-imgs"))
                                  ("stinkless" :base-directory "~/doc/org/2/non-agenda"
                                   :publishing-directory "~/doc/published-projects/stinkless"
                                   :recursive t
                                   :publishing-function org-html-publish-to-html
                                   :auto-sitemap t
                                   :makeindex t)))

(defconst org+-counselrg-link-type "counselrg"
  "String for identifying counsel-rg links.")

(defun org+-buffer-base-name (buffer-name)
  "Remove trailing <.*> in BUFFER-NAME."
  (save-match-data
    (when (string-match "<[^<>]+>\\'" buffer-name)
      (setq buffer-name (substring buffer-name nil (match-beginning 0)))))
  buffer-name)

(defun org+-counselrg-store ()
  "Store links of type counselrg:LINK-PATH."
  (when (derived-mode-p 'ivy-occur-grep-mode)
    (org-store-link-props :type org+-counselrg-link-type
              :link (format "%s:%S" org+-counselrg-link-type (cons default-directory (org+-buffer-base-name (buffer-name))))
              :description (format "%s in %s" (buffer-name) default-directory))
    t))

(defun org+-counselrg-follow (link-path)
  "Follow org-links of type counselrg:LINK-PATH."
  (let* ((caller-window (get-buffer-window))
     (dir-buf (read link-path))
     (dir-name (car dir-buf))
     (buf-name (cdr dir-buf))
     (buf
      ;; First try to re-use already existing buffer.
      (cl-loop for buf in (buffer-list)
           if (with-current-buffer buf
            (and
             (derived-mode-p 'ivy-occur-grep-mode)
             (file-equal-p default-directory dir-name)
             (string-equal
              (org+-buffer-base-name (buffer-name))
              buf-name
              )))
           do (with-current-buffer buf (ivy-occur-revert-buffer))
           and return buf
           finally
           ;; if there is no buffer for re-use:
           return
           (with-current-buffer (generate-new-buffer buf-name)
             (setq default-directory dir-name)
             (ivy-occur-grep-mode)
             (let ((inhibit-read-only t)
               (counsel-ag-base-command counsel-rg-base-command))
               (setq-local ivy-occur-last
                   (make-ivy-state
                    :directory default-directory
                    :caller 'counsel-ag
                    :window caller-window
                    :buffer (current-buffer)
                    ))
               (counsel-rg-occur))
             (current-buffer))
           )))
    (switch-to-buffer buf)))

(org-link-set-parameters org+-counselrg-link-type
             :follow #'org+-counselrg-follow
             :store #'org+-counselrg-store)

(bind-key "C-." 'l-org-jump-to-dir org-mode-map)
(bind-key "M-h" 'avy-goto-word-1 org-mode-map)
(bind-key "M-z" 'ace-jump-char-mode org-mode-map)

(add-to-list 'load-path (concat l-elisp-home "lib/org-ml"))
(require 'org-ml)

(fset 'l-update-efforts-subtree
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("q" 0 "%d")) arg)))

(defun lsc ()
  (interactive)
  (l-update-efforts-subtree)
  (org-entry-put (point) "COMMITTED_EFFORT" (org-entry-get (point) "EFFORT")))

(load (concat l-elisp-home "lib/my-org-roam.el"))

(fset 'my-save-link-to-note
      [?\C-c ?l ?\C-c ?n ?b return ?\M-< ?\C-1 ?s ?* ?  ?c ?u ?r ?r ?e ?n ?t ?  ?n ?o ?t ?e ?s return ?\C-1 ?e ?\C-c ?\C-l return return ?\C-x ?\C-s ?\C-<])
(global-set-key (kbd "C-x C-k 1") 'my-save-link-to-note)

(use-package ox-reveal :pin "melpa")

(bind-key "C-M-z" 'comment-or-uncomment-region)

(defhydra my-main-hydra (:color red)
  ("e" l-org-new-line-at-end "new line at end" :column "Org")
  ("q" counsel-rg "rip grep" :column "MISC")
  ("s" isearch-forward "isearch-forward")
  ("b" isearch-backward "isearch-backward")
  ("i" l-copy-current-directory "l-copy-current-directory")
  ("r" josh/org-refile-hydra/body "josh/refile")
  ("w" my-work-tasks/body "work tasks" :exit t))

(bind-key "C-1" 'my-main-hydra/body)