;; If a headline has tags, and all of them belonged to this set, then it is not a node
(defvar my-roam-node-disqualifying-tags '("ATTACH" "CANCELLED"))

(defun my-node-is-good-node ()
  (let* ((tags (org-get-tags)))
    (or (not tags)
        (--filter (not (member it my-roam-node-disqualifying-tags))
                  tags))))

(setq org-roam-db-node-include-function 'my-node-is-good-node)

;; remove?
(setq org-roam-v2-ack t)

(defun my-org-roam-buffer-title (buffer)
  (with-current-buffer buffer
   (org-with-point-at 1
     (when-let ((n (org-roam-node-at-point)))
       (org-roam-node-title n)))))

(defun my-read-org-roam-buffer ()
  (let* ((candidates (--map
                      (cons (my-org-roam-buffer-title it) it)
                      (org-roam-buffer-list)))
         (chosen (completing-read "Buffer: " candidates)))
    (cdr (assoc chosen candidates))))

;; I am not really using this. 
(defun my-switch-to-org-roam-buffer (buffer)
  (interactive (list (my-read-org-roam-buffer)))
  (if (buffer-live-p buffer)
      (switch-to-buffer buffer)))

;; I am using this
(defun my-rename-org-roam-file-buffer ()
  (when (org-roam-file-p)
    (when-let ((title (my-org-roam-buffer-title (current-buffer))))
      (rename-buffer title))))

(add-hook 'find-file-hook 'my-rename-org-roam-file-buffer)

(use-package org-roam
  :demand t
  :custom (org-roam-directory (concat org-directory "/roam"))
  :config (progn (setq org-roam-dailies-directory "daily/")
                 (setq org-roam-dailies-capture-templates
                       '(("d" "default" entry
                          "* %?"
                          :target (file+head "%<%Y-%m-%d>.org"
                                             "#+title: %<%Y-%m-%d>\n")))))
  :bind (:map global-map
              (("C-c n c" . company-complete)
               ("C-c n l" . org-roam-buffer-toggle)
               ("C-c n f" . org-roam-node-find)
               ("C-c n d" . org-id-get-create)
               ("C-c n i" . org-roam-node-insert)
               ("C-c n r" . org-roam-refile)
               ("C-c n b" . my-switch-to-org-roam-buffer)
               ("C-c n t" . org-roam-dailies-capture-today)
               ("C-c n g" . org-roam-dailies-goto-date)
               ("C-c n w" . my-org-copy-text-under-heading))))

(org-roam-db-autosync-mode)

(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section))

;; (add-to-list 'display-buffer-alist
;;              '("\\*org-roam\\*"
;;                (display-buffer-in-direction)
;;                (direction . below)
;;                (window-width . fit-window-to-buffer)
;;                (window-height . 0.33)))
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-same-window)))

(defun l-turn-on-visual-line-mode ()
  (when (and (fboundp 'org-roam-file-p)
	     (org-roam-file-p))
    (visual-line-mode 1)))

(add-hook 'find-file-hook #'l-turn-on-visual-line-mode)

(setq org-roam-capture-templates
      `(("p" "Permanent Note" plain "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

;; (setq org-roam-tag-sources '(prop all-directories))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:30}" 'face 'org-tag)))