(setq org-roam-v2-ack t)

(use-package org-roam
  :custom (org-roam-directory (concat org-directory "/roam"))
  :bind (:map global-map
              (("C-c n f" . org-roam-node-find))
              :map org-mode-map
              (("C-c n l" . org-roam-buffer-toggle)
               ("C-c n f" . org-roam-node-find)
               ("C-c n d" . org-id-get-create) ; make the current heading a node
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph)
               ("C-c n i" . org-roam-node-insert))))

(org-roam-db-autosync-mode)

(setq org-roam-mode-section-functions
      (list #'org-roam-backlinks-section))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(defun l-turn-on-visual-line-mode ()
  (when (and (fboundp 'org-roam-file-p)
	     (org-roam-file-p))
    (visual-line-mode 1)))

(add-hook 'find-file-hook #'l-turn-on-visual-line-mode)

(setq org-roam-capture-templates
      `(("p" "Permanent Note" plain "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)

        ("f" "Fleeting Note" plain "%?"
         :if-new (file+head "fleeting/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n")
         :unnarrowed t)

        ("t" "Time Note" plain "%?"
         :if-new (file+head "fleeting/%(my/initiate-org-roam-time-note).org"
                            "#+TITLE: %(eval my/*time-note-last-time*)--${title}

[[tsl:%(eval my/*time-note-last-time*)]]

")
         :unnarrowed t)

        ("l" "Literature Note" plain "%?"
         :if-new (file+head "literature/${slug}.org"
                            "#+TITLE: ${title}
#+ROAM_KEY: ${ref}")
         :unnarrowed t)
        ))

;; (setq org-roam-tag-sources '(prop all-directories))

