(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(bmkp-last-as-first-bookmark-file nil)
 '(debug-on-error t)
 '(js-indent-level 2)
 '(js2-bounce-indent-p nil)
 '(magit-list-refs-sortby "-creatordate")
 '(magit-reset-arguments nil)
 '(package-selected-packages
   '(org-transclusion bookmark+ quelpa-use-package quelpa org-jira hydra counsel swiper transient exec-path-from-shell which-key free-keys ht dash org-contrib use-package))
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-context ((t (:inherit ##))))
 '(highlight ((t (:inverse-video nil :background "white" :inherit hl-line))))
 '(hl-line ((t (:extend nil :background "white" :foreground "dark red"))))
 '(lsp-ui-peek-highlight ((t (:background "gray100" :distant-foreground "black" :foreground "white"))))
 '(org-agenda-clocking ((t (:foreground "light green"))) nil "secondary selection was not working for me")
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))))
 '(secondary-selection ((t (:extend nil :background "gray96" :foreground "MediumOrchid3")))))
