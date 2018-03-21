(defun l-getenv (var)
  (shell-command-to-string (concat ". ~/.bashrc; echo -n $" var)))

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

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(defvar l-drill-org-template
  "** Lyn Headley
***                                                                 :drill:
SCHEDULED: <2018-03-20 Tue>
:PROPERTIES:
:ID:       EB027A26-BFF4-4F08-BF9E-17D80E001209
:DRILL_LAST_INTERVAL: 4.0
:DRILL_REPEATS_SINCE_FAIL: 2
:DRILL_TOTAL_REPEATS: 1
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: 4.0
:DRILL_EASE: 2.5
:DRILL_LAST_QUALITY: 4
:DRILL_LAST_REVIEWED: [2018-03-16 Fri 09:26]
:END:
Lyn Headley boss
**** 
Matt Litwin
***                                                                 :drill:
SCHEDULED: <2018-03-20 Tue>
:PROPERTIES:
:ID:       EB027A26-BFF4-4F08-BF9E-17D80E001209
:DRILL_LAST_INTERVAL: 4.0
:DRILL_REPEATS_SINCE_FAIL: 2
:DRILL_TOTAL_REPEATS: 1
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: 4.0
:DRILL_EASE: 2.5
:DRILL_LAST_QUALITY: 4
:DRILL_LAST_REVIEWED: [2018-03-16 Fri 09:26]
:END:
Lyn Headley joined
**** 
2014
***                                                                 :drill:
SCHEDULED: <2018-03-20 Tue>
:PROPERTIES:
:ID:       EB027A26-BFF4-4F08-BF9E-17D80E001209
:DRILL_LAST_INTERVAL: 4.0
:DRILL_REPEATS_SINCE_FAIL: 2
:DRILL_TOTAL_REPEATS: 1
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: 4.0
:DRILL_EASE: 2.5
:DRILL_LAST_QUALITY: 4
:DRILL_LAST_REVIEWED: [2018-03-16 Fri 09:26]
:END:
Lyn Headley title
**** 
Web App Developer
")

;; insert template
(defun l-insert-l-drill-org-template ()
  (beginning-of-line)
  (save-excursion
    (insert l-drill-org-template))
  (narrow-or-widen-dwim)
  (let ((new (read-from-minibuffer "Employee name: "))
        (new-replaced
         (replace-regexp-in-string
          "Lyn Headley"
          "new"
          l-drill-org-template)))
    (save-excursion (insert new-replaced))
    (narrow-or-widen-dwim)))


;; run saved macro: narrow, replace

(set-register ?d '(file . "/lheadley@dn.kcptech.com:/home/lheadley"))

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.mheducation.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.mheducation\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))
