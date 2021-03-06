(defun l-getenv (var)
  (shell-command-to-string (concat ". ~/.bashrc; echo -n $" var)))

(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; lines at a time
    
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(set-register ?v '(file . "/Users/lyn_headley/Workspace/acme/DLE-Acme-Vivarium/public/svn/sn_0dce"))
(set-register ?g '(file . "/Users/lyn_headley/Workspace/glossary/g"))
(set-register ?a '(file . "/Users/lyn_headley/Workspace/asset-registry/a"))
(set-register ?f '(file . "/Users/lyn_headley/Workspace/docs/org/non-agenda/frontend.org"))
(set-register ?b '(file . "/Users/lyn_headley/Workspace/bb-scripts"))

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

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(defconst l-drill-org-template
  "** Lyn Headley
***                                                                 :drill:
Lyn Headley Boss
**** 
jj-Boss
***                                                                 :drill:
Lyn Headley joined
**** 
jj-Joined
***                                                                 :drill:
Lyn Headley title
**** 
jj-Title
")

;; insert template
(defun l-insert-l-drill-org-template ()
  (interactive)
  (beginning-of-line)
  (let* ((new (read-from-minibuffer "Employee name: "))
         (boss (read-from-minibuffer "Boss: "))
         (joined (read-from-minibuffer "Joined: "))
         (title (read-from-minibuffer "Title: "))
         (new-replaced
          (replace-regexp-in-string
           "Lyn Headley"
           new
           (replace-regexp-in-string
            "jj-Boss"
            boss
            (replace-regexp-in-string
             "jj-Joined"
             joined
             (replace-regexp-in-string
              "jj-Title"
              title
              l-drill-org-template))))))
    (save-excursion (insert new-replaced))))


;; run saved macro: narrow, replace

(use-package transient)

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
