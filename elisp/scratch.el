;; A NOTE this already exists
(use-package helm-lsp
  :pin "melpa"
  :ensure t
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))
;; A NOTE this already exists
(use-package lsp-ui
  :pin "melpa"
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
              lsp-ui-doc-position 'bottom
	      lsp-ui-doc-max-width 100
              ))
(use-package lsp-treemacs
  :pin "melpa"
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :pin "melpa"
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

(use-package dap-mode
  :pin "melpa"
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package lsp-mode
  :pin "melpa"
  :ensure t
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
         (java-mode . #'lsp-deferred)
         )
  :init (setq 
         lsp-keymap-prefix "C-c l" ; this is for which-key integration documentation, need to use lsp-mode-map
         lsp-enable-file-watchers nil
         read-process-output-max (* 1024 1024) ; 1 mb
         lsp-completion-provider :capf
         lsp-idle-delay 0.500
         )
  :config 
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-java
  :pin "melpa"
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-java
  :pin "melpa"
  :ensure nil)

(setq lsp-java-java-path
      (format "%s/bin/java" (getenv "JAVA_HOME")))
(defun my-directory-status (dir)
  (interactive)
  (find-file dir) (magit-status))

;; scratch


("y" (lambda () (interactive) (my-directory-status (get-register ?a))))

(get-register ?a)
(set-register ?a default-directory)

(lambda (interactive) (switch-to-first-matching-buffer "agenda"))

(with-current-buffer (car (org-roam-buffer-list))
  (org-get-tags))

(defun my-debugger (&rest debugger-args)
  (message "BACKTRACE: %s"
           (with-temp-buffer
             (let ((standard-output (current-buffer)))
               (backtrace)
               (buffer-string)))))
(let ((debugger #'my-debugger))
  (org-roam-db-sync t))

(setq debug-on-error t)

(defun my-org-roam-db-update-file (&optional file-path no-require)
  "Update Org-roam cache for FILE-PATH.

If the file does not exist anymore, remove it from the cache.

If the file exists, update the cache with information.

If NO-REQUIRE, don't require optional libraries. Set NO-REQUIRE
when the libraries are already required at some toplevel, e.g.
in `org-roam-db-sync'."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [:select hash :from files
                                                   :where (= file $s1)] file-path)))
        info)
    (org-roam-with-file file-path nil
      (emacsql-with-transaction (org-roam-db)
        (org-with-wide-buffer
         (org-set-regexps-and-options 'tags-only)
         (org-refresh-category-properties)
         (org-roam-db-clear-file)
         (org-roam-db-insert-file)
         (org-roam-db-insert-file-node)
         (setq org-outline-path-cache nil)
         (org-roam-db-map-nodes
          (list #'org-roam-db-insert-node-data
                #'org-roam-db-insert-aliases
                #'org-roam-db-insert-tags
                #'org-roam-db-insert-refs))
         (setq org-outline-path-cache nil)
         (setq info (org-element-parse-buffer))
         (org-roam-db-map-links
          (list #'org-roam-db-insert-link))
         (when (fboundp 'org-cite-insert)
           (require 'oc)                ;ensure feature is loaded
           (org-roam-db-map-citations
            info
            (list #'org-roam-db-insert-citation))))))))

(with-current-buffer (car (org-roam-buffer-list))
  (my-org-roam-db-update-file))

(with-current-buffer (car (org-roam-buffer-list))
  (my-this-headline-has-attachment))

(defun my-element-context-property-names ()
  (interactive)
  (if-let* ((node (org-element-context))
            (type (car node))
            (properties (cadr node))
            (keys (-slice properties 0 nil 2)))
      (message "%s: %s" type keys)
    node))

(with-current-buffer (car (org-roam-buffer-list))
  (org-roam-node-at-point))


(remove-hook 'find-file-hook #'l-turn-on-visual-line-mode)
(buffer-file-name (get-buffer "Never be hungry again"))
(defun my-indirect-buffer-on-node (node)
  (let* ((marker (org-roam-node-marker node))
         (buf (marker-buffer marker)))
    (with-current-buffer buf
      (goto-char marker)
      (with-current-buffer (clone-indirect-buffer (org-roam-node-title node)
                                                  nil)
        (org-narrow-to-subtree)))))
(my-indirect-buffer-on-node (org-roam-node-from-title-or-alias "You do not poop enough"))
(defun my-indirect-buffer-on-node-at-point ()
  (my-indirect-buffer-on-node (org-roam-node-at-point)))

(org-roam-db-map-nodes)

(setq org-indirect-buffer-display 'new-frame)
(defun my-indirect-buffer-on-all-nodes-in-buffer (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (org-roam-db-map-nodes 
     (list 'my-indirect-buffer-on-node-at-point))))

(let ((buf (-> (org-roam-node-from-title-or-alias "My Diet")
               (org-roam-node-marker)
               (marker-buffer))))
  (my-indirect-buffer-on-all-nodes-in-buffer buf))


(let ((buf (-> (org-roam-node-from-title-or-alias "My Diet")
               (org-roam-node-marker)
               (marker-buffer))))
  buf)

(let ((buf (-> (org-roam-node-from-title-or-alias "My Diet")
               (org-roam-node-marker)
               (marker-buffer))))
  (with-current-buffer buf
    (cl-flet ((my-collect () (message "n %s" (org-roam-node-at-point))))
      (org-roam-db-map-nodes 
       (list #'my-collect)))))

(lexical-let* ((my-collect (lambda ()
                             (message "n %s" (org-roam-node-at-point)))))
  (funcall #'my-collect))
(cl-flet ((my-collect () (message "n %s" (org-roam-node-at-point))))
  (funcall #'my-collect))

(lexical-let* ((my-collect (lambda ()
                             (message "n %s" (org-roam-node-at-point)))))
  (org-roam-db-map-nodes 
   (list #'my-collect)))

(equal (vector 12 3)
       (vector 12 3))
(equal (vector 12 3)
       [12 3])
bookmark-alist



(defvar my-databases `(,org-roam-directory "~/doc/data/a-media-system"))
()


(define-key global-map (kbd "C-c n c") 'company-complete)
(define-key global-map (kbd "C-c n b") #'my-switch-to-org-roam-buffer)

(-map
 #'buffer-name
 (org-roam-buffer-list))

(org-roam-db-query [:select [dest] :from links :where (= type "https")])
(org-roam-db-query [:select :distinct [type] :from links])

(setq display-buffer-alist nil)
(switch-to-buffer-other-window (get-buffer "scratch.org"))
display-buffer-alist
(define-key org-mode-map (kbd "C-c n r") #'org-roam-refile)
(let ((h1-count 0))
  (incf h1-count))

#'incf

(combine-and-quote-strings ["a" 'b])
'(#(1 2))
command-history


(setq lsp-diagnostic-clean-after-change t)
(add-hook 'lsp-diagnostics-updated-hook (lambda () (message "diagnostics updated")))
(remove-hook 'lsp-diagnostics-updated-hook (lambda () (message "diagnostics updated")))
;; see buffer lsp-log, and there is one specific to the server with the same name prefix
(setq lsp-log-io t)
(setq lsp-log-io nil)

(progn
  (when (and (file-exists-p bookmark-old-default-file)  (not (file-exists-p bookmark-default-file)))
    (rename-file bookmark-old-default-file bookmark-default-file))
  (let ((file-to-load  (bmkp-default-bookmark-file)))
    (and (not bookmarks-already-loaded)
         (null bookmark-alist)
         (file-readable-p file-to-load)
         (bookmark-load file-to-load t 'nosave)
         (setq bookmarks-already-loaded  t))))

(defun cider-situated-eval (form-str)
  "evaluate a form in the context of the current package"
  (interactive)
  (let* ((form form-str)
         (override cider-interactive-eval-override)
         (ns-form (if (cider-ns-form-p form) "" (format "(ns %s)" (cider-current-ns)))))
    (with-current-buffer (get-buffer-create cider-read-eval-buffer)
      (erase-buffer)
      (clojure-mode)
      (unless (string= "" ns-form)
        (insert ns-form "\n\n"))
      (insert form)
      (let ((cider-interactive-eval-override override))
        (cider-interactive-eval form
                                nil
                                nil
                                (cider--nrepl-pr-request-map))))))

(defun my-rich-reload-code ()
  (interactive)
  (cider-situated-eval "(clojure.tools.namespace.repl/refresh)"))

(define-key cider-mode-map (kbd "C-c #") 'my-rich-reload-code)


(require 'ox-json)
(use-package ox-json :pin "melpa")

  

(let ((jstr (with-current-buffer (get-buffer "20210522182900-we_thinkers.org")
              (org-export-as 'json))))
  (with-temp-file "~/tmp/note.json"
    (insert jstr))
  ;; (with-current-buffer (get-buffer-create "*demo*")
  ;;   (erase-buffer)
  ;;   (insert jstr)
  ;;   (pop-to-buffer (current-buffer)))
  )

(--map (seq-length it) ["\"" "\\\"" "\\\\\""])





(defun my-test (file)
  (interactive (list (read-file-name "file: ")))
  (message "%s" file))

(ido-everywhere -1)

(org-roam-db-sync t)
load-path

(defun lsp-install-server (update? &optional server-id)
  "Interactively install or re-install server.
When prefix UPDATE? is t force installation even if the server is present."
  (interactive "P")
  (lsp--require-packages)
  (let* ((chosen-client (or (gethash server-id lsp-clients)
                            (lsp--completing-read
                             "Select server to install/re-install: "
                             (or (->> lsp-clients
                                      (ht-values)
                                      (-filter (-andfn
                                                (-not #'lsp--client-download-in-progress?)
                                                #'lsp--client-download-server-fn)))
                                 (user-error "There are no servers with automatic installation"))
                             (lambda (client)
                               (let ((server-name (-> client lsp--client-server-id symbol-name)))
                                 (if (lsp--server-binary-present? client)
                                     (concat server-name " (Already installed)")
                                   server-name)))
                             nil
                             t)))
         (update? (or update?
                      (and (not (lsp--client-download-in-progress? chosen-client))
                           (lsp--server-binary-present? chosen-client)))))
    (lsp--install-server-internal chosen-client update?)))

(->> lsp-clients
     (ht-values)
     (-map #'lsp--client-server-id))
(lsp-install-server t 'clojure-lsp)

(defun lsp--install-server-internal (client &optional update?)
  (unless (lsp--client-download-server-fn client)
    (user-error "There is no automatic installation for `%s', you have to install it manually following lsp-mode's documentation."
                (lsp--client-server-id client)))

  (setf (lsp--client-download-in-progress? client) t)
  (add-to-list 'global-mode-string '(t (:eval (lsp--download-status))))
  (cl-flet ((done
             (success? &optional error-message)
             ;; run with idle timer to make sure the lsp command is executed in
             ;; the main thread, see #2739.
             (run-with-timer
              0.0
              nil
              (lambda ()
                (-let [(&lsp-cln 'server-id 'buffers) client]
                  (setf (lsp--client-download-in-progress? client) nil
                        (lsp--client-buffers client) nil)
                  (if success?
                      (lsp--info "Server %s downloaded, auto-starting in %s buffers." server-id
                                 (length buffers))
                    (lsp--error "Server %s install process failed with the following error message: %s.
Check `*lsp-install*' and `*lsp-log*' buffer."
                                server-id
                                error-message))
                  (seq-do
                   (lambda (buffer)
                     (when (lsp-buffer-live-p buffer)
                       (lsp-with-current-buffer buffer
                         (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                                    global-mode-string)
                         (when success? (lsp)))))
                   buffers)
                  (unless (lsp--filter-clients #'lsp--client-download-in-progress?)
                    (cl-callf2 -remove-item '(t (:eval (lsp--download-status)))
                               global-mode-string)))))))
    (lsp--info "Download %s started." (lsp--client-server-id client))
    (condition-case err
        (funcall
         (lsp--client-download-server-fn client)
         client
         (lambda () (done t))
         (lambda (msg) (done nil msg))
         update?)
      (error
       (done nil (error-message-string err))))))
(pcase '(1 2)
  (`(,one ,two) 12))
(pcase '(1 . 2)
  (`(,one . ,two) 12))
(pcase '(1 . 2)
  (`(,one . ,two) one))
(pcase 2
  ('2 4))
(pcase 2
  (2 4))
(pcase 2
  (1 2)
  (_ 4))
(pcase 2
  (1 2)
  ((pred evenp) 4))
(pcase 2
  (1 2)
  ((pred (equal 2)) 4))
(pcase 2
  ((pred (> 1)) 3)
  ((pred (< 1)) 4))


(insert (s-join "\n" (my-buffer-lines)))

(seq--into-string [?a ?b])

(defun replace-matches (regx func)
  (while (search-forward-regexp regx nil t)
    (let ((md (match-data)))
      (funcall func md))))

(nthcdr 2 '(1 2 3))

(replace-matches (rx "match-"
                     (group (+? anything))
                     ")")
                 (lambda (data)
                   (message "%s" data)
                   (let ((my-data (nthcdr 2 data)))
                     (goto-char (car my-data))
                     (kill-region (car my-data) (cadr my-data))
                     (insert "inserted"))))

;; https://github.com/Wilfred/ht.el
(let ((h (ht (3 4) (1 2))))
  (list (ht-get h 1)
        (ht-get h 2 5)))

(list-faces-display (rx "org"))
(custom-file)
(let ((s "hello"))
  (list (string-match
         (rx (seq string-start
                  "h"
                  (any "je")
                  (group (+ anything))))
         s)
        (match-data)
        (match-string 0 s)
        (match-string 1 s)))

(let ((s "abc"))
  (list (string-match
         (rx (seq string-start
                  "h"
                  (any "je")
                  (group anything)))
         s)
        (match-data)))

(-filter (lambda (b) (string-match-p (rx (seq
                                          string-start
                                          (* any)
                                          "magit"
                                          (* any)
                                          string-end))
                                     (buffer-name b)))
         (buffer-list))

(if 0 1 2)