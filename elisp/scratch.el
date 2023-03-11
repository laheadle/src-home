
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

(with-current-buffer (car (org-roam-buffer-list))
  (org-roam-node-at-point))

(defvar my-databases `(,org-roam-directory "~/doc/data/a-media-system"))
()
(defun my-element-context-property-names ()
  (interactive)
  (if-let* ((node (org-element-context))
            (type (car node))
            (properties (cadr node))
            (keys (-slice properties 0 nil 2)))
      (message "%s: %s" type keys)
    node))

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