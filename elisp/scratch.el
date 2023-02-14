(--filter (string-match-p "use-package" it)
          load-path)



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