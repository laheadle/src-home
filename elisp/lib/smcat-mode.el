
(define-derived-mode smcat-mode prog-mode "State Machine Cat Mode" "")
(add-to-list 'auto-mode-alist '("\\.smcat$" . smcat-mode))