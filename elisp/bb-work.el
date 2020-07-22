
(defun l-echo (&rest args) (interactive) (message "%s" args))

(l-echo 1 2)
(l-echo '(1 2))
(l-echo nil)
(l-echo)

;; (define-transient-command l-ava () "run ava" [("-a" "what to echo" "--arg")
;;                                               ("e" "echo me" l-echo)])


;; (defun l-call-ava (&optional args)
;;   (interactive
;;    (list (transient-args 'l-ava-transient)))
;;   (message "args %s" args))

(defun l-ava-so-show-matching (&optional args)
  (interactive
   (list (transient-args 'l-ava-study-objects)))
  (apply 'l-echo (cons "bb -m laheadle.ava.script.show-study-objects" args)))

(define-infix-argument l-ava-transient:--project ()
  :description "Project"
  :class 'transient-option
  :shortarg "-p"
  :argument "--project=")

(define-infix-argument l-ava-transient:--pattern ()
  :description "Pattern"
  :class 'transient-option
  :shortarg "-t"
  :argument "--pattern=")

(define-transient-command l-ava-study-objects ()
  "Ava Study Objects"
  ["Arguments"
   ("-a" "Show AIs" "--show-ais")
   (l-ava-transient:--pattern)
   (l-ava-transient:--project)]
  ["Actions"
   ("m" "Show matching" l-ava-so-show-matching)])

 (l-ava-study-objects)