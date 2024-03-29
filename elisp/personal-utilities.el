;; copy zoom url to clipboard
(fset 'copy-zoom-to-clipboard
      (kmacro-lambda-form [?\C-c ?n ?f ?z ?o ?o ?m return ?\C-1 ?s ?h ?t ?t ?p return ?\M-b ?\M-k] 0 "%d"))
(defalias 'a 'copy-zoom-to-clipboard)
;; org: copy headline and body
(fset 'ocp
      (kmacro-lambda-form [?\C-e ?\C-a ?\C-a ?\C-c ?\C-x ?\M-w ?\C-c ?\C-x ?\C-y ?\C-a] 0 "%d"))
(defalias 'my-org-copy-headline-and-body 'ocp)

;; copy buffer
(fset 'c
      (kmacro-lambda-form [?\C-  ?\M-< ?\C-  ?\M-> ?\M-w ?\C-u ?\C-  ?\C-u ?\C- ] 0 "%d"))



;; indent block forward
(fset 'ib
      (kmacro-lambda-form [?\C-e left ?\C-a ?\C-  ?\C-e left ?\C-\M-f ?\C-e ?\C-\M-\\] 0 "%d"))
(defalias 'my-indent-block-forward 'ib)

(fset 'do-go-vendor
      (kmacro-lambda-form [?\C-x ?\C-f return ?^ ?\M-< down down ?0 ?w ?\C-b ?* ?s ?h ?e ?l ?l ?* ?$ backspace return ?c ?d ?  ?\C-y return ?g ?o ?  ?g ?e ?t ?  ?. ?/ ?. ?. ?. ?  ?& ?& ?  ?g ?o ?  ?m ?o ?d ?  ?v ?e ?n ?d ?o ?r return] 0 "%d"))
(defalias 'gv 'do-go-vendor)


(fset 'j ;; my-indent-block
      (kmacro-lambda-form [?\C-a ?\C-e left ?\C-  ?\C-\M-f ?\C-\M-\\ ?\C-\M-b ?\C-a] 0 "%d"))

;; fire up bb
(fset 'start-bb
      (kmacro-lambda-form [?\C-c ?n ?f ?g ?e ?n ?e ?r ?a ?t ?i ?o ?n ?  ?c ?s ?v ?\C-m ?\C-c ?\C-a ?o ?c ?s ?v ?\C-m ?\M-< right right right ?\C-  ?\C-e ?\M-w ?\M-x ?c ?r ?  ?s ?h ?e ?\C-m ?b ?b ?\C-m ?\C-x ?q ?\C-y ?\C-m ?\C-x ?q ?\C-b ?c ?s ?v ?. ?c ?l ?j ?\C-m] 0 "%d"))


;; evaluate in comment
(fset 'eic
      (kmacro-lambda-form [?\[ ?A ?d ?f ?\C-x ?\C-s ?\C-c ?\C-v ?f ?e] 0 "%d"))

;; cd in shell to the current directory of the current buffer
(fset 'cc
      (kmacro-lambda-form [?\C-x ?\C-f return ?\M-< ?0 ?w ?\C-b ?* ?s ?h ?e ?l ?l ?* return ?c ?d ?  ?\C-y return] 0 "%d"))

(fset 'my-org-copy-text-content
   (kmacro-lambda-form [?\C-c ?\C-b ?\C-e right ?\C-  ?\C-c ?\C-f left ?\M-w] 0 "%d"))
(defalias 'ct 'my-org-copy-text-content)

(fset 'jppb
   (kmacro-lambda-form [?\M-< ?\C-  ?\M-> ?\M-x ?j ?p ?p ?$ return] 0 "%d"))

(defalias 'my-pretty-print-buffer-json 'jppb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert the next one right above this
;; C-x C-k n
;; see: insert-kbd-macro

