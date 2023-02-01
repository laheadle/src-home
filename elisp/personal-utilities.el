;; copy zoom url to clipboard
(fset 'copy-zoom-to-clipboard
      (kmacro-lambda-form [?\C-c ?n ?f ?z ?o ?o ?m return ?\C-1 ?s ?h ?t ?t ?p return ?\M-b ?\M-k] 0 "%d"))
(defalias 'a 'copy-zoom-to-clipboard)
;; org: copy headline and body
(fset 'my-copy-headline-and-body
      (kmacro-lambda-form [?\C-e ?\C-a ?\C-a ?\C-c ?\C-x ?\M-w ?\C-c ?\C-x ?\C-y ?\C-a] 0 "%d"))
(defalias 'ocp 'my-org-copy-headline-and-body)

;; copy buffer
(fset 'c
      (kmacro-lambda-form [?\C-  ?\M-< ?\C-  ?\M-> ?\M-w ?\C-u ?\C-  ?\C-u ?\C- ] 0 "%d"))



;; indent block forward
(fset 'my-indent-block-forward
      (kmacro-lambda-form [?\C-e left ?\C-a ?\C-  ?\C-e left ?\C-\M-f ?\C-e ?\C-\M-\\] 0 "%d"))
(defalias 'e 'my-indent-block-forward)

(fset 'do-go-vendor
      (kmacro-lambda-form [?\C-x ?\C-f return ?^ ?\M-< down down ?0 ?w ?\C-b ?* ?s ?h ?e ?l ?l ?* ?$ backspace return ?c ?d ?  ?\C-y return ?g ?o ?  ?g ?e ?t ?  ?. ?/ ?. ?. ?. ?  ?& ?& ?  ?g ?o ?  ?m ?o ?d ?  ?v ?e ?n ?d ?o ?r return] 0 "%d"))
(defalias 'gv 'do-go-vendor)


(fset 'j ;; my-indent-block
      (kmacro-lambda-form [?\C-a ?\C-e left ?\C-  ?\C-\M-f ?\C-\M-\\ ?\C-\M-b ?\C-a] 0 "%d"))



;; fire up bb
(fset 'start-bb
      (kmacro-lambda-form [?\C-c ?n ?f ?g ?e ?n ?e ?r ?a ?t ?i ?o ?n ?  ?c ?s ?v return ?\C-c ?\C-a ?f ?\M-< ?\C-1 ?s ?c ?s ?v return return ?\M-< right right right ?\C-  ?\C-e ?\M-w ?\M-x ?c ?r ?  ?s ?h ?e return ?\C-y return ?\C-y return ?\C-x ?o  ?\M-x ?s ?h ?e ?l ?l ?- ?c ?o ?m ?m ?a ?n ?d return ?s ?l ?e ?e ?p ? ?2 return ?\C-c ?\C-x ?c ?j ?1 ?6 backspace backspace ?l ?o ?c ?a ?l ?h return ?1 ?6 ?6 ?7 return ?\M-x ?s ?h ?e ?l ?l ?- ?c ?o ?m ?m ?a ?n ?d return ?s ?l ?e ?e ?p ?  ?2 return ?\C-x ?o ?\C-c ?\C-k] 0 "%d"))

;; evaluate in comment
(fset 'eic
      (kmacro-lambda-form [?\[ ?A ?d ?f ?\C-x ?\C-s ?\C-c ?\C-v ?f ?e] 0 "%d"))

;; cd in shell to the current directory of the current buffer
(fset 'cc
      (kmacro-lambda-form [?\C-x ?\C-f return ?\M-< ?0 ?w ?\C-b ?* ?s ?h ?e ?l ?l ?* return ?c ?d ?  ?\C-y return] 0 "%d"))

(fset 'my-org-copy-text-content
   (kmacro-lambda-form [?\C-c ?\C-b ?\C-e right ?\C-  ?\C-c ?\C-f left ?\M-w] 0 "%d"))
(defalias 'ct 'my-org-copy-text-content)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert the next one right above this
;; C-x C-k n
;; see: insert-kbd-macro

