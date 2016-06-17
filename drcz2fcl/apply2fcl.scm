;; prototype of (defunctionalized, CPS form of drcz2 -> FCL)-transforation.
(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 nice-9)
	     (ice-9 pretty-print))

(define (mk-dispatch-name n)
  (string->symbol (string-append "dispatch-" (#;symbol->string number->string n))))

(define (mk-block-name n)
  (string->symbol (string-append "block-" (if (number? n) (number->string n) (symbol->string (cadr n))))))

(define (mk-new-block-name parent-name)
  (gensym (string-append (symbol->string parent-name) "-")))

(define (mk-label n) (if (number? n) `',(string->symbol (string-append "L" (number->string n))) n))

;; for now just the compilation of the apply tree:
(define (applytree->blockmap tree)
  (let* ((blocks-to-do
	  (let loop ((tree tree))
	    (match tree
	      (() '())
	      (('if ('= ('car 'label) n) block tree)
	       `((,(mk-block-name n) ,block)
		 . ,(loop tree)))
	      (otherwise `(error-skurwysyn-error ,tree))))) ;; !!
	 (dispatch-blocks
	  (reverse (mk-dispatches (iota (length blocks-to-do))))))
    (let loop ((pending blocks-to-do)
	       (blockmap dispatch-blocks))
      (match pending
	(() (reverse blockmap))
	(((name block) . pending)
	 ;;; thanks to the cps phrase, there are only 3 possibilities:
	 ;;; the block starts with either apply, an IF, or raw expression, which
	 ;;; correspond to "goto/if", "if" (spawn 2 new blocks), and "return" resp.
	 (match block
	   (('apply label args)
	    (loop pending `((,name
			    . ,(mk-apply-block block))
			    . ,blockmap)))
	   (('if cond b1 b2)
	    (let ((true-block-name (mk-new-block-name name))
		  (false-block-name (mk-new-block-name name)))
	      (loop `((,true-block-name ,b1)
		      (,false-block-name ,b2)
		      . ,pending)
		    `((,name
		       . ,(mk-if-block cond
				       true-block-name
				       false-block-name))
		      . ,blockmap))))
	   (expression
	    (loop pending `((,name
			     . ,(mk-return-block expression))
			    . ,blockmap)))))
	(otherwise `(error w petli ,pending))))))


;;; (mk-dispatches '(0 1 2)) =>
; ((dispatch-0 (if (eq (car label) 0) block-0 dispatch-1))
; (dispatch-1 (if (eq (car label) 1) block-1 dispatch-2))
; (dispatch-2 (goto block-2)))
(define (mk-dispatches block-nums)
  (let* ((block-num (car block-nums))
	 (rest-block-nums (cdr block-nums))
	 (dispatch-name (mk-dispatch-name block-num))
	 (destination-name (mk-block-name (mk-label block-num))))
  (if (null? rest-block-nums)
      `((,dispatch-name (goto ,destination-name)))
      (cons `(,dispatch-name (if (= (car label) ,(mk-label block-num))
				 ,destination-name
				 ,(mk-dispatch-name (car rest-block-nums))))
	    (mk-dispatches rest-block-nums)))))

(define (d0->fcl-expr expr) expr) ;;; TMP? cf ./to-fcl-symbols.sh
      
;;; hehe.
(define mk-apply-block
  (lambda ((_apply_ label args))
    `((let new-label ,(d0->fcl-expr label))
      (let new-args ,(d0->fcl-expr args))
      (let args new-args)
      (let label new-label)
      (goto dispatch-0))))
;(pretty-print (mk-apply-block '(apply (CONS 3 ()) (CONS (CAR args) ()))))
  

(define (mk-if-block cond true-block-name false-block-name)
  `((if ,(d0->fcl-expr cond)
	,true-block-name
	,false-block-name)))

(define (mk-return-block expr)
  `((return ,(d0->fcl-expr expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (d17n->blockmap program)
  (let* ((apply-def (car program))
	 (apply-tree (caddr (caddr apply-def)))
	 (apply-blockmap (applytree->blockmap apply-tree))
	 (trivial-defs (cdr program))
	 (last-def (last program))
	 (last-def-name (cadr last-def))
	 (last-def-args (cadr (caddr last-def)))
	 (init-block (mk-main trivial-defs)))
    `(,last-def-args
      ,last-def-name
      [,last-def-name . ,init-block] . ,apply-blockmap)))


(define (mk-main defs)
  (match defs
    [(('apply label args))
     `((let label ,label) (let args ,args) (goto dispatch-0))]
    [(('! name (^ vars ('apply label args)))) ;; nb: last one, no tail!
     `((let label ,label) (let args ,args) (goto dispatch-0))]
    [(('! v e) . defs) (cons `(let ,v ,e) (mk-main defs))]))

;;; todo: read na te jebane n,m cosie wieeesz, nazwanie inputów.	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; output from defun on output of cps... [apply only]

(pretty-print (d17n->blockmap (read)))

;;; the test:
#;(let* ((program (read))
       (apply-def (car program))
       (tree (caddr (caddr apply-def))))
  (pretty-print (applytree->blockmap tree)))

;(define test1-tree (caddr (caddr test1)))
;(pretty-print (applytree->blockmap test1-tree))
;;; this is next-to-impossible, but it seems to work...
