;; prototype of [defunctionalized CPS form] -> [FCL] transforation.
(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 nice-9)
	     (ice-9 pretty-print))

(define (mk-dispatch-name n)
  (string->symbol (string-append "dispatch-" (number->string n))))

(define (mk-block-name n)
  (string->symbol (string-append "block-" (number->string n))))

(define (mk-new-block-name parent-name)
  (gensym (string-append (symbol->string parent-name) "-")))


;; for now just the compilation of the apply tree:
(define (applytree->blockmap tree)
  (let* ((blocks-to-do
	  (let loop ((tree tree))
	    (match tree
	      (() '())
	      (('IF ('EQ ('CAR 'label) n) block tree)
	       `((,(mk-block-name n) ,block)
		 . ,(loop tree)))
	      (otherwise `(error-skurwysyn-error ,tree))))) ;; !!
	 (dispatch-blocks
	  (mk-dispatches (iota (length blocks-to-do)))))
    (let loop ((pending blocks-to-do)
	       (blockmap dispatch-blocks))
      (match pending
	(() blockmap)
	(((name block) . pending)
	 ;;; thanks to the cps phrase, there are only 3 possibilities:
	 ;;; the block starts with either apply, an IF, or raw expression, which
	 ;;; correspond to "goto/if", "if" (spawn 2 new blocks), and "return" resp.
	 (match block
	   (('apply label args)
	    (loop pending `((,name
			    . ,(mk-apply-block block))
			    . ,blockmap)))
	   (('IF cond b1 b2)
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
	 (destination-name (mk-block-name block-num)))
  (if (null? rest-block-nums)
      `((,dispatch-name (GOTO ,destination-name)))
      (cons `(,dispatch-name (IF (EQ (CAR label) ,block-num)
				 ,destination-name
				 ,(mk-dispatch-name (car rest-block-nums))))
	    (mk-dispatches rest-block-nums)))))

(define (d0->fcl-expr expr) expr) ;;; TMP? cf ./to-fcl-symbols.sh
      
;;; hehe.
(define mk-apply-block
  (lambda ((_apply_ label args))
    `((LET new-label ,(d0->fcl-expr label))
      (LET new-args ,(d0->fcl-expr args))
      (LET args new-args)
      (LET label new-label)
      (GOTO dispatch-0))))
;(pretty-print (mk-apply-block '(apply (CONS 3 ()) (CONS (CAR args) ()))))
  

(define (mk-if-block cond true-block-name false-block-name)
  `((IF ,(d0->fcl-expr cond)
	,true-block-name
	,false-block-name)))

(define (mk-return-block expr)
  `((return ,(d0->fcl-expr expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; output from defun on output of cps... [apply only]
(define test1
'(LABEL apply
        (LAMBDA
          (label args)
          (IF (EQ (CAR label) 0)
              (apply (CAR (CDR (CDR args)))
                     (CONS (EQ (CAR args) (CAR (CDR args))) ()))
              (IF (EQ (CAR label) 1)
                  (apply (CAR (CDR args))
                         (CONS (CAR (CAR args)) ()))
                  (IF (EQ (CAR label) 2)
                      (apply (CAR (CDR args))
                             (CONS (CDR (CAR args)) ()))
                      (IF (EQ (CAR label) 3)
                          (apply (CAR (CDR (CDR args)))
                                 (CONS (CONS (CAR args) (CAR (CDR args))) ()))
                          (IF (EQ (CAR label) 4)
                              (apply (CONS 5
                                           (CONS (CAR (CDR (CDR args)))
                                                 (CONS (CAR (CDR args))
                                                       (CONS (CAR args) ()))))
                                     (CONS (CAR (CDR args)) ()))
                              (IF (EQ (CAR label) 5)
                                  (apply (CONS 6
                                               (CONS (CAR args)
                                                     (CONS (CAR (CDR label))
                                                           (CONS (CAR (CDR (CDR label)))
                                                                 (CONS (CAR (CDR (CDR (CDR label))))
                                                                       ())))))
                                         (CONS () ()))
                                  (IF (EQ (CAR label) 6)
                                      (apply EQ-k
                                             (CONS (CAR (CDR label))
                                                   (CONS (CAR args)
                                                         (CONS (CONS 7
                                                                     (CONS (CAR (CDR (CDR label)))
                                                                           (CONS (CAR (CDR (CDR (CDR label))))
                                                                                 (CONS (CAR (CDR (CDR (CDR (CDR label)))))
                                                                                       ()))))
                                                               ()))))
                                      (IF (EQ (CAR label) 7)
                                          (IF (CAR args)
                                              (apply (CAR (CDR label))
                                                     (CONS () ()))
                                              (apply (CONS 8
                                                           (CONS (CAR (CDR label))
                                                                 (CONS (CAR (CDR (CDR label)))
                                                                       (CONS (CAR (CDR (CDR (CDR label))))
                                                                             ()))))
                                                     (CONS (CAR (CDR (CDR (CDR label))))
                                                           ())))
                                          (IF (EQ (CAR label) 8)
                                              (apply (CONS 9
                                                           (CONS (CAR args)
                                                                 (CONS (CAR (CDR label))
                                                                       (CONS (CAR (CDR (CDR label)))
                                                                             (CONS (CAR (CDR (CDR (CDR label))))
                                                                                   ())))))
                                                     (CONS (CAR (CDR (CDR label)))
                                                           ()))
                                              (IF (EQ (CAR label) 9)
                                                  (apply CAR-k
                                                         (CONS (CAR args)
                                                               (CONS (CONS 10
                                                                           (CONS (CAR (CDR label))
                                                                                 (CONS (CAR (CDR (CDR label)))
                                                                                       (CONS (CAR (CDR (CDR (CDR label))))
                                                                                             (CONS (CAR (CDR (CDR (CDR (CDR label)))))
                                                                                                   ())))))
                                                                     ())))
                                                  (IF (EQ (CAR label) 10)
                                                      (apply (CAR (CDR label))
                                                             (CONS (CAR args)
                                                                   (CONS (CONS 11
                                                                               (CONS (CAR (CDR (CDR label)))
                                                                                     (CONS (CAR (CDR (CDR (CDR label))))
                                                                                           (CONS (CAR (CDR (CDR (CDR (CDR label)))))
                                                                                                 ()))))
                                                                         ())))
                                                      (IF (EQ (CAR label) 11)
                                                          (apply (CONS 12
                                                                       (CONS (CAR args)
                                                                             (CONS (CAR (CDR label))
                                                                                   (CONS (CAR (CDR (CDR label)))
                                                                                         (CONS (CAR (CDR (CDR (CDR label))))
                                                                                               ())))))
                                                                 (CONS map ()))
                                                          (IF (EQ (CAR label)
                                                                  12)
                                                              (apply (CONS 13
                                                                           (CONS (CAR args)
                                                                                 (CONS (CAR (CDR label))
                                                                                       (CONS (CAR (CDR (CDR label)))
                                                                                             (CONS (CAR (CDR (CDR (CDR label))))
                                                                                                   ())))))
                                                                     (CONS (CAR (CDR (CDR (CDR (CDR label)))))
                                                                           ()))
                                                              (IF (EQ (CAR label)
                                                                      13)
                                                                  (apply (CONS 14
                                                                               (CONS (CAR (CDR label))
                                                                                     (CONS (CAR args)
                                                                                           (CONS (CAR (CDR (CDR label)))
                                                                                                 (CONS (CAR (CDR (CDR (CDR label))))
                                                                                                       ())))))
                                                                         (CONS (CAR (CDR (CDR (CDR (CDR label)))))
                                                                               ()))
                                                                  (IF (EQ (CAR label)
                                                                          14)
                                                                      (apply CDR-k
                                                                             (CONS (CAR args)
                                                                                   (CONS (CONS 15
                                                                                               (CONS (CAR (CDR label))
                                                                                                     (CONS (CAR (CDR (CDR label)))
                                                                                                           (CONS (CAR (CDR (CDR (CDR label))))
                                                                                                                 (CONS (CAR (CDR (CDR (CDR (CDR label)))))
                                                                                                                       ())))))
                                                                                         ())))
                                                                      (IF (EQ (CAR label)
                                                                              15)
                                                                          (apply (CAR (CDR label))
                                                                                 (CONS (CAR (CDR (CDR label)))
                                                                                       (CONS (CAR args)
                                                                                             (CONS (CONS 16
                                                                                                         (CONS (CAR (CDR (CDR (CDR label))))
                                                                                                               (CONS (CAR (CDR (CDR (CDR (CDR label)))))
                                                                                                                     ())))
                                                                                                   ()))))
                                                                          (IF (EQ (CAR label)
                                                                                  16)
                                                                              (apply CONS-k
                                                                                     (CONS (CAR (CDR label))
                                                                                           (CONS (CAR args)
                                                                                                 (CONS (CAR (CDR (CDR label)))
                                                                                                       ()))))
                                                                              (IF (EQ (CAR label)
                                                                                      17)
                                                                                  (apply (CAR (CDR args))
                                                                                         (CONS (MUL (CAR args)
                                                                                                    (CAR args))
                                                                                               ()))
                                                                                  (IF (EQ (CAR label)
                                                                                          18)
                                                                                      (CAR args)
                                                                                      ()))))))))))))))))))))))

;;; the test:

(define test1-tree (caddr (caddr test1)))

(pretty-print (applytree->blockmap test1-tree))
;;; this is next-to-impossible, but it seems to work...
