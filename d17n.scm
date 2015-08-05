;;; fast and dirty prototype of total defunctionalizator;
;; in a way it should compile micro higher order lisp with lexical scoping into micro 1st order lisp...

;; todo: rewrite it "as asap as possible"

(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 nice-9)
	     (ice-9 pretty-print))


(define (gather-all-lambdas-in an-expression)
  (match an-expression
    (() '())
    (('QUOTE _) '())
    ((? number?) '())
    ((? symbol?) '())
    (('EQ e1 e2) (append (gather-all-lambdas-in e1)
			 (gather-all-lambdas-in e2)))
    (('ADD e1 e2) (append (gather-all-lambdas-in e1)
			  (gather-all-lambdas-in e2)))
    (('MUL e1 e2) (append (gather-all-lambdas-in e1)
			  (gather-all-lambdas-in e2)))
    (('CONS e1 e2) (append (gather-all-lambdas-in e1)
			   (gather-all-lambdas-in e2)))
    (('CAR e1) (gather-all-lambdas-in e1))
    (('CDR e1) (gather-all-lambdas-in e1))
    (('IF e1 e2 e3) (append (gather-all-lambdas-in e1)
			    (gather-all-lambdas-in e2)
			    (gather-all-lambdas-in e3)))
    (('LAMBDA vars body) `((,vars ,body) . ,(gather-all-lambdas-in body)))
    ((f . args) (append-map gather-all-lambdas-in an-expression))

    (otherwise `(ERROR: illformed expression ,an-expression))))
;(map pretty-print (gather-all-lambdas-in '(LAMBDA (x y) (IF (EQ 2 3) ((LAMBDA (z) (MUL x z)) 997) (LAMBDA (z) (MUL y z))))))

(define (gather-all-lambdas-of program)
  (match program    
    ((('LABEL v expr) . program0)
     (append (gather-all-lambdas-in expr)
	     (gather-all-lambdas-of program0)))
    ((expr . program0)
     (append (gather-all-lambdas-in expr)
	     (gather-all-lambdas-of program0)))
    (() '())
    (otherwise `(ERROR: illformed program ,program))))
;(map (lambda (x) (pretty-print x) (newline) 23) (gather-all-lambdas-of ex1))

;;; free vars of any expression; if applied to body of lambda it should exclude vars bound by it.
;;; also if applied to the program, it should exclude variables bound with LABELs.
(define (free-vars-of an-expression)
  (delete-duplicates
   (let loop ((an-expression an-expression))
     (match an-expression
       (() '())
       (('QUOTE _) '())
       ((? number?) '())
       ((? symbol? s) `(,s))
       (('EQ e1 e2) (append (loop e1)
			    (loop e2)))
       (('ADD e1 e2) (append (loop e1)
			     (loop e2)))
       (('MUL e1 e2) (append (loop e1)
			     (loop e2)))
       (('CONS e1 e2) (append (loop e1)
			      (loop e2)))
       (('CAR e1) (loop e1))
       (('CDR e1) (loop e1))
       (('IF e1 e2 e3) (append (loop e1)
			       (loop e2)
			       (loop e3)))
       (('LAMBDA vars body) (lset-difference eq? (loop body) vars))
       ((f . args) (append-map loop an-expression)) ;; ?!
       
       (otherwise `(ERROR: illformed expression ,an-expression))))))
;(free-vars-of '(CONS (MUL x 4) (IF (EQ a b) (QUOTE elo) ((LAMBDA (a1 a2) (f a1 a2)) x y))))

;; ah yes, the names [variables] bound with LABEL:
(define (gather-labeled-names program)
  (match program    
    ((('LABEL v _) . program0)
     `(,v . ,(gather-labeled-names program0)))
    ((expr . program0)
     (gather-labeled-names program0))
    (() '())
    (otherwise `(ERROR: illformed program ,program))))
  
;; now build representations for lambdas:
(define (mk-lambdas-to-labels-mapping lambdas labeled-names)
  (map (lambda ((args body) num)
	 (cons num
	       (lset-difference eq?
				(free-vars-of body)
				(append args labeled-names))))		     
       lambdas
       (iota (length lambdas))))

;; others have backquotes, we have explicit CONSes ;)
(define (mk-list-constructor xs) (fold-right (lambda (h t) `(CONS ,h ,t)) '() xs))

(define (replace-lambdas-with-labels an-expression all-lambdas lambdas-to-labels)
  (let loop ((an-expression an-expression))
     (match an-expression
       (() '())
       (('QUOTE _) an-expression)
       ((? number?) an-expression)
       ((? symbol?) an-expression)
       (('EQ e1 e2) `(EQ ,(loop e1)
			 ,(loop e2)))
       (('ADD e1 e2) `(ADD ,(loop e1)
			   ,(loop e2)))
       (('MUL e1 e2) `(MUL ,(loop e1)
			   ,(loop e2)))
       (('CONS e1 e2) `(CONS ,(loop e1)
			     ,(loop e2)))
       (('CAR e1) `(CAR ,(loop e1)))
       (('CDR e1) `(CDR ,(loop e1)))
       (('IF e1 e2 e3) `(IF ,(loop e1)
			    ,(loop e2)
			    ,(loop e3)))
       (('LAMBDA vars body)
	(let* ((label (list-ref lambdas-to-labels
				(list-index (lambda (x)
					      (equal? x `(,vars ,body)))
					    all-lambdas)))
	       (constructor (mk-list-constructor label)))
	  constructor))
       ((f . args) `(apply ,(loop f)
			   ,(mk-list-constructor (map loop args)))) ;; ?!  
       (otherwise `(ERROR: illformed expression ,an-expression)))))

;; THE THING:
(define (build-apply all-lambdas lambdas-labels-mapping)
  `(LABEL apply
	  (LAMBDA (label args)
		  ,(build-conds-for all-lambdas lambdas-labels-mapping))))

(define (build-conds-for all-lambdas lambdas-labels-mapping)
  (let* ((lambda-replacer
	  (lambda ((args body))
	    `(,args ,(replace-lambdas-with-labels body
						  all-lambdas
						  lambdas-labels-mapping))))
	 (all-lambdas (map lambda-replacer all-lambdas))
	 (conds (map (lambda ((args body)
			      (id . fvv))
		       `((EQ (CAR label) ,id)
			 ,(finalize-apply-branch args fvv body)))
		     all-lambdas
		     lambdas-labels-mapping)))
    (fold-right (lambda ((p e) rest) `(IF ,p ,e ,rest)) '() conds))) ;; :D
  
(define (assoc_ key alist)
  (let ((val (assoc key alist)))
    (if val
	(cdr val)
	val)))

;; this one we'll use to convert bound variables ("arguments") inside apply.
(define (mk-vars-to-positions-mapping varlist source)
  (let loop ((varlist varlist)
	     (deepest source))
    (if (null? varlist)
	'()
	`((,(car varlist) . (CAR ,deepest))
	  . ,(loop (cdr varlist) `(CDR ,deepest))))))
;(mk-vars-to-positions-mapping '(a b c) 'args)

;;; "map on list constructors":
(define (on-all-elems-of-CONS conslist f)
  (if (null? conslist)
      '()
      `(CONS ,(f (cadr conslist))
	     ,(on-all-elems-of-CONS (caddr conslist) f))))      
;(on-all-elems-of-CONS '(CONS 1 (CONS 2 (CONS 3 ()))) (lambda (x) (* x x)))

;; now just replace variables with their CAR/CDR addresses in args/fvv:
(define (finalize-apply-branch args fvv body)
  (let* ((vars-mapping
	  (append
	   (mk-vars-to-positions-mapping args 'args)
	   (mk-vars-to-positions-mapping fvv '(CDR label)))))
    (let loop ((expr body))
      (match expr
	((? symbol? s) (assoc_ s vars-mapping))
	(('EQ e1 e2) `(EQ ,(loop e1)
			  ,(loop e2)))
	(('ADD e1 e2) `(ADD ,(loop e1)
			    ,(loop e2)))
	(('MUL e1 e2) `(MUL ,(loop e1)
			    ,(loop e2)))
	(('CONS e1 e2) `(CONS ,(loop e1)
			      ,(loop e2)))
	(('CAR e1) `(CAR ,(loop e1)))
	(('CDR e1) `(CDR ,(loop e1)))
	(('IF e1 e2 e3) `(IF ,(loop e1)
			     ,(loop e2)
			     ,(loop e3)))
	(('apply lbl args)
	 (let ((lbl (if (symbol? lbl)
			(if (loop lbl)
			    (loop lbl)
			    lbl)
			(on-all-elems-of-CONS lbl loop)))
	       (args (on-all-elems-of-CONS args loop)))
	   `(apply ,lbl ,args)))
	;;; something else?
	(otherwise otherwise)
       ))))
       
;;; heart of it all
(define (d17n program)
  (let* ((all-lambdas (gather-all-lambdas-of program))
	 (l2l (mk-lambdas-to-labels-mapping all-lambdas (gather-labeled-names program)))
	 (the-replacer (lambda (x)
			 (match x
			   (('LABEL v expr)
			    `(LABEL ,v ,(replace-lambdas-with-labels expr all-lambdas l2l)))
			   (something (replace-lambdas-with-labels something all-lambdas l2l)))))
	 (new-program (map the-replacer program))
	 (apply (build-apply all-lambdas l2l)))
    (cons apply new-program)))


;;; "main":
(pretty-print (d17n (read)))

