;;; fast and dirty prototype of total defunctionalizator.

(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 nice-9)
	     (ice-9 pretty-print))

(define (mk-label n) (if (number? n) `',(string->symbol (string-append "L" (number->string n))) n))

(define (gather-all-lambdas-in an-expression)
  (match an-expression
    (() '())
    (('quote _) '())
    ((? number?) '())
    ((? symbol?) '())
    (('read) '())
    (('= e1 e2) (append (gather-all-lambdas-in e1)
			 (gather-all-lambdas-in e2)))
    (('< e1 e2) (append (gather-all-lambdas-in e1)
			 (gather-all-lambdas-in e2)))
    (('atom? e1) (gather-all-lambdas-in e1))
    (('number? e1) (gather-all-lambdas-in e1))
    (('+ e1 e2) (append (gather-all-lambdas-in e1)
			  (gather-all-lambdas-in e2)))
    (('- e1 e2) (append (gather-all-lambdas-in e1)
			  (gather-all-lambdas-in e2)))
    (('* e1 e2) (append (gather-all-lambdas-in e1)
			  (gather-all-lambdas-in e2)))
    (('cons e1 e2) (append (gather-all-lambdas-in e1)
			   (gather-all-lambdas-in e2)))
    (('car e1) (gather-all-lambdas-in e1))
    (('cdr e1) (gather-all-lambdas-in e1))
    (('if e1 e2 e3) (append (gather-all-lambdas-in e1)
			    (gather-all-lambdas-in e2)
			    (gather-all-lambdas-in e3)))
    (('^ vars body) `((,vars ,body) . ,(gather-all-lambdas-in body)))
    ((f . args) (append-map gather-all-lambdas-in an-expression))

    (otherwise `(ERROR: illformed expression ,an-expression))))
;(map pretty-print (gather-all-lambdas-in '(LAMBDA (x y) (IF (EQ 2 3) ((LAMBDA (z) (MUL x z)) 997) (LAMBDA (z) (MUL y z))))))

(define (gather-all-lambdas-of program)
  (match program    
    ((('! v expr) . program0)
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
       (('quote _) '())
       ((? number?) '())
       ((? symbol? s) `(,s))
       (('read) '())
       (('= e1 e2) (append (loop e1)
			    (loop e2)))
       (('< e1 e2) (append (loop e1)
			    (loop e2)))
       (('atom? e1) (loop e1))
       (('number? e1) (loop e1))
       (('+ e1 e2) (append (loop e1)
			     (loop e2)))
       (('- e1 e2) (append (loop e1)
			     (loop e2)))
       (('* e1 e2) (append (loop e1)
			     (loop e2)))
       (('cons e1 e2) (append (loop e1)
			      (loop e2)))
       (('car e1) (loop e1))
       (('cdr e1) (loop e1))
       (('if e1 e2 e3) (append (loop e1)
			       (loop e2)
			       (loop e3)))
       (('^ vars body) (lset-difference eq? (loop body) vars))
       ((f . args) (append-map loop an-expression)) ;; ?!
       
       (otherwise `(ERROR: illformed expression ,an-expression))))))
;(free-vars-of '(CONS (MUL x 4) (IF (EQ a b) (QUOTE elo) ((LAMBDA (a1 a2) (f a1 a2)) x y))))

;; ah yes, the names [variables] bound with LABEL:
(define (gather-labeled-names program)
  (match program    
    ((('! v _) . program0)
     `(,v . ,(gather-labeled-names program0)))
    ((expr . program0)
     (gather-labeled-names program0))
    (() '())
    (otherwise `(ERROR: illformed program ,program))))
  
;; now build representations for lambdas:
(define (mk-lambdas-to-labels-mapping lambdas labeled-names)
  (map (lambda ((args body) num)
	 (cons (mk-label num)
	       (lset-difference eq?
				(free-vars-of body)
				(append args labeled-names))))		     
       lambdas
       (iota (length lambdas))))

;; others have backquotes, we have explicit CONSes ;)
(define (mk-list-constructor xs) (fold-right (lambda (h t) `(cons ,h ,t)) '() xs))

(define (replace-lambdas-with-labels an-expression all-lambdas lambdas-to-labels)
  (let loop ((an-expression an-expression))
     (match an-expression
       (() '())
       (('quote _) an-expression)
       ((? number?) an-expression)
       ((? symbol?) an-expression)
       (('read) an-expression)
       (('= e1 e2) `(= ,(loop e1)
		       ,(loop e2)))
       (('< e1 e2) `(< ,(loop e1)
			,(loop e2)))
       (('atom? e1) `(atom? ,(loop e1)))
       (('number? e1) `(number? ,(loop e1)))
       (('+ e1 e2) `(+ ,(loop e1)
		       ,(loop e2)))
       (('- e1 e2) `(- ,(loop e1)
		       ,(loop e2)))
       (('* e1 e2) `(* ,(loop e1)
		       ,(loop e2)))
       (('cons e1 e2) `(cons ,(loop e1)
			     ,(loop e2)))
       (('car e1) `(car ,(loop e1)))
       (('cdr e1) `(cdr ,(loop e1)))
       (('if e1 e2 e3) `(if ,(loop e1)
			    ,(loop e2)
			    ,(loop e3)))
       (('^ vars body)
	(let* ((label (list-ref lambdas-to-labels
				(list-index (lambda (x)
					      (equal? x `(,vars ,body)))
					    all-lambdas)))
	       (constructor (mk-list-constructor label)))
	  constructor))
       ((f . args) `(apply ,(mk-label (loop f))
			   ,(mk-list-constructor (map loop args)))) ;; ?!  
       (otherwise `(ERROR: illformed expression ,an-expression)))))

;; THE THING:
(define (build-apply all-lambdas lambdas-labels-mapping)
  `(! apply
      (^ (label args)
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
		       `((= (car label) ,(mk-label id))
			 ,(finalize-apply-branch args fvv body)))
		     all-lambdas
		     lambdas-labels-mapping)))
    (fold-right (lambda ((p e) rest) `(if ,p ,e ,rest)) '() conds))) ;; :D
  
(define (assoc_ key alist)  
  (let ((val (assoc key alist)))
    (if val
	(cdr val)
	key #;val))) ;; KEY instead of #f, as it might be somehting defined in topenv.

;; this one we'll use to convert bound variables ("arguments") inside apply.
(define (mk-vars-to-positions-mapping varlist source)
  (let loop ((varlist varlist)
	     (deepest source))
    (if (null? varlist)
	'()
	`((,(car varlist) . (car ,deepest))
	  . ,(loop (cdr varlist) `(cdr ,deepest))))))
;(mk-vars-to-positions-mapping '(a b c) 'args)

;;; "map on list constructors":
(define (on-all-elems-of-CONS conslist f)
  (if (null? conslist)
      '()
      `(cons ,(f (cadr conslist))
	     ,(on-all-elems-of-CONS (caddr conslist) f))))      
;(on-all-elems-of-CONS '(CONS 1 (CONS 2 (CONS 3 ()))) (lambda (x) (* x x)))

;; now just replace variables with their CAR/CDR addresses in args/fvv:
(define (finalize-apply-branch args fvv body)
  (let* ((vars-mapping
	  (append
	   (mk-vars-to-positions-mapping args 'args)
	   (mk-vars-to-positions-mapping fvv '(cdr label)))))
    (let loop ((expr body))
      (match expr
	((? symbol? s) (assoc_ s vars-mapping))
	(('read) expr)
	(('= e1 e2) `(= ,(loop e1)
			,(loop e2)))
	(('< e1 e2) `(< ,(loop e1)
			,(loop e2)))
	(('+ e1 e2) `(+ ,(loop e1)
			,(loop e2)))
	(('- e1 e2) `(- ,(loop e1)
			,(loop e2)))
	(('* e1 e2) `(* ,(loop e1)
			,(loop e2)))
	(('cons e1 e2) `(cons ,(loop e1)
			      ,(loop e2)))
	(('car e1) `(car ,(loop e1)))
	(('cdr e1) `(cdr ,(loop e1)))
	(('if e1 e2 e3) `(if ,(loop e1)
			     ,(loop e2)
			     ,(loop e3)))
	(('apply lbl args)
	 (let ((lbl (if (symbol? lbl)
			(if (loop lbl)
			    (loop lbl)
			    lbl)
			(on-all-elems-of-CONS lbl loop)))
	       (args (on-all-elems-of-CONS args loop)))
	   `(apply #;,lbl ,(mk-label lbl) ,args)))
	;;; something else?
	(otherwise otherwise)
       ))))
       
;;; heart of it all
(define (d17n program)
  (let* ((all-lambdas (gather-all-lambdas-of program))
	 (l2l (mk-lambdas-to-labels-mapping all-lambdas (gather-labeled-names program)))
	 (the-replacer (lambda (x)
			 (match x
			   (('! v expr)
			    `(! ,v ,(replace-lambdas-with-labels expr all-lambdas l2l)))
			   (something
			    (replace-lambdas-with-labels something all-lambdas l2l)))))
	 (defs (drop-right program 1))
	 (last-def (last program))
	 (last-def-var (cadr last-def))
	 (last-def-args (cadr (caddr last-def)))
	 (last-def-body (caddr (caddr last-def)))
	 (new-program (append (map the-replacer defs)
			      `((! ,last-def-var (^ ,last-def-args ,(the-replacer last-def-body))))))
	 (apply (build-apply all-lambdas l2l)))
    (cons apply new-program)))

;;; "main":
(pretty-print (d17n (read)))

