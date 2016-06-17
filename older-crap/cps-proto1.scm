(use-modules (ice-9 match)
	     (ice-9 pretty-print)	     
	     (srfi srfi-1))

;;; zaiste chujowy, ale jakże pięknie prosty CPSator:

(define (primop? sym)
  (member sym '(CONS CAR CDR ADD SUB MUL EQ LT NUM ATOM)))

;;; LOL.
(define (mk-primop-cps sym)
  (string->symbol (string-append (symbol->string sym) "-k")))

(define (mk-result-name) (gensym "res"))
(define (mk-continuation-name) (gensym "k"))

(define (trivial? expr)
  (or (null? expr)
      (number? expr)
      (symbol? expr)      
      (and (pair? expr) (eq? 'QUOTE (car expr)))))


(define (cps expr k)
  (match expr
    ((? trivial?)
     `(,k ,expr))
    (((? primop? p) . args)
     (let* ((names (map (lambda (x) (mk-result-name)) args))
	    (exprs args)
	    (fin (append (list (mk-primop-cps p)) names (list k))))
       (mk-cps-chain names exprs fin)))
    (('LAMBDA args body)
     (let ((cont (mk-continuation-name)))
       `(,k (LAMBDA ,(append args (list cont))
		    ,(cps body cont)))))
    (('IF e1 e2 e3)     
     (cps e1 `(LAMBDA (v1) (IF v1 ,(cps e2 k) ,(cps e3 k)))))
    ((f . args)
     (let* ((names (map (lambda (x) (mk-result-name)) expr))
	    (exprs expr)
	    (fin (append names (list k))))
       (mk-cps-chain names exprs fin)))))

(define (mk-cps-chain names exprs fin)
  (if (null? names)
      fin
      (cps (car exprs)
	   `(LAMBDA (,(car names))
		    ,(mk-cps-chain (cdr names) (cdr exprs) fin)))))


#;(begin 
  (pretty-print (cps '(LAMBDA (x) (IF (LT x 0) (MUL x -1) x)) 'K)) ;; ok
  (pretty-print (cps '(LAMBDA (x y) (SUB x y)) 'K))
  (pretty-print (cps '(LAMBDA (x) (IF (EQ x 0) 1 (MUL x (f (SUB x 1))))) 'K)) ) ;; boom done

(pretty-print (cps (read) 'K)) ;;; twój stary K.

