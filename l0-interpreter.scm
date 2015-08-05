;; an interpreter micro 1st order lisp that we compile micro higher order lisp into.

(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 nice-9)
	     (ice-9 pretty-print))

(define (lookup sym env)
  (let ((val (assoc sym env)))
    (if val (cdr val)'())))

(define (pair xs ys) (map cons xs ys))

(define (l0-eval expr env)
  (match expr
    (() '())
    (('QUOTE e) e)
    ((? number? n) n)
    ((? symbol? s) (lookup s env))
    (('EQ e1 e2) (if (eqv? (l0-eval e1 env)
			   (l0-eval e2 env))
		     42 ;; stands for #t, you know.
		     '()))
    (('ADD e1 e2) (+ (l0-eval e1 env)
		     (l0-eval e2 env)))
    (('MUL e1 e2) (* (l0-eval e1 env)
		     (l0-eval e2 env)))
    (('CONS e1 e2) (cons (l0-eval e1 env)
			 (l0-eval e2 env)))
    (('CAR e1) (car (l0-eval e1 env)))
    (('CDR e1) (cdr (l0-eval e1 env)))
    (('IF e1 e2 e3) (if (eq? (l0-eval e1 env) '())
			(l0-eval e3 env)
			(l0-eval e2 env)))
    (('LAMBDA vs e1) `(LAMBDA ,vs ,e1))
    ((f . args)
     (l0-apply (l0-eval f env)
	       (map (lambda (e) (l0-eval e env)) args)
	       env))))

(define (l0-apply (_ as body) xs env)
  (l0-eval body (append (pair as xs) env)))

(define (run-prog prg)
  (let loop ((prg prg) (env '()))
    (if (null? prg)
	(pretty-print '(boom done))
	(match (car prg)
	  (('LABEL v expr)
	   (begin
	     (pretty-print `(extended env with ,v))
	     (loop (cdr prg)
		   `((,v . ,(l0-eval expr env)) . ,env))))
	  (other
	   (begin
	     (pretty-print (l0-eval other env))
	     (loop (cdr prg)
		   env)))))))

;;;; "main"
(run-prog (read))