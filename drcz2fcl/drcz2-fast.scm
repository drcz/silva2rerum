(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 pretty-print))

(define (lookup k al) (let ((val (assoc k al))) (if val (cdr val) #f)))
;(lookup 'w '((q . 2) (w . 3) (e . 23)))
(define (update k v al) `((,k . ,v) . ,(delete k al)))
;(lookup 'w (update 'w 7 '((q . 2) (w . 3) (e . 23))))
(define (delete k al) (alist-delete k al equal?))
;(delete 'w '((q . 2) (w . 3) (e . 23)))

(define (bool->T/nil b) (if b 'T '()))
(define (atom? e) (not (pair? e)))
(define (primop? sym) (member sym '(car cdr cons + - * = < atom? number?)))

;;; 1. drcz2
(define (Eval expr env topenv)
  (let E ((expr expr))
    (match expr
      [() '()]
      ['T 'T]
      [(? number? n) n]
      [('closure args body bindings) expr]
      [(? symbol? v) (if (primop? v) v (lookup v (append env topenv)))]
      [('quote e) e]
      [('if e0 e1 e2)
       (if (null? (E e0))
	   (E e2)
	   (E e1))]
      [('^ args body) `(closure ,args ,body ,env)]
      [application
       (match (map E application)
	 [('car e) (car e)]
	 [('cdr e) (cdr e)]
	 [('cons e1 e2) (cons e1 e2)]
	 [('+ e1 e2) (+ e1 e2)]
	 [('- e1 e2) (- e1 e2)]
	 [('* e1 e2) (* e1 e2)]
	 [('= e1 e2) (bool->T/nil (eqv? e1 e2))]
	 [('< e1 e2) (bool->T/nil (< e1 e2))]
	 [('atom? e) (bool->T/nil (atom? e))]
	 [('number? e) (bool->T/nil (number? e))]
	 [(('closure args body bindings) . vals)
	  (Eval body (append (map cons args vals) bindings) topenv)]
	 #;[otherwise `(err on ,(map E application))] )])))

;(Eval '((^ (x) (* x x)) (+ 2 3)) '((x . 23)))

;; program:=(def)+
;; def:=('! var expr)

(define (run program inputs)
  (let* ((topenv (let fire ((topenv '())
			    (defs program))
		   (if (null? defs)
		       topenv
		       (fire (let* ((def (car defs))
				    (var (cadr def))
				    (expr (caddr def))
				    (expr-val (Eval expr '() topenv)))
			       (update var expr-val topenv))
			     (cdr defs)))))
	 (last-def (last program)) ;; nb it must be lambda def...
	 (init-name (cadr last-def)))
    (Eval `(,init-name . ,inputs) '() topenv)))

(let* ((prog (read))
       (inputs (read)))
  (pretty-print (run prog inputs)))

