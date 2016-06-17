(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 pretty-print))

(define (assoc_ k al)
  (let ((v (assoc k al)))
    (if v (cdr v) #f)))

(define (zip_ xs ys) (map cons xs ys))

;; store ;;
(define (lookup v store) (assoc_ v store))

(define (update v e store)
  (match store
    (() `((,v . ,e)))
    (((v0 . e0) . store)
     (if (eq? v0 v)
	 `((,v0 . ,e) . ,store)
	 `((,v0 . ,e0) . ,(update v e store))))))
;(update 'z 4 (update 'y 'elo (update 'x 3 '())))

(define (eval-expression expr store)
  (let E ((expr expr))
    (match expr
      (() '())
      ('T 'T)
      (('quote e) e)
      ((? number? n) n)
      ((? symbol? s) (lookup s store))
      (('= e1 e2) (if (equal? (E e1) (E e2)) 'T '()))
      (('< e1 e2) (if (< (E e1) (E e2)) 'T '()))
      (('+ e1 e2) (+ (E e1) (E e2)))
      (('* e1 e2) (* (E e1) (E e2)))
      (('- e1 e2) (- (E e1) (E e2)))
      (('/ e1 e2) (/ (E e1) (E e2)))
      (('% e1 e2) (modulo (E e1) (E e2)))
      (('cons e1 e2) (cons (E e1) (E e2)))
      (('car e) (car (E e)))
      (('cdr e) (cdr (E e)))
      (('atom? e) (if (pair? (E e)) '() 'T))
      (('number? e) (if (number? (E e)) 'T '()))
      (otherwise `(SYNTAX ERROR in expression ,expr)))))
;(eval-expression '(+ (* 7 8) x) `((x . 3)))

;; program: ( vars start . blockmap )
;; block : ( label . cmds )
;; cmd : ('let var expr)
;;     | ('goto label)
;;     | ('if expr label1 label2) 
;;     | ('return expr)

(define (find-block label block-map)
;  (pretty-print `(block: ,label))
  (assoc_ label block-map))

(define (run program inputs)
  (let* ((input-names (car program))
	 (start-label (cadr program))
	 (block-map (cddr program))
	 (store (zip_ input-names inputs)))
    (run-block (find-block start-label block-map)
	       store
	       block-map)))

(define (run-block block store block-map)
 ; (write #;pretty-print store) (newline)
  (match block
    ((('let v e) . block)
     (run-block block
		(update v (eval-expression e store) store)
		block-map))
    ((('return e))
     (eval-expression e store))
    ((('goto l))
     (run-block (find-block l block-map)
		store
		block-map))
    ((('if e l1 l2))
     (if (eq? '() (eval-expression e store))
	 (run-block (find-block l2 block-map)
		    store
		    block-map)
	 (run-block (find-block l1 block-map)
		    store
		    block-map)))
    (otherwise
     `(SYNTAX ERROR in block ,block))))

;;; 3:07 -- 14 minut do działania. nie starzejemy się!

;(display "program >")
(define p1 (read))
;(newline)
;(display "inputs >")
(define is1 (read))
;(newline)

;(display "result :")
(pretty-print (run p1 is1))
