; demet yayla
; 2019400105
; compiling: yes
; complete: yes

#lang racket
(provide (all-defined-out))


(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))


(define get-value-single (lambda (num) (num-value num)))
(define get-value (lambda(x) (if (list? x) (map get-value-single (map eval x)) (get-value-single x))))


(define get-grad-single (lambda (num) (num-grad num)))
(define get-grad (lambda(x) (if (list? x) (map get-grad-single (map eval x)) (get-grad-single x))))


(define add (lambda args (num (apply + (get-value args)) (apply + (get-grad args)))))


(define derivMul (lambda (g v m) (apply + (map (lambda(a b) (/ b a)) v (map (lambda (l) (* m l)) g)))))
(define mul (lambda args (num (apply * (get-value args)) (derivMul (get-grad args) (get-value args) (apply * (get-value args))))))


(define sub (lambda (a b) (num (- (get-value a) (get-value b)) (- (get-grad a) (get-grad b)))))


(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))


(define mse (lambda (x y) (mul (sub x y) (sub x y))))


(define create-hash (lambda (names values var) (make-hash (map (lambda (a b)(if (eq? var a) (cons a  (num b 1.0)) (cons a (num b 0.0)) )) names values ))) )


(define operator (list '* '+ '- 'mse 'relu))
(define operatorH #hash((* . mul) (- . sub) (+ . add) (mse . mse) (relu . relu)))
(define (parse hash expr) (cond ((null? expr) '())
                                ((list? (member (car expr) operator)) (cons (hash-ref operatorH (car expr)) (parse hash (cdr expr))))
                                ((number? (car expr)) (cons (num (car expr) 0.0) (parse hash (cdr expr))))
                                ((list? (car expr)) (cons (parse hash (car expr)) (parse hash (cdr expr))))
                                (else (cons (hash-ref hash (car expr)) (parse hash (cdr expr))))))


(define (grad names values var expr) (get-grad (eval (parse (create-hash names values var) expr)))) ;I reaaaly not sure of this


(define (partial-grad names values vars expr ) (map(lambda (x)(cond ((null? names) '())
                                                                    ((list? (member x vars)) (grad names values x expr) )
                                                                    (else '0.0)))names))


(define (gradient-descent names values vars lr expr ) (let ((sub (map (lambda (x) (* lr x))(partial-grad names values vars expr))))
                                                        (map (lambda (y x) (- x y) ) sub values)))  


(define (optimize names values vars lr k expr ) (cond ((eq? k '0) values)
                                                      (else (optimize names (gradient-descent names values vars lr expr) vars lr (- k 1) expr))))