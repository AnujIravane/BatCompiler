#lang racket


(define keywords
  '(begin
     new-array
     new-tuple
     read
     aref
     aset
     alen
     +
     -
     *
     <
     <=
     =
     number?
     a?
     print))

(define (get-frees exp bounded)
  (match exp
    [`(lambda (,x ...) ,b) (get-frees b (append x bounded))]
    [`(let ([,var ,d]) ,b) (append (get-frees d bounded) (get-frees b (cons var bounded)))]
    [`(letrec ([,var ,e1]) ,e2) (append (get-frees e1 (cons var bounded)) (get-frees e2 (cons var bounded)))]
    [`(if ,c ,t ,e) (append (get-frees c bounded) (get-frees t bounded) (get-frees e bounded))]
    [`(begin ,a ,b) (append (get-frees a bounded) (get-frees b bounded))]
    [`(,f ,a ...) (append (get-frees f bounded) (apply append (map (lambda (x) (get-frees x bounded)) a)))]
    [(? number?) '()]
    [(? symbol?) (if (or (set-member? bounded exp)
                         (set-member? keywords exp))
                     '()
                     `(,exp))]))

(define (free-var-assign free-vars exp i)
  (if (empty? free-vars)
      (eliminate-lambda exp)
      `(let ([,(car free-vars) (aref vars-tup ,i)])
         ,(free-var-assign (cdr free-vars) exp (+ i 1)))))

(define (new-func name free-vars args e)
  ;(if (empty? free-vars)
   ;   `(,name (,@args)
    ;          ,(free-var-assign free-vars e 0))
      `(,name (vars-tup ,@args)
              ,(free-var-assign free-vars e 0)))

(define (replace-free-var exp var)
   (match exp
    [`(lambda (,x ...) ,b) (if  (set-member? x var)   
                               exp
                               `(lambda ,x ,(replace-free-var b var)))]
    [`(let ([,var1 ,d]) ,b) (if  (equal? var1 var)
                               `(let ([,var1 ,(replace-free-var d var)]) ,b)
                               `(let ([,var1 ,(replace-free-var d var)]) ,(replace-free-var b var )))]
    [`(letrec ([,var1 ,e1]) ,e2) (if (equal? var1 var)
                                     exp
                                     `(letrec ([,var1 ,(replace-free-var e1 var)]) ,(replace-free-var e2 var)))]
                                    
    [`(if ,c ,t ,e) `(if ,(replace-free-var c var) ,(replace-free-var t var ) ,(replace-free-var e var)) ]
    [`(begin ,a ,b) `(begin ,(replace-free-var a var ) ,(replace-free-var b var ))]
    [`(,f ,a ...) `(,(replace-free-var f var) ,@(map (lambda (x) (replace-free-var x var)) a))]
    [(? number?) exp]
    [(? symbol?) (if (equal? exp var)
                     `(aref ,var 0)
                     exp)]))
          
          
(define 0-arity '(read))
(define 1-arity '(number? a? print alen))
(define 2-arity '(aref new-array + - * < <= =))
(define 3-arity '(aset))

(define new-funcs '())
(define fn-count 0)
(define l-count 0)
(define (fresh-fn-name)
  (set! fn-count (+ 1 fn-count))
  (string->symbol (format ":newL5fn~A" fn-count)))

(define (fresh-label)
  (set! l-count (+ 1 l-count))
  (string->symbol (format "newL5label~A" l-count)))



(define (eliminate-lambda exp)
  (match exp
    [`(lambda (,x ...) ,e) (let ([free-vars (remove-duplicates (get-frees e x))]
                                 [new-fn-name (fresh-fn-name)])
                             (set! new-funcs (cons (new-func new-fn-name free-vars x e) new-funcs))
                             (set! fn-count (+ fn-count 1))
                             `(make-closure ,new-fn-name (new-tuple ,@free-vars)))]
    [`(let ([,var ,d]) ,e) `(let ([,var ,(eliminate-lambda d)]) ,(eliminate-lambda e))]
    [`(letrec ([,var ,e1]) ,e2) (eliminate-lambda `(let ([,var (new-tuple 0)])
                                                     (begin (aset ,var 0 ,(replace-free-var e1 var))
                                                            ,(replace-free-var e2 var))))]
    [`(if ,c ,t ,e) `(if ,(eliminate-lambda c) ,(eliminate-lambda t) ,(eliminate-lambda e))]
    [`(,f ,a ...) (if (set-member? keywords f)
                      `(,f ,@(map eliminate-lambda a))
                      (let ([flabel (fresh-label)])
                      `(let ([,flabel ,(eliminate-lambda f)])
                         ((closure-proc ,flabel) (closure-vars ,flabel) ,@(map eliminate-lambda a)))))]
    [(? number?) exp]
    [(? symbol?) (if (set-member? keywords exp)
                     (eliminate-lambda (prim-f exp))
                     exp)]))


(define (prim-f exp)
  (cond
    [(set-member? 0-arity exp) `(lambda () (,exp))]
    [(set-member? 1-arity exp) `(lambda (x) (,exp x))]
    [(set-member? 2-arity exp) `(lambda (x y) (,exp x y))]
    [(set-member? 3-arity exp) `(lambda (x y z) (,exp x y z))]))

(define (L5->L4 prog)
  (set! new-funcs '())
  (let ([e1 (eliminate-lambda  prog)])
    (pretty-write `(,e1 ,@new-funcs))))

(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (lambda (x)
       (L5->L4 (read x))
               )))


