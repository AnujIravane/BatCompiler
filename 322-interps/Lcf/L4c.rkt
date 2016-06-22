#lang plai

(print-only-errors)
(define tcount 0)

(define (L4e? x)
  #t)
(define (val? x)
  (or (symbol? x) (number? x)))
(define-type context
  [letcx (x symbol?)
         (b L4e?) 
         (c context?)]
  [ifcx (then L4e?)
        (else L4e?)
        (c context?)]
  [funcx (fs (listof L4e?))
         (r (listof L4e?))
         (c context?)]
  [nocx])

(define (fresh-num)
  (set! tcount (+ 1 tcount))
  (string->symbol (format "tempp~A" tcount)))

(define (replace-begin e)
  (match e
    [`(begin ,a ,b) `(let ((,(fresh-num) ,(replace-begin a))) ,(replace-begin b))]
    [`(let ([,var ,d]) ,e) `(let ([,var ,(replace-begin d)]) ,(replace-begin e))]
    [`(if ,c ,t ,e) `(if ,(replace-begin c) ,(replace-begin t) ,(replace-begin e))]
    [`(,f ,a ...) `(,(replace-begin f) ,@(map replace-begin a))]
    [(? val?) e]))
(define (f1 exp ctxt)
  (match exp
    [`(let ([,x ,d]) ,b)
     (f1 d (letcx x b ctxt))]
    [`(if ,c ,t ,e)
     (f1 c (ifcx t e ctxt))]
    [`(,f ,a ...)
     (f1 f (funcx '() a ctxt))]
    [(? val?)
     (f2 exp ctxt)]))

(define (f2 exp ctxt)
  (type-case context ctxt
    [letcx (x b c)
           `(let ([,x ,exp])
              ,(f1 b c))]
    [ifcx (t e c)
          (if (val? exp)
              `(if ,exp
                   ,(f1 t c)
                   ,(f1 e c))
              (let ([x (fresh-num)])
                `(let ([,x ,exp])
                   (if ,x
                       ,(f1 t c)
                       ,(f1 e c)))))]
    [funcx (fs r c)
          (if (val? exp)
              (if (empty? r)
                  (f2 `(,@fs ,exp) c)
                  (f1 (car r) (funcx `(,@fs ,exp) (cdr r) c)))
              (let ([n (fresh-num)])
                `(let ([,n ,exp])
                   ,(if (empty? r)
                        (f2 `(,@fs ,n) c)
                        (f1 (car r) (funcx `(,@fs ,n) (cdr r) c))))))]
    [nocx () exp]))
           

(define (normalize exp)
  (set! tcount 0)
  (f1 exp (nocx)))

(define (L4->L3 e)
  (if (empty? (cdr e))
      (list (normalize (replace-begin (car e))))
      `(,(normalize (replace-begin (car e))),@(map (lambda (x) `(,(car x) ,(cadr x) ,(normalize (replace-begin (caddr x))))) (cdr e))))) 
   









