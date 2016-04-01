#lang plai
(define-type L1-expr
  [register (name symbol?)]
  [num (n number?)]
  [arrow-assign (l L1-expr?) (r L1-expr?)]
  [mem-read (dest L1-expr?) (source L1-expr?) (offset number?)]
  [mem-write (dest L1-expr?) (source L1-expr?) (offset number?)]
  [aop (aop-type symbol?) (l L1-expr?) (r L1-expr?)]
  [sop (sop-type symbol?) (l L1-expr?) (r L1-expr?)]
  [cmp-assign (dest L1-expr?) (op1 L1-expr?) (cmp symbol?) (op2 L1-expr?)]
  [label (l string?)]
  [goto (dest L1-expr?)]
  [cjump (op1 L1-expr?) (cmp symbol?) (op2 L1-expr?) (dest1 L1-expr?) (dest2 L1-expr?)]
  [call (fn string?) (n-args number?)]
  [tail-call (fn string?) (n-args number?)]
  [return])

(define (parse expr)
  (match expr
    [`(,w <- (mem, src, offset)) (mem-read (parse-arg w) (parse-arg src) offset)]
    [`((mem, dest, offset) <- ,w) (mem-write (parse-arg dest) (parse-arg w)  offset)]
    [`(,w <- ,s) (arrow-assign w s)]
    [`(,w <- ,t1 ,cmp ,t2) (cmp-assign (parse-arg w) (parse-arg t1) cmp (parse-arg t2))]
    [`(,w += ,s) (aop '+= (parse-arg w) (parse-arg s))]
    [`(,w *= ,s) (aop '*= (parse-arg w) (parse-arg s))]
    [`(,w -= ,s) (aop '-= (parse-arg w) (parse-arg s))]
    [`(,w <<= ,s) (sop '<<= (parse-arg w) (parse-arg s))]
    [`(,w >>= ,s) (sop '>>= (parse-arg w) (parse-arg s))]
    [`(goto label) (goto (parse-arg label))]
    [`(call ,fn ,n) (call fn n)]
    [`(cjump ,t1 ,cmp ,t2 ,label1 ,label2) (cjump (parse-arg t1) cmp (parse-arg t2) (parse-arg label1) (parse-arg label1))]
    [`(tail-call ,fn ,n) (tail-call fn n)]
    [`(return) (return)]))
    
    

(define (parse-arg arg)
  (match arg
    [(? number?) (num arg)]
    [(? symbol?) (let ([arg-string (symbol->string arg)])
                   (if (string=? (string #\:) (string-ref arg-string 0))
                       (string-append "_" (substring arg-string 1))
                       (register arg)))]))

                        