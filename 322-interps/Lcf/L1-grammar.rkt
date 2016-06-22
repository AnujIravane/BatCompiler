#lang plai

(define (L1-p? p-exp)
  (match p-exp
    [`(,(? is-label?) ,(? L1-f?) ...) #t]
    [_ #f]))

(define (L1-f? f-exp)
  (match f-exp
    [`(,(? is-label?) ,(? number?) ,(? number?) ,(? L1-i?) ...) #t]
    [_ #f]))

(define (L1-i? i-exp)
  (match i-exp
    [`(,(? L1-w?) <- ,(? L1-s?)) #t]
    [`(,(? L1-w?) <- (mem ,(? L1-x?) ,(? number?))) #t]
    [`((mem ,(? L1-x?) ,(? number?)) <- ,(? L1-s?)) #t]
    [`(,(? L1-w?) ,(? is-aop?) ,(? L1-t?)) #t]
    [`(,(? L1-w?) ,(? is-sop?) ,(? L1-sx?)) #t]
    [`(,(? L1-w?) ,(? is-sop?) ,(? number?)) #t]
    [`(,(? L1-w?) <- ,(? L1-t?) ,(? is-cmp?) ,(? L1-t?)) #t]
    [(? is-label?) #t]
    [`(goto ,(? is-label?)) #t]
    [`(cjump ,(? L1-t?) ,(? is-cmp?) ,(? L1-t?) ,(? is-label?) ,(? is-label?)) #t]
    [`(call ,(? L1-u?) ,(? number?)) #t]
    [`(call print 1) #t]
    [`(call allocate 2) #t]
    [`(call array-error 2) #t]
    [`(tail-call ,(? L1-u?) ,(? number?)) #t]
    [`(return) #t]
    [_ #f]))

(define (is-aop? sym)
  (match sym
    ['+= #t]
    ['-= #t]
    ['*= #t]
    ['&= #t]
    [_ #f]))

(define (is-sop? sym)
  (match sym
    ['<<= #t]
    ['>>= #t]
    [_ #f]))

(define (is-label? sym)
  (if (symbol? sym)
      (regexp-match? #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string sym))
      #f))

(define (is-cmp? sym)
  (and (symbol? sym)
       (or (equal? sym '<)
           (equal? sym '<=)
           (equal? sym '=))))

(define (L1-u? u-exp)
  (or (L1-x? u-exp)
      (is-label? u-exp)))

(define (L1-t? t-exp)
  (or (L1-x? t-exp)
      (number? t-exp)))

(define (L1-s? s-exp)
  (or (L1-x? s-exp)
      (number? s-exp)
      (is-label? s-exp)))

(define (L1-x? x-exp)
  (or (L1-w? x-exp)
      (equal? 'rsp x-exp)))

(define (L1-w? w-exp)
  (or (L1-a? w-exp)
      (equal? 'rax w-exp)
      (equal? 'rbx w-exp)
      (equal? 'rbp w-exp)
      (equal? 'r10 w-exp)
      (equal? 'r11 w-exp)
      (equal? 'r12 w-exp)
      (equal? 'r13 w-exp)
      (equal? 'r14 w-exp)
      (equal? 'r15 w-exp)))

(define (L1-a? a-exp)
  (or (L1-sx? a-exp)
      (equal? 'rdi a-exp)
      (equal? 'rsi a-exp)
      (equal? 'rdx a-exp)
      (equal? 'r8 a-exp)
      (equal? 'r9 a-exp)))

(define (L1-sx? sx-exp)
  (equal? 'rcx sx-exp))