#lang plai

(require "L1-grammar.rkt")

(define top-level-code-block-1
  ".text\n.global go\ngo:\npushq %rbx\npushq %rbp\npushq %r12\npushq %r13\npushq %r14\npushq %r15\ncall ")

(define top-level-code-block-2
  "popq %r15\npopq %r14\npopq %r13\npopq %r12\npopq %rbp\npopq %rbx\nretq\n")


(define/contract (L1p->x86 p)
  (-> L1-p? string?)
  (match p
    [`(,(? is-label? l) ,(? L1-f? f) ...)
     (format "~a ~a\n ~a ~a"
             top-level-code-block-1
             (make-x86-label l)
             top-level-code-block-2
             (string-append* (map L1f->x86 f)))]))

(define/contract (L1f->x86 f)
  (-> L1-f? string?)
  (match f
    [`(,(? is-label? l) ,(? number? argc) ,(? number? stack-space) ,(? L1-i? i-lst) ...)
     (format "~a:\nsubq $~a, %rsp\n~a\n"
             (make-x86-label l)
             (number->string (* stack-space 8))
             (string-append* (map (λ (i) (L1i->x86 i argc stack-space)) i-lst)))]))

(define/contract (L1i->x86 i f-argc f-stack-space)
  (-> L1-i? number? number? string?)
  (match i
    [`(,(? L1-w? w) <- ,(? L1-s? s))
     (format "movq ~a, %~a\n"
             (convert-s/t s)
             w)]
    [`(,(? L1-w? w) <- (mem ,(? L1-x? x) ,(? number? offset)))
     (format "movq ~a(%~a), %~a\n"
             offset x w)]
    [`((mem ,(? L1-x? x) ,(? number? offset)) <- ,(? L1-s? s))
     (format "movq ~a, ~a(%~a)\n"
             (convert-s/t s)
             offset
             x)]
    [`(,(? L1-w? w) ,(? is-aop? aop) ,(? L1-t? t))
     (format "~a ~a, %~a\n"
             (convert-aop aop)
             (convert-s/t t)
             w)]
    [`(,(? L1-w? w) ,(? is-sop? sop) ,(? L1-sx?))
     (format "~a %cl, %~a\n"
             (convert-sop sop)
             w)]
    [`(,(? L1-w? w) ,(? is-sop? sop) ,(? number? num))
     (format "~a $~a, %~a\n"
             (convert-sop sop)
             num
             w)]
    [`(,(? L1-w? w) <- ,(? L1-t? t1) ,(? is-cmp? cmp) ,(? L1-t? t2))
     (cond
       [(and (number? t1) (number? t2))
        (format "movq $~a, %~a\n"
                (if (((λ (sym) (match sym
                                ['< <]
                                ['<= <=]
                                ['= =])) cmp)
                     t1 t2)
                    1
                    0)
                w)]
       [(number? t1)
        (format "cmpq $~a, %~a\n~a %~a\nmovzbq %~a, %~a\n"
                t1
                t2
                ((λ (sym) (match sym
                            ['<= "setge"]
                            ['< "setg"]
                            ['= "sete"]))
                 cmp)
                (convert-8bit w)
                (convert-8bit w)
                w)]
       [(number? t2)
        (format "cmpq $~a, %~a\n~a %~a\nmovzbq %~a, %~a\n"
                t2
                t1
                ((λ (sym) (match sym
                            ['<= "setle"]
                            ['< "setl"]
                            ['= "sete"]))
                 cmp)
                (convert-8bit w)
                (convert-8bit w)
                w)]
       [else
        (format "cmpq %~a, %~a\n~a %~a\nmovzbq %~a, %~a\n"
                t2
                t1
                ((λ (sym) (match sym
                            ['<= "setle"]
                            ['< "setl"]
                            ['= "sete"]))
                 cmp)
                (convert-8bit w)
                (convert-8bit w)
                w)])]
    [(? is-label? l)
     (format "~a:\n"
             (make-x86-label l))]
    [`(goto ,(? is-label? l))
     (format "jmp ~a\n"
             (make-x86-label l))]
    [`(cjump ,(? L1-t? t1) ,(? is-cmp? cmp) ,(? L1-t? t2) ,(? is-label? l1) ,(? is-label? l2))
     (cond
       [(and (number? t1) (number? t2))
        (format "jmp ~a\n"
                (make-x86-label
                 (if (((λ (sym) (match sym
                                 ['< <]
                                 ['<= <=]
                                 ['= =])) cmp)
                      t1 t2)
                    l1
                    l2)))]
       [(number? t1)
        (format "cmpq $~a, %~a\n~a ~a\njmp ~a\n"
                t1
                t2
                ((λ (sym) (match sym
                            ['<= "jge"]
                            ['< "jg"]
                            ['= "je"]))
                 cmp)
                (make-x86-label l1)
                (make-x86-label l2))]
       [(number? t2)
        (format "cmpq $~a, %~a\n~a ~a\njmp ~a\n"
                t2
                t1
                ((λ (sym) (match sym
                            ['<= "jle"]
                            ['< "jl"]
                            ['= "je"]))
                 cmp)
                (make-x86-label l1)
                (make-x86-label l2))]
       [else
        (format "cmpq %~a, %~a\n~a ~a\njmp ~a\n"
                t2
                t1
                ((λ (sym) (match sym
                            ['<= "jle"]
                            ['< "jl"]
                            ['= "je"]))
                 cmp)
                (make-x86-label l1)
                (make-x86-label l2))])]
    [`(call ,(? L1-u? u) ,(? number? argc))
     (format "subq $~a, %rsp\njmp ~a\n"
             (if (<= argc 6)
                 8
                 (* 8 (- argc 5)))
             (if (is-label? u)
                 (make-x86-label u)
                 (format "*%~a" u)))]
    [`(call print 1)
     "call print\n"]
    [`(call allocate 2)
     "call allocate\n"]
    [`(call array-error 2) 
     "call array_error\n"]
    [`(tail-call ,(? L1-u? u) ,(? number?)) 
     (format "addq $~a, %rsp\njmp ~a\n"
             (if (<= f-argc 6)
                 (* f-stack-space 8)
                 (+ (* f-stack-space 8) (* (- f-argc 6) 8)))
             (if (is-label? u)
                 (make-x86-label u)
                 (format "*%~a" u)))]
    [`(return) 
     (format "addq $~a, %rsp\nret\n"
             (if (<= f-argc 6)
                 (* f-stack-space 8)
                 (+ (* f-stack-space 8) (* (- f-argc 6) 8))))]))

(define/contract (make-x86-label l)
  (-> is-label? string?)
  (format "_~a" (substring (symbol->string l) 1)))

(define/contract (convert-s/t s)
  (-> (or/c L1-s? L1-t?) string?)
  (cond 
    [(is-label? s) (format "$~a" (make-x86-label s))]
    [(number? s) (format "$~a" s)]
    [else (format "%~a" s)]))
    
(define/contract (convert-aop aop)
  (-> is-aop? string?)
  (match aop
    ['+= "addq"]
    ['-= "subq"]
    ['*= "imulq"]
    ['&= "andq"]))

(define/contract (convert-sop sop)
  (-> is-sop? string?)
  (match sop
    ['<<= "salq"]
    ['>>= "sarq"]))

(define/contract (convert-8bit sym)
  (-> symbol? symbol?)
  (match sym
    ['r8 'r8b]
    ['r9 'r9b]
    ['r10 'r10b]
    ['r11 'r11b]
    ['r12 'r12b]
    ['r13 'r13b]
    ['r14 'r14b]
    ['r15 'r15b]
    ['rbp 'bpl]
    ['rdi 'dil]
    ['rax 'al]
    ['rbx 'bl]
    ['rcx 'cl]
    ['rdx 'dl]
    ['rsi 'sil]))
