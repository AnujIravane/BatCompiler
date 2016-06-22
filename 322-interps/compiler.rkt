#lang plai
(define-type L1-expr
  [register (name symbol?)]
  [num (n number?)]
  [arrow-assign (l L1-expr?) (r L1-expr?)]
  [mem-read (dest L1-expr?) (source L1-expr?) (offset number?)]
  [mem-write (dest L1-expr?) (source L1-expr?) (offset number?)]
  [aop+ (l L1-expr?) (r L1-expr?)]
  [aop* (l L1-expr?) (r L1-expr?)]
  [aop- (l L1-expr?) (r L1-expr?)]
  [aop& (l L1-expr?) (r L1-expr?)]
  [sop<< (l L1-expr?) (r L1-expr?)]
  [sop>> (l L1-expr?) (r L1-expr?)]
  [cmp-assign (dest L1-expr?) (op1 L1-expr?) (cmp symbol?) (op2 L1-expr?)]
  [label-dest (l string?)]
  [label-expr (l string?)]
  [goto (dest L1-expr?)]
  [cjump (op1 L1-expr?) (cmp symbol?) (op2 L1-expr?) (dest1 L1-expr?) (dest2 L1-expr?)]
  [call (fn symbol?) (n-args number?)]
  [tail-call (fn symbol?) (n-args number?)]
  [return])

(define (parse-L1 expr)
  (let ([label-result (parse-label-check expr)])
    (if label-result
        label-result
        (parse expr))))
      

(define (parse-label-check expr)
  (match expr
    [(? symbol?) (label-dest (string-append (string-append "_" (substring (symbol->string expr) 1)) ":"))]
    [_ #f]))

(define (parse expr)
  (match expr
    [`(,w <- (mem, src, offset)) (mem-read (parse-arg w) (parse-arg src) offset)]
    [`((mem, dest, offset) <- ,w) (mem-write (parse-arg w) (parse-arg dest) offset)]
    [`(,w <- ,s) (arrow-assign (parse-arg w) (parse-arg s))]
    [`(,w <- ,t1 ,cmp ,t2) (cmp-assign (parse-arg w) (parse-arg t1) cmp (parse-arg t2))]
    [`(,w += ,s) (aop+ (parse-arg w) (parse-arg s))]
    [`(,w *= ,s) (aop* (parse-arg w) (parse-arg s))]
    [`(,w -= ,s) (aop- (parse-arg w) (parse-arg s))]
    [`(,w &= ,s) (aop& (parse-arg w) (parse-arg s))]
    [`(,w <<= ,s) (sop<< (parse-arg w) (parse-arg s))]
    [`(,w >>= ,s) (sop>> (parse-arg w) (parse-arg s))]
    [(? symbol?) (label-expr (string-append "_" (substring (symbol->string expr) 1)))]
    [`(goto ,label) (goto (parse-arg label))]
    [`(call ,fn ,n) (call fn n)]
    [`(cjump ,t1 ,cmp ,t2 ,label1 ,label2) (cjump (parse-arg t1) cmp (parse-arg t2) (parse-arg label1) (parse-arg label2))]
    [`(tail-call ,fn ,n) (tail-call fn n)]
    [`(return) (return)]))
    
    

(define (parse-arg arg)
  (match arg
    [(? number?) (num arg)]
    [(? symbol?) (let ([arg-string (symbol->string arg)])
                   (if (char=?  #\: (string-ref arg-string 0))
                       (label-expr
                        (string-append "_" (substring arg-string 1)))
                        (register arg)))]))

(define (cmp-map sym)
  (match sym
    ['< <]
    ['<= <=]
    ['= =]))
(define (8-bit-reg reg)
  (match reg
    ['r10  'r10b ]
    ['r11  'r11b]
    ['r12  'r12b]
    ['r13  'r13b]
    ['r14  'r14b]
    ['r15  'r15b]
    ['r8  'r8b]
    ['r9  'r9b]
    ['rax  'al]
    ['rbp  'bpl]
    ['rbx  'bl]
    ['rcx 'cl]
    ['rdi 'dil]
    ['rdx  'dl]
    ['rsi  'sil]))

(define all-regs '(rax rbx rbp r10 r11 r12 r13 r14 r15 rdi rsi rdx rcx r8 r9))

(define (isReg? x)
  (member x all-regs))
(define args-spill 0)
(define (label-or-reg exp)
  (if (isReg? exp)
      (format "*%~A" exp)
      (format "_~A" (substring (symbol->string exp) 1))))


(define (compile instr)
  (type-case L1-expr instr
    [register (name) (format "%~A" name)]
    [num (n) (format "$~A" n)]
    [label-expr (l) (format "$~A" l)]
    [label-dest (l) (format "~A\n" l)]
    [arrow-assign (l r)  (format "movq ~A, ~A\n" (compile r) (compile l))]
    [cmp-assign (dest op1 cmp op2) (type-case L1-expr op1
                                     [num (n1) (type-case L1-expr op2
                                                 [num (n2) (format "movq $~A, ~A\n" (if ((cmp-map cmp) n1 n2) 1 0)  (compile dest))]
                                                 [else (cond
                                                        [(symbol=? cmp '<=) (format "cmpq ~A, ~A\nsetge %~A\nmovzbq %~A, ~A\n" (compile op1) (compile op2) (8-bit-reg (register-name dest)) (8-bit-reg (register-name dest)) (compile  dest))]
                                                        [(symbol=? cmp '=) (format "cmpq ~A, ~A\nsete %~A\nmovzbq %~A, ~A\n" (compile op1) (compile op2) (8-bit-reg (register-name dest)) (8-bit-reg (register-name dest)) (compile dest))]
                                                        [(symbol=? cmp '<) (format "cmpq ~A, ~A\nsetg %~A\nmovzbq %~A, ~A\n" (compile op1) (compile op2) (8-bit-reg (register-name dest)) (8-bit-reg (register-name dest)) (compile dest))])])]
                                     [register (name) (cond 
                                                        [(symbol=? cmp '<=) (format "cmpq ~A, ~A\nsetle %~A\nmovzbq %~A, ~A\n" (compile op2) (compile op1) (8-bit-reg (register-name dest)) (8-bit-reg (register-name dest)) (compile dest))]
                                                        [(symbol=? cmp '=) (format "cmpq ~A, ~A\nsete %~A\nmovzbq %~A, ~A\n" (compile op2) (compile op1) (8-bit-reg (register-name  dest)) (8-bit-reg (register-name  dest)) (compile dest))]
                                                        [(symbol=? cmp '<) (format "cmpq ~A, ~A\nsetl %~A\nmovzbq %~A, ~A\n" (compile op2) (compile op1) (8-bit-reg (register-name dest)) (8-bit-reg (register-name dest)) (compile dest))]
                                                        )]
                                     [else 'HUH])]                                                      
    [mem-read (w src offset) (format "movq ~A(~A), ~A\n" offset (compile src) (compile w))]
    [mem-write (w dest offset) (format "movq ~A, ~A(~A)\n" (compile w) offset (compile dest))]
    [aop+ (l r) (format "addq ~A, ~A\n" (compile r) (compile l))]
    [aop- (l r) (format "subq ~A, ~A\n" (compile r) (compile l))]
    [aop* (l r) (format "imulq ~A, ~A\n" (compile r) (compile l))]
    [aop& (l r) (format "andq ~A, ~A\n" (compile r) (compile l))]
    [sop<< (l r) (format "salq ~A, ~A\n" (type-case L1-expr r
                                           [register (name) (format "%~A" (8-bit-reg name))]
                                           [else (compile r)]) (compile l))]
    [sop>> (l r) (format "sarq ~A, ~A\n" (type-case L1-expr r
                                           [register (name) (format "%~A"(8-bit-reg name))]
                                           [else (compile r)]) (compile l))]
    [goto (l) (format "jmp ~A\n" (substring (compile l) 1))]
    [cjump (op1 cmp op2 dest1 dest2) (type-case L1-expr op1
                                       [num (n1) (type-case L1-expr op2
                                                   [num (n2) (format "jmp ~A\n" (if ((cmp-map cmp) n1 n2)
                                                                                    (substring (compile dest1) 1)
                                                                                    (substring (compile dest2) 1)))]
                                                   [else (cond
                                                           [(symbol=? cmp '<=) (format "cmpq ~A, ~A\njge ~A\njmp ~A\n" (compile op1) (compile op2) (substring (compile dest1) 1) (substring (compile dest2) 1))]
                                                           [(symbol=? cmp '=) (format "cmpq ~A, ~A\nje ~A\njmp ~A\n" (compile op1) (compile op2) (substring (compile dest1) 1) (substring (compile dest2) 1))]
                                                           [(symbol=? cmp '<) (format "cmpq ~A, ~A\njg ~A\njmp ~A\n" (compile op1) (compile op2) (substring (compile dest1) 1) (substring (compile dest2) 1) )])])]
                                       [register (name) (cond
                                                          [(symbol=? cmp '<=) (format "cmpq ~A, ~A\njle ~A\njmp ~A\n" (compile op2) (compile op1) (substring (compile dest1) 1) (substring (compile dest2) 1) )]
                                                          [(symbol=? cmp '=) (format "cmpq ~A, ~A\nje ~A\njmp ~A\n" (compile op2) (compile op1) (substring (compile dest1) 1) (substring (compile dest2) 1))]
                                                          [(symbol=? cmp '<) (format "cmpq ~A, ~A\njl ~A\njmp ~A\n" (compile op2) (compile op1) (substring (compile dest1) 1) (substring (compile dest2) 1) )])]
                                       [else '()])]
    [call (fn n) (match fn
                   ['array-error (format "call array_error\n")]
                   ['print (format "call print\n")]
                   ['allocate (format "call allocate\n")]
                   ['read (format "call read\n")]
                   [_ (format "subq $~A, %rsp\njmp ~A\n" (if (> n 6) (* 8 (- n 5)) 8) (label-or-reg fn))])]
    [tail-call (fn n) (format "addq $~A, %rsp\njmp ~A\n" (* 8 args-spill) (label-or-reg fn))]
    [return () (format "addq $~A, %rsp\nret\n" (* 8 args-spill))]))
    

(define (compile-function fn-code)
  (let ([args (cadr fn-code)]
        [spill (caddr fn-code)])
    (begin
      (set! args-spill (+ (if (> args 6) (- args 6) 0) spill))
      (string-append
       (format "_~A:\nsubq $~A, %rsp\n" (substring (symbol->string (first fn-code)) 1) (* 8 spill))
       (foldr string-append
              ""
              (map (lambda (x) (compile (parse-L1 x))) (cdddr fn-code)))))))

(define (compile-main main-code)
  (format ".text
.globl go
go:
pushq %rbx
pushq %rbp
pushq %r12
pushq %r13
pushq %r14
pushq %r15
call _~A
popq %r15
popq %r14
popq %r13
popq %r12
popq %rbp
popq %rbx
retq\n"
          (substring (symbol->string main-code) 1)))
          
 
(define (L1->Assembly code)
  (string-append
   (compile-main (first code))
   (foldr string-append "" (map compile-function (rest code)))))


(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
      (lambda (x) (display (L1->Assembly (read x))))))





