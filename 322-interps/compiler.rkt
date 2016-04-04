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
  [call (fn string?) (n-args number?)]
  [tail-call (fn string?) (n-args number?)]
  [return])

(define (parse-L1 expr)
  (let ([label-result (parse-label-check expr)])
    (if label-result
        label-result
        (parse expr))))
      

(define (parse-label-check expr)
  (match expr
    [`(? symbol?) (label-dest (string-append (string-append "_" (substring (symbol->string expr) 1)) ":"))]
    [_ #f]))
(define (parse expr)
  (match expr
    [`(,w <- (mem, src, offset)) (mem-read (parse-arg w) (parse-arg src) offset)]
    [`((mem, dest, offset) <- ,w) (mem-write (parse-arg dest) (parse-arg w)  offset)]
    [`(,w <- ,s) (arrow-assign w s)]
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

(define (compile instr)
  (type-case L1-expr instr
    [register (name) (format "%~A" name)]
    [num (n) (format "$~A" n)]
    [label-expr (l) (format "$~A" l)]
    [label-dest (l) (format "~A\n")]
    [arrow-assign (l r)  (format "movq ~A, ~A" (compile r) (compile l))]
    [cmp-assign (dest op1 cmp op2) (type-case L1-expr op1
                                     [num (n1) (type-case L1-expr op2
                                                 [num (n2) (format "movq $~A, ~A" ((cmp-map cmp) op1 op2) (8-bit-reg dest))]
                                                 [else(cond
                                                        [(symbol=? cmp '<=) (format "cmpq ~A, ~A\nsetge %~A\nmovzbq %~A, ~A" (compile op1) (compile op2) (8-bit-reg dest) (8-bit-reg dest) (compile dest))]
                                                        [(symbol=? cmp '<) (format "cmpq ~A, ~A\nsetg %~A\nmovzbq %~A, ~A" (compile op1) (compile op2) (8-bit-reg dest) (8-bit-reg dest) (compile dest))])])]
                                     [register (name) (cond 
                                                        [(symbol=? cmp '<=) (format "cmpq ~A, ~A\nsetle %~A\nmovzbq %~A, ~A" (compile op2) (compile op1) (8-bit-reg dest) (8-bit-reg dest) (compile dest))]
                                                        [(symbol=? cmp '<) (format "cmpq ~A, ~A\nsetle %~A\nmovzbq %~A, ~A" (compile op2) (compile op1) (8-bit-reg dest) (8-bit-reg dest) (compile dest))]
                                                        )]
                                     [else '()])]                                                      
    [mem-read (w src offset) (format "movq ~A(~A), ~A" offset (compile src) (compile w))]
    [mem-write (w dest offset) (format "movq ~A, ~A(~A)" (compile w) offset (compile dest))]
    [aop+ (l r) (format "addq ~A, ~A" (compile r) (compile l))]
    [aop- (l r) (format "subq ~A, ~A" (compile r) (compile l))]
    [aop* (l r) (format "imulq ~A, ~A" (compile r) (compile l))]
    [aop& (l r) (format "andq ~A, ~A" (compile r) (compile l))]
    [sop<< (l r) (format "salq ~A, ~A" (compile r) (compile l))]
    [sop>> (l r) (format "sarq ~A, ~A" (compile r) (compile l))]
    
    [else '()]))
    
    
    
    
    


                        