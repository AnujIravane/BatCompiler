#lang racket

(define all-regs '(rax rbx rbp r10 r11 r12 r13 r14 r15 rdi rsi rdx rcx r8 r9))
(define arg-regs '(rdi rsi rdx rcx r8 r9))

(define (isReg? x)
  (member x all-regs))
(define (isVar3? x)
   (if (number? x) #f
       (if (isReg? x) #f
           (if (or (equal? x 'print)
                   (equal? x 'call)
                   (equal? x 'return)
                   (equal? x 'mem)
                   (equal? x 'cjump)
                   (equal? x 'goto)
                   (equal? x 'allocate)
                   (equal? x 'allocate-error)
                   (equal? x 'stack-arg)
                   (equal? x 'rsp)) #f
               (match x
                 [`(mem ,a ,b) #f]
                 [`(return) #f]
                 [_ (regexp-match #rx"^[a-zA-Z_][-a-zA-Z_0-9]*$" (symbol->string x))])
               ))))

(define (encode x)
  (cond
    [(number? x) (+ (* 2 x) 1)]
    [else x]))

(define (encode-var v)
  (list `(,v *= 2)
        `(,v += 1)))

(define (c-biop2 op a b store)
  (if (equal? a store)
      (c-biop3 op b a b store)
      (if (equal? b store)
          (c-biop3 op a a b store)
          (c-biop op a b store))))

(define (c-biop3 op x a b store)
  (match op
        ['+  (list `(,store += ,(encode x))
                   `(,store -= 1))]          
        ['- (list `(tmps <- ,(encode a))
                  `(tmps -= ,(encode b))
                  `(tmps += 1)
                  `(,store <- tmps)
                  )] 
        ['* (list `(tmpm <- ,(encode x))
                  `(tmpm >>= 1)
                  `(,store >>= 1)
                  `(,store *= tmpm)
                  `(,store *= 2)
                  `(,store += 1))]
        [else (list `(,store <- ,(encode a) ,op ,(encode b))
                    `(,store <<= 1)
                    `(,store += 1))]))
      
(define (c-biop op a b store)
  (match op
    ['+  (list `(,store <- ,(encode a))
               `(,store += ,(encode b))
               `(,store -= 1))]          
    ['- (list `(,store <- ,(encode a))
              `(,store -= ,(encode b))
              `(,store += 1))] 
    ['* (list `(tmp <- ,(encode a))
              `(tmp >>= 1)
              `(,store <- ,(encode b))
              `(,store >>= 1)
              `(,store *= tmp)
              `(,store *= 2)
              `(,store += 1))]
    [else (list `(,store <- ,(encode a) ,op ,(encode b))
                `(,store <<= 1)
                `(,store += 1))]))

(define (c-pred op v x)
  (match op
    ['a? (list `(,x <- ,(encode v))
                    `(,x &= 1)
                    `(,x *= -2)
                    `(,x += 3))]
    ['number? (list `(,x <- ,(encode v))
                   `(,x &= 1)
                   `(,x <<= 1)
                   `(,x += 1))]
    [_ 'dontbreakmychops]))

(define (change-array lst n)
  (if (empty? lst) lst
      (cons `((mem rax ,n) <- ,(encode (car lst)))
            (change-array (cdr lst) (+ n 8)))))
        

(define (make-tuple lst store)
  (append (list `(rdi <- ,(encode (length lst)))
                `(rsi <- 1)
                `(call allocate 2))
          (change-array lst 8)
          (list `(,store <- rax))))
                    
(define (c-aset v1 v2 v3 x)
  (let ([bounds-pass1-label (create-label)]
        [bounds-fail-label (create-label)]
        [bounds-pass2-label (create-label)])
    (list  `(tmprr2 <- ,(encode v2))
           `(tmprr2 >>= 1)
           `(tmp <- (mem ,v1 0))
           `(cjump tmprr2 < tmp  ,bounds-pass1-label ,bounds-fail-label)
           bounds-fail-label
           `(rdi <- ,v1)
           `(rsi <- ,(encode v2))
           `(call array-error 2)
           bounds-pass1-label
           `(cjump 0 <= tmprr2  ,bounds-pass2-label ,bounds-fail-label)
           bounds-pass2-label
           `(tmprr2 *= 8)
           `(tmprr2 += ,v1)
           `((mem tmprr2 8) <- ,(encode v3))
           `(,x <- 1))))   ;; put the final result for aset into x (always 0).

(define (c-aref v1 v2 x)
  (let ([bounds-pass1-label (create-label)]
        [bounds-fail-label (create-label)]
        [bounds-pass2-label (create-label)])
  (list  `(tmppr <- ,(encode v2))
           `(tmppr >>= 1)
           `(tmp <- (mem ,v1 0))
           `(cjump tmppr < tmp  ,bounds-pass1-label ,bounds-fail-label)
           bounds-fail-label
           `(rdi <- ,v1)
           `(rsi <- ,(encode v2))
           `(call array-error 2)
           bounds-pass1-label
           `(cjump 0 <= tmppr  ,bounds-pass2-label ,bounds-fail-label)
           bounds-pass2-label
           `(tmppr *= 8)
           `(tmppr += ,v1)
           `(,x <- (mem tmppr 8))
           )))
   ;(list `(,store <- (mem ,v1 ,(* 8 (+ 1 v2)))))

(define (get-arg-regs n isclosure)
  (if (<= n 6)
      (take arg-regs n)
      (if isclosure
          (append arg-regs (reverse (build-list (- n 6) (lambda (x) `(stack-arg ,(* 8 x))))))
          (append arg-regs (build-list (- n 6) (lambda (x) `(mem rsp ,(* -8 (+ 2 x)))))))))

(define (c-fcall label vars store)
  (define return-label (create-label))
  (if (or (> (length vars) 6) (not (equal? store 'rax)))
      (append (list `((mem rsp -8) <- ,return-label))
              (for/list ([arg vars]
                         [loc (get-arg-regs (length vars) #f)])
                `(,loc <- ,(encode arg)))
              (list `(call ,label ,(length vars))
                    return-label
                    `(,store <- rax)))
      (append (for/list ([arg vars]
                         [loc (get-arg-regs (length vars) #f)])
                `(,loc <- ,(encode arg)))
              (list `(tail-call ,label ,(length vars))))))
      
                  
(define l-count 0)

(define (create-label)
  (set! l-count (+ 1 l-count))
  (string->symbol (format ":label~A" l-count)))

(define (label? x)
  (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string x)))
  
(define (compileL3 exp store)
   (match exp
     [`(,(? label? label) ,(? list? args) ,exp)  (append (list label (length args) 0)
                                                         (for/list ([arg args]
                                                                    [loc (get-arg-regs (length args) #t)])
                                                           `(,arg <- ,loc))
                                                         (compileL3 exp 'rax))]                                                
     [`(let ([,var ,d]) ,e) (append (compileL3 d var) (compileL3 e 'rax))]
     [`(if ,v ,e1 ,e2)
      (let ([l1 (create-label)]
            [l2 (create-label)])
        (append (list `(cjump ,(encode v) = 1 ,l2 ,l1) l1)
                (compileL3 e1 'rax)
                (cons l2 (compileL3 e2 'rax))))]
     [d (append
         (match d
          [(list (or '+ '- '* '< '<= '=) v1 v2) (c-biop2 (car d) v1 v2 store)]
          [(list (or 'number? 'a?) v) (c-pred (car exp) v store)]
          [`(new-array ,v1 ,v2) (list `(rdi <- ,(encode v1)) `(rsi <- ,(encode v2))
                                      `(call allocate 2)
                                      `(,store <- rax))]
          [`(new-tuple ,v ...) (make-tuple v store)]
          [`(aref ,v1 ,v2) (c-aref v1 v2 store)]
          [`(aset ,v1 ,v2 ,v3) (c-aset v1 v2 v3 store)]
          [`(alen ,v) (cons `(,store <- (mem ,v 0)) (encode-var store))]
          
          [`(print ,v) (list `(rdi <- ,(encode v)) `(call print 1) `(,store <- rax))]
          [`(make-closure ,label ,v) (compileL3 `(new-tuple ,label ,v) store)]
          [`(closure-proc ,v) (compileL3 `(aref ,v 0) store)]
          [`(closure-vars ,v) (compileL3 `(aref ,v 1) store)]
          [`(,v1 ,v2 ...) (c-fcall v1 v2 store)]
          [v (list `(,store <- ,(encode v)))])
         (if (equal? store 'rax)
             (list '(return))
             '()))]
     )
  )
  


(define (L3->L2 prog)
  (let ([compiled (map (lambda (x) (compileL3 x 'rax)) prog)])
    (string-append (format "(:maintroll (:maintroll 0 0 ")
                   (foldr  string-append "" (map (lambda (x) (format "~A" x)) (car compiled)))
                   ")"
                   (foldr string-append "" (map (lambda (x) (format "~A" x)) (cdr compiled)))
                   ")")))








(provide L3->L2)
                   

(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (lambda (x)
      (display (L3->L2 (read x)))
               )))

(L3->L2 '((if 1
   (let ((x 2)) (if x (let ((x 3)) (if x (print x) (print 11))) (print 12)))
   (print 13)))
)






                   