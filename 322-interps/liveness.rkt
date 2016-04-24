#lang racket

(define caller-save-reg '(r8 r9 r10 r11 rax rcx rdi rdx rsi))
(define callee-save-reg '(r12 r13 r14 r15 rbp rbx))
(define args '(rdi rsi rdx rcx r8 r9))
(define result 'rax)

(define all-regs '(rax rbx rbp r10 r11 r12 r13 r14 r15 rdi rsi rdx rcx r8 r9))

(define (isReg? x)
  (member x all-regs))
(define (isVar? x)
  (if (number? x) #f
      (if (or (equal? x 'print) (equal? x 'allocate) (equal? x 'allocate-error) (equal? x 'rsp)) #f
          (regexp-match #rx"^[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string x)))))
(define (reg-or-var? x)
  (if (or (isReg? x) (isVar? x))
      (list x)
      '()))

(define prog '())
(define prog-vec #())
(define gen-set #())
(define kill-set #())
(define in-set #())
(define out-set #())
(define succ-set #())

(define (find-label l)
  (vector-member l prog-vec))
(define (get-args n)
  (if (> n (length args))
      args
      (drop-right args (- (length args) n))))
(define (get-final-in-set)
  (append (list 'in) (map (lambda (x) (sort x symbol<?)) (vector->list in-set))))
(define (get-final-out-set)
  (append (list 'out) (map (lambda (x) (sort x symbol<?)) (vector->list out-set))))
  

(define (gen-kill i)
  (match i
    [`(,w <- (mem ,x ,n)) (cons (reg-or-var? x) (reg-or-var? w))]
    [`((mem ,x ,n) <- ,s) (cons (append (reg-or-var? s)(reg-or-var? x)) '())]
    [`(,w <- (stack-arg ,n)) (cons '() (reg-or-var? w))]
    [`(,w <- ,s) (cons(reg-or-var? s)(reg-or-var? w))]
    [`(call ,u ,n) (cons (let ([u1 (reg-or-var? u)] [n-args (get-args n)])(if (not (equal? '() u1)) (append u1 n-args) n-args))(cons result caller-save-reg))]
    [`(tail-call ,u ,n)
     (cons
      (let ([u1 (reg-or-var? u)] [n-args (get-args n)])
        (if (not (equal? '() u1))
            (append u1 n-args callee-save-reg)
            (append n-args callee-save-reg)))
      '())]
    [`(,w ,op ,s) (cons (append (reg-or-var? s) (reg-or-var? w)) (reg-or-var? w))]    
    [`(,w <- ,t1 ,cmp ,t2) (cons (append (reg-or-var? t1)  (reg-or-var? t2)) (reg-or-var? w))]
    [(? symbol?) (cons '() '())]
    [`(goto ,l) (cons '() '())]
    [`(cjump ,t1 ,cmp ,t2 ,l1 ,l2) (cons (append (reg-or-var? t1) (reg-or-var? t2)) '())]
    [`(return) (cons (append (list result) callee-save-reg)'())]
    [_ "bug"]))

(define (create-successors)
  (begin
    (set! succ-set (make-vector (vector-length prog-vec) '()))
    (for ([i (in-range (vector-length prog-vec))]
          [instr prog-vec])
      (match instr
        [`(cjump ,t1 ,cmp ,t2 ,l1 ,l2) ;(if (= i (- (vector-length prog-vec) 1))
                                           (vector-set! succ-set i (list (find-label l1) (find-label l2)))]
                                           ;(vector-set! succ-set i (list (+ i 1) (find-label l1) (find-label l2))))]
        [`(goto ,l) ;(if (= i (- (vector-length prog-vec) 1))
                        (vector-set! succ-set i (list (find-label l)))]
                        ;(vector-set! succ-set i (list (+ i 1) (find-label l))))]
        [`(return) (vector-set! succ-set i '())]
        [`(call array-error ,n) (vector-set! succ-set i '())]
        [`(tail-call ,u ,n) (vector-set! succ-set i '())]
        [_ (if (= i (- (vector-length prog-vec) 1))
                            (vector-set! succ-set i '())
                            (vector-set! succ-set i (list (+ i 1))))]))))

(define (update-in-sets i)
  (let ([gen (vector-ref gen-set i)]
        [kill (vector-ref kill-set i)]
        [succ (vector-ref succ-set i)]
        [in (vector-ref in-set i)]
        [out (vector-ref out-set i)])
      (vector-set! in-set i (set-union gen (set-subtract out kill)))
    ))

(define (update-out-sets i)
  (let ([gen (vector-ref gen-set i)]
        [kill (vector-ref kill-set i)]
        [succ (vector-ref succ-set i)]
        [in (vector-ref in-set i)]
        [out (vector-ref out-set i)])
    (begin
     ; (vector-set! in-set i (set-union gen (set-subtract out kill)))
      (vector-set! out-set i (set-union out (foldr append '() (map (lambda (x) (vector-ref in-set x)) succ)))))))


(define (in-out in out ln)
  (do ([temp-in (make-vector ln #f)]
       [temp-out (make-vector ln #f)])
    ((and (equal? temp-in in-set) (equal? temp-out out-set)) (list (get-final-in-set) (get-final-out-set)))
    (begin
      (vector-copy! temp-in 0 in-set)
      (vector-copy! temp-out 0 out-set)
      (for ([i (in-range (vector-length in-set))])
        (update-in-sets i))
      (for ([i (in-range (vector-length in-set))])
        (update-out-sets i)))))
    
        
 
(define (main x)
  (begin
    (set! prog-vec (list->vector (cdddr x)))
    (set! gen-set (make-vector (vector-length prog-vec) '()))
    (set! kill-set (make-vector (vector-length prog-vec) '()))
    (set! in-set (make-vector (vector-length prog-vec) '()))
    (set! out-set (make-vector (vector-length prog-vec) '()))
    (for ([instr prog-vec]
          [i (in-range (vector-length prog-vec))])
      (let ([gen-kill-val (gen-kill instr)])
        (begin
          (vector-set! gen-set i (remove-duplicates (car gen-kill-val)))
          (vector-set! kill-set i (remove-duplicates (cdr gen-kill-val))))))
    ;;(display gen-set)
    (create-successors)
    (in-out in-set out-set(vector-length prog-vec))))

(define (liveness-main x)
  (begin
    (set! prog-vec (list->vector (cdddr x)))
    (set! gen-set (make-vector (vector-length prog-vec) '()))
    (set! kill-set (make-vector (vector-length prog-vec) '()))
    (set! in-set (make-vector (vector-length prog-vec) '()))
    (set! out-set (make-vector (vector-length prog-vec) '()))
    (for ([instr prog-vec]
          [i (in-range (vector-length prog-vec))])
      (let ([gen-kill-val (gen-kill instr)])
        (begin
          (vector-set! gen-set i (remove-duplicates (car gen-kill-val)))
          (vector-set! kill-set i (remove-duplicates (cdr gen-kill-val))))))
    ;;(display gen-set)
    (create-successors)
    (cons (vector->list kill-set) (in-out in-set out-set(vector-length prog-vec)))))
  
  
;(when (= (vector-length (current-command-line-arguments)) 1)
 ; (call-with-input-file
  ;    (vector-ref (current-command-line-arguments) 0)
   ; (lambda (x)
    ;  (display (liveness-main (read x))
     ;          ))))

(provide liveness-main all-regs isReg? isVar?)

; (main '(:go
 ; 0 0
  ;(rdi <- 5)
  ;(rsi <- 7)
  ;(rdi += rsi)
  ;(rdi -= 1)
  ;(return)))


        
