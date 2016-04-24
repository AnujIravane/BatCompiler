#lang racket
(require "liveness.rkt")

(define base-graph
  (hash 'r10 (set 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'r11 (set 'r10 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'r12 (set 'r10 'r11 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'r13 (set 'r10 'r11 'r12 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'r14 (set 'r10 'r11 'r12 'r13 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'r15 (set 'r10 'r11 'r12 'r13 'r14 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'r8 (set 'r10 'r11 'r12 'r13 'r14 'r15 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'r9 (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'rax 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'rax (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rbp 'rbx 'rcx 'rdi 'rdx 'rsi)
        'rbp (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbx 'rcx 'rdi 'rdx 'rsi)
        'rbx (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rcx 'rdi 'rdx 'rsi)
        'rcx (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rdi 'rdx 'rsi)
        'rdi (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdx 'rsi)
        'rdx (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rsi)
        'rsi (set 'r10 'r11 'r12 'r13 'r14 'r15 'r8 'r9 'rax 'rbp 'rbx 'rcx 'rdi 'rdx)))

(define graph (hash-copy base-graph))
(define var-hash (make-hash))

(define (check-graph-key x)
  (when (not (hash-has-key? graph x)) (hash-set! graph x (set))
             ))

(define (isVar2? x)
   (if (number? x) #f
       (if (isReg? x) #f
           (if (or (equal? x 'print) (equal? x 'allocate) (equal? x 'allocate-error) (equal? x 'rsp)) #f
               (regexp-match #rx"^[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string x))))))
  
(define (add-edge x y)
  (unless (equal? x y)
    (check-graph-key x)
    (check-graph-key y)
    (let ([x_neighbors (hash-ref graph x)]
          [y_neighbors (hash-ref graph y)])
      (hash-set! graph x (set-add x_neighbors y))
      (hash-set! graph y (set-add y_neighbors x)))))


  

(define (live-recur out-list)
  (if (empty? out-list) #t
      (begin
        (for-each (lambda (x) (add-edge (car out-list) x)) (cdr out-list))
        (live-recur (cdr out-list)))))

      

(define (find-interference instr kill in out i)
  (if (= i 0)
      (live-recur in)
      (live-recur (append kill out))))

  
  

(define (make-graph func)
  (let ([result (liveness-main func)]
        [prog (cdddr func)])
    (let ([kill-set (first result)]
          [in-set (cdr (second result))]
          [out-set (cdr (third result))])
      (for ([instr prog]
            [kill kill-set]
            [in in-set]
            [out out-set]
            [i (in-range (length prog))])
        (match instr
          [`(,w <- ,s) (unless (and (isVar2? w) (isVar2? s))
                           (find-interference instr kill in out i))]
          [_ (find-interference instr kill in out i)])))))

(define (color-recur vars coloring graph)
  (if (empty? vars) coloring
      (let ([free-regs (sort (set-subtract all-regs
                                           (map (lambda (x) (cadr x)) coloring)
                                           (set->list (hash-ref graph (car vars)))) symbol<?)])
        (if (empty? free-regs) #f
            (color-recur (cdr vars) (append coloring (list (list (car vars)
                                                                 (car free-regs)))) graph)))))

(define (color-graph graph)
  (let ([vars (filter isVar2? (hash-keys graph))])
    (color-recur vars '() graph)))

(define (mod<? x y)
  (if (symbol<? (car x) (car y))
      x
      y))
   

(define (graph->list graph)
   (sort (map (lambda (x) (list (car x) (sort (set->list (cdr x)) symbol<?))) (hash->list graph)) #:key car symbol<?) )
 
(define (L2Graph func)
  (make-graph func)
  (list (graph->list graph)
        (color-graph graph)))



           
(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (lambda (x)
      (display (L2Graph (read x))
               ))))
                       
  
(L2Graph '(:f 0 0 (x <- 1) (rax += x) (return)))
   