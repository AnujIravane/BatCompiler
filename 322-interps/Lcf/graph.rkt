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




(define (check-graph-key x graph)
  (when (and (not (hash-has-key? graph x)) (isVar2? x)) (hash-set! graph x (set))
             ))

(define (isVar2? x)
   (if (number? x) #f
       (if (isReg? x) #f
           (if (or (equal? x 'print)
                   (equal? x 'call)
                   (equal? x 'tail-call)
                   (equal? x 'return)
                   (equal? x 'read)
                   (equal? x 'mem)
                   (equal? x 'cjump)
                   (equal? x 'goto)
                   (equal? x 'allocate)
                   (equal? x 'array-error)
                   (equal? x 'stack-arg)
                   (equal? x 'rsp)) #f
               (match x
                 [`(mem ,a ,b) #f]
                 [`(return) #f]
                 [_  (regexp-match #rx"^[a-zA-Z_][-a-zA-Z_0-9]*$" (symbol->string x))]) ;;really poor way of handling test cases
               ))))
  
(define (add-edge x y graph)
  (unless (equal? x y)
    (check-graph-key x graph)
    (check-graph-key y graph)
    (let ([x_neighbors (hash-ref graph x)]
          [y_neighbors (hash-ref graph y)])
      (hash-set! graph x (set-add x_neighbors y))
      (hash-set! graph y (set-add y_neighbors x)))))


(define (live-recur out-list w s graph)
  (if (empty? out-list) #t
      (begin
        (for-each (lambda (x) (check-graph-key x graph) (check-graph-key (car out-list) graph)
                    (unless (equal?
                             (set (car out-list) x)
                             (set w s))
                      (add-edge (car out-list) x graph))) out-list)
        (live-recur (cdr out-list) w s graph))))

      

(define (find-interference instr kill in out i w s graph)
  (when (= i 0)
    (live-recur in w s graph))
  (live-recur (append kill out) w s graph))

  
  

(define (make-graph func graph)
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
        (for-each (lambda (x) (check-graph-key x graph)) in)
        (match instr
          [`(,w <- ,s) (find-interference instr kill in out i w s graph)]
          [`(,w <<= ,s) (for-each (lambda (x) (when (isVar2? s) (add-edge s x graph))) (set->list (set-remove all-regs 'rcx)))
                        (find-interference instr kill in out i #f #f graph)]
          [`(,w >>= ,s) (for-each (lambda (x) (when (isVar2? s) (add-edge s x graph))) (set->list (set-remove all-regs 'rcx)))
                        (find-interference instr kill in out i #f #f graph)]
          [_ (find-interference instr kill in out i #f #f graph)])))))

(define (isTrue? x)
  (if x #t #f))

(define (color-recur vars coloring graph)
  (if (empty? vars) coloring
      (let ([free-regs (sort (set-subtract all-regs
                                           (map (lambda (x) (if (set-member? (hash-ref graph (car x)) (car vars)) (cadr x) #f)) coloring)
                                           (set->list (hash-ref graph (car vars)))) symbol<?)])
        (if (empty? free-regs) #f
            (color-recur (cdr vars) (append coloring (list (list (car vars)
                                                                 (car free-regs)))) graph)))))


(define (hash-length>? x y graph)
  (if (> (set-count (hash-ref graph x)) (set-count (hash-ref graph y))) #t #f))

(define (color-graph graph)
  (let ([vars (sort (filter isVar2? (hash-keys graph)) (lambda (x y) (hash-length>? x y graph)))])
    (color-recur vars '() graph)))

(define (mod<? x y)
  (if (symbol<? (car x) (car y))
      x
      y))
   

(define (graph->list graph)
   (sort (map (lambda (x) (append (list (car x)) (sort (set->list (cdr x)) symbol<?))) (hash->list graph)) #:key car symbol<?) )
 
(define (L2Graph func)
  (define graph (hash-copy base-graph))
  (make-graph func graph)
;(display (graph->list graph))
  (let ([colored-graph (color-graph graph)])
    (if colored-graph (sort colored-graph #:key car symbol<?) #f)))




           
;(when (= (vector-length (current-command-line-arguments)) 1)
 ; (call-with-input-file
  ;    (vector-ref (current-command-line-arguments) 0)
   ; (lambda (x)
    ;  (L2Graph (read x))
     ;          )))

(provide L2Graph)