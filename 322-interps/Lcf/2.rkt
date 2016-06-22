#lang racket

(require "liveness.rkt" "graph.rkt" "spill.rkt")

(define (get-vars-from-line line)
  (filter isVar3? (flatten line)))


(define (isVar3? x)
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
                 [_ (regexp-match #rx"^[a-zA-Z_][-a-zA-Z_0-9]*$" (symbol->string x))])
               ))))


(define (get-vars func)
  (if (empty? func) '()
      (remove-duplicates (append (get-vars-from-line (car func)) (get-vars (cdr func))))))

(define (replace-var line vars regs)
  (if (list? line) (map (lambda (x) (replace-var x vars regs)) line)
      (let ([i (vector-member line vars)])
        (if i
            (vector-ref regs i)
            line))))

(define (replace line spill_c)
  (match line
    [`(,w <- (stack-arg ,n)) (let ([n_new (+ (* 8 spill_c) n)])
                                `(,w <- (mem rsp ,n_new)))]
    [_ line]))
      

(define (spill vars func)
  (if (empty? vars)
      func
      (spill (cdr vars) (read (open-input-string (L2->L2Spill (list func (car vars) 's)))))))

  

(define (L2f->L1f func spilled?)
  (let ([vars (get-vars (cdddr func))]
        [coloring (L2Graph func)]
        [uptown-func (map (lambda (x) (replace x (caddr func))) (cdddr func))])
    (if coloring
        (if (not (empty? coloring))
          (if (not (= (length coloring) (length vars))) (list coloring vars)
              (append (list (first func) (second func) (third func))
                    (map (lambda (x) (replace-var x (list->vector (map car coloring))
                                            (list->vector (map cadr coloring))))
                         uptown-func)))
          (append (list (first func) (second func) (third func)) uptown-func))
        (if spilled? #f
            (let ([spilled-func (spill vars func)])
              (L2f->L1f spilled-func #t))))
    ))

(define (L2->L1 code)
  (let ([allocated-funcs (map (lambda (x) (L2f->L1f x #f)) (cdr code))])
    (if (member #f allocated-funcs )
        (display (format "\"could not register allocate ~A\"" (car code)))
        (append (list (first code)) allocated-funcs))
          ))






;(:main
;	(:main 0 0
;		((mem rsp -8) <- :ret)
;		((mem rsp -24) <- 5)
;		(call :nope 8)
;		(return))
;	(:nope 8 2
 ; 		((mem rsp 0) <- :label) ; possibilities: r10, r11, r8, r9, rax, rcx, rdi, rdx, rsi
;		(r8 <- (mem rsp 0))
 ;		(call r8 0)    ; kill: r10, r11, r8, r9, rax, rcx, rdi, rdx, rsi (REASSIGN)
 ;		(r8 += rax)
;		(r10 <- (mem rsp 24))
;		(r8 += r10)
;		((mem rsp 0) <- r8)
 ;		(rdi <- (mem rsp 0))
 ;		(call print 1)	
 ;		(return))) ; r10 r11 r8 r9 rax rcx rdi rdx rsi OUT; have to use another register but don't have one

(provide L2->L1)

