#lang racket

(define spill-var 'a)
(define prefix 's)
(define sv-counter -1)
(define spilled #f)
(define spill-loc 0)
(define (rsv x)
  (if (equal? x spill-var) (format "~A~A" prefix sv-counter) x))

(define (inc)
  (set! sv-counter (+ 1 sv-counter)))

(define (spill i)
  (match i
    [`(,w <- (mem ,x ,n))
     (cond
       [(and (equal? w spill-var) (equal? x spill-var)) (begin (inc) (format " (~A <- (mem rsp ~A)) (~A <- (mem ~A ~A)) ((mem rsp ~A) <- ~A)" (rsv w) spill-loc (rsv w) (rsv x) n spill-loc (rsv w)))]
       [(equal? w spill-var) (begin (inc)  (format " (~A <- (mem ~A ~A)) ((mem rsp ~A) <- ~A)" (rsv w) (rsv x) n spill-loc (rsv w)))]
       [(equal? x spill-var) (begin (inc) (format " (~A <- (mem rsp ~A)) (~A <- (mem ~A ~A))" (rsv x) spill-loc (rsv w) (rsv x) n))]
       [else (format " ~A" i)])]
    [`((mem ,x ,n) <- ,s)
     (cond
       [(equal? s spill-var) (begin (inc) (format " (~A <- (mem rsp ~A)) ((mem ~A ~A) <- ~A)" (rsv s) spill-loc (rsv x) n (rsv s)))]
       [(equal? x spill-var) (begin (inc) (format " (~A <- (mem rsp ~A)) ((mem ~A ~A) <- ~A)" (rsv x) spill-loc (rsv x) n s))]
       [else (format " ~A" i)])]
    [`(,w <- (stack-arg ,n))
     (cond
       [(equal? w spill-var) (begin (inc) (format " (~A <- (stack-arg ~A)) ((mem rsp ~A) <- ~A)" (rsv w) n spill-loc (rsv w)))]
       [else (format " ~A" i)])]
    [`(,w <- ,s)
     (cond
       [(and (equal? w s) (equal? w spill-var)) (format "")]
       [(equal? w spill-var)  (begin  (format " ((mem rsp ~A) <- ~A)" spill-loc s))]
       [(equal? s spill-var)  (begin  (format " (~A <- (mem rsp ~A))" w spill-loc))]
       [else (format " ~A" i)])]
    [`(call ,u ,n)
     (cond
       [(equal? u spill-var) (begin (inc) (format " (~A <- (mem rsp ~A)) (call ~A ~A)" (rsv u) spill-loc (rsv u) n))]
       [else (format " ~A"  i)])]
    [`(tail-call ,u ,n)
     (cond
       [(equal? u spill-var) (begin (inc) (format " (~A <- (mem rsp ~A)) (tail-call ~A ~A)" (rsv u) spill-loc (rsv u) n))]
       [else (format " ~A"  i)])]
    [`(,w ,op ,s) 
     (cond 
       [(and (equal? w s) (equal? w spill-var)) (begin (inc) (format " (~A <- (mem rsp ~A)) (~A ~A ~A) ((mem rsp ~A) <- ~A)" (rsv w) spill-loc (rsv w) op (rsv w) spill-loc (rsv w)))]
       [(equal? s spill-var) (begin (inc) (format " (~A <- (mem rsp ~A)) (~A ~A ~A)" (rsv s) spill-loc w op (rsv s) ))]
       [(equal? w spill-var) (begin (inc) (format " (~A <- (mem rsp ~A)) (~A ~A ~A)  ((mem rsp ~A) <- ~A)" (rsv w) spill-loc (rsv w) op s spill-loc (rsv w)))]
       [else (format " ~A" i)])]  
    [`(,w <- ,t1 ,cmp ,t2)
     (cond
       [(and (equal? w spill-var) (or (equal? t1 spill-var) (equal? t2 spill-var))) (begin (inc) (format " (~A <- (mem rsp ~A)) (~A <- ~A ~A ~A) ((mem rsp ~A) <- ~A)" (rsv w) spill-loc (rsv w) (rsv t1) cmp (rsv t2) spill-loc (rsv w)))]
       [(equal? w spill-var) (begin (inc) (format " (~A <- ~A ~A ~A) ((mem rsp ~A) <- ~A)" (rsv w) (rsv t1) cmp (rsv t2) spill-loc (rsv w)))]
       [(or (equal? t1 spill-var) (equal? t2 spill-var)) (begin (inc) (format " (~A <- (mem rsp ~A)) (~A <- ~A ~A ~A)" (rsv spill-var) spill-loc w (rsv t1) cmp (rsv t2)))]
       [else (format " ~A" i)])]
    [(? symbol?) (format " ~A" i)]
    [`(goto ,l) (format " (goto ~A)" l)]
    [`(cjump ,t1 ,cmp ,t2 ,l1 ,l2)
     (cond
       [(or (equal? t1 spill-var) (equal? t2 spill-var)) (begin (inc) (format " (~A <- (mem rsp ~A)) (cjump ~A ~A ~A ~A ~A)" (rsv spill-var) spill-loc (rsv t1) cmp (rsv t2) l1 l2))]
       [else (format " ~A" i)])]
    [`(return) " (return)"]
    ))


(define (L2->L2Spill code)
  (let ([fname (car (first code))]
        [regm (cadr (first code))]
        [spill-sp (caddr (first code))])
    (begin
      (set! spill-var (second code))
      (set! prefix (third code))
      (set! spill-loc (* 8 spill-sp))
      (let ([result (foldr string-append "" (map spill (cdddar code)))])
       (format "(~A ~A ~A~A)\n" fname regm  (+ 1 spill-sp) result)))))
  
    
(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (lambda (x) (display (L2->L2Spill (list (read x) (read x) (read x)))))))                                   
    
                                  
