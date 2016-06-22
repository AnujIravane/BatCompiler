#lang racket
(require rackunit racket/set)
;L3 compiler

(define bounds-count 0)
(define if-counter 0)
(define arg-registers '(ecx edx eax))

(define (L3->L2 prog)
  (append (list '((call :main123)))
          (list (cons ':main123 
                      (compile-e (first prog))))
          (map compile-L3f (rest prog))))

(define (compile-L3f fn)
  (let ([name (first fn)]
        [arglist (second fn)]
        [e (third fn)])
    (append (list name)
            (for/list ((arg arglist)
                       (i (in-naturals)))
              `(,arg <- ,(list-ref arg-registers i)))
            (compile-e e))))

(define/contract (list-contains? sym lst)
  (-> symbol? list? boolean?)
  (cond [(empty? lst) #f]
        [(list? (car lst)) (or (list-contains? sym (car lst))
                               (list-contains? sym (cdr lst)))]
        [(symbol? (car lst)) (or (symbol=? (car lst) sym)
                                 (list-contains? sym (cdr lst)))]
        [else #f]))

(define (compile-e e)
   (compile-e-int e))

(define (compile-e-int e)
  (match e 
    [`(let ([,x ,d]) ,e1) (compile-let x d e1)]
    [`(if ,test ,then ,else) (compile-if test then else)]
    [d (let ([d-code (compile-d d 'eax #t)])
                      (if (list-contains? 'tail-call d-code)
                          d-code
                          (append d-code `((return)))))]))
  

(define (compile-d d dest tail?)
  (match d
    [`(aref ,a ,off) (aref a off dest) ]
    [`(aset ,a ,off ,new) (aset a off new dest) ]
    [`(alen ,a) `((,dest <- (mem ,a 0))
                  (,dest <<= 1)
                  (,dest += 1))]
    [`(new-array ,size ,init) `((eax <- (allocate ,(encode size) ,(encode init)))
                                (,dest <- eax))]
    [`(new-tuple ,vs ...) (init-tuple vs dest)]
    ;biops
    [`(+ ,v1 ,v2) (compile-add/sub dest v1 v2 '+=)]
    [`(- ,v1 ,v2) (compile-add/sub dest v1 v2 '-=)]
    [`(* ,v1 ,v2) `((yoloswaggins <- ,(encode v1))
                    (yoloswaggins >>= 1)
                    (,dest <- ,(encode v2))
                    (,dest >>= 1)
                    (,dest *= yoloswaggins)
                    (,dest *= 2)
                    (,dest += 1))]
    [`(< ,v1 ,v2) (compile-comp '< v1 v2 dest)]
    [`(<= ,v1 ,v2) (compile-comp '<= v1 v2 dest)]
    [`(= ,v1 ,v2) (compile-comp '= v1 v2 dest)]
    ;preds
    [`(number? ,v) (compile-huh 'number? v dest)]
    [`(a? ,v) (compile-huh 'a? v dest)]
    [`(print ,v) `((eax <- (print ,(encode v))))]
    [`(make-closure ,label ,v) (compile-d `(new-tuple ,label ,v) dest tail?)]
    [`(closure-proc ,v) (compile-d `(aref ,v 0) dest tail?)]
    [`(closure-vars ,v) (compile-d `(aref ,v 1) dest tail?)]
    [`(,fn) (compile-call fn '() tail? dest) ]
    [`(,fn ,arglist ...) (compile-call fn arglist tail? dest)]
    [v `((,dest <- ,(encode v)))]));check for labels

(define (tmp)
    (string->symbol
     (string-append "__x_"
                    (number->string (random 1000)))))

(define (compile-add/sub dest v1 v2 op)
  (let* ([temp (tmp)]
         (store-instr `(,temp <- ,dest))
         (not-op (if (symbol=? op '+=)
                     '-=
                     '+=))
         (newv1  (encode v1))
         (newv2 (if (and (not (number? v2)) (symbol=? v2 dest))
                    temp
                    (encode v2))))
    (cond [(equal? v1 dest)
           `((,dest ,op ,newv2)
             (,dest ,not-op 1))]
          [(equal? temp newv2)
           `(,store-instr
             (,dest <- ,newv1)
             (,dest ,op ,newv2)
             (,dest ,not-op 1))]
          [else 
           `((,dest <- ,newv1)
             (,dest ,op ,newv2)
             (,dest ,not-op 1))])))

(define (compile-call fn arglist tail? dest)
  (append (for/list [(arg arglist)
                     (i (in-naturals))]
            `(,(list-ref arg-registers i) <- ,(encode arg)))
          (if tail?
              `((tail-call ,fn))
              `((call ,fn) (,dest <- eax)))))
           

(define (compile-comp op v1 v2 dest)
 `((,dest <- ,(encode v1) ,op ,(encode v2))
   (,dest <<= 1)
   (,dest += 1)))

(define (compile-huh huh v1 dest)
  (append `((,dest <- ,(encode v1))
            (,dest &= 1))
          (cond [(symbol=? huh 'a?)
                 `((,dest *= -2)
                   (,dest += 3))]
                [else 
                 `((,dest *= 2)
                   (,dest += 1))])))
                
(define (aset array pos new dest)
  (let ([pass (new-bounds-label bounds-count "pass")]
        [fail (new-bounds-label bounds-count "fail")]
        [bounds-temp (tmp)])
    (set! bounds-count (add1 bounds-count))
    `((,dest <- ,(encode pos))
      (,dest >>= 1)
      (,bounds-temp <- (mem ,array 0))
      (cjump ,dest < ,bounds-temp ,pass ,fail)
      ,fail
      (eax <- (array-error ,array ,(encode pos)))
      ,pass
      (,dest *= 4)
      (,dest += ,array)
      ((mem ,dest 4) <- ,(encode new))
      (,dest <- 1))))

(define (aref array pos dest)
  (let ([pass (new-bounds-label bounds-count "pass")]
        [fail (new-bounds-label bounds-count "fail")])
    (set! bounds-count (add1 bounds-count))
    `((,dest <- ,(encode pos))
      (,dest >>= 1)
      (bounds-temp <- (mem ,array 0))
      (cjump ,dest < bounds-temp ,pass ,fail)
      ,fail
      (eax <- (array-error ,array ,(encode pos)))
      ,pass
      (,dest *= 4)
      (,dest += ,array)
      (,dest <- (mem ,dest 4)))))                  

(define (compile-let dest dexpr e)
  (let* ((newIC (compile-e-int e))
         (instrs newIC))
     (append (compile-d dexpr dest #f) 
             instrs)))


(define (compile-if test then else)
  (set! if-counter (+ 1 if-counter))
  (let* ([else-lab (new-if-label "else")]
         [then-lab (new-if-label "then")]
         [compiled-then (compile-e-int then)]
         [compiled-else (compile-e-int else)])
  (append `((cjump ,(encode test) = 1 ,else-lab ,then-lab))
                         (list then-lab)
                         compiled-then
                         (list else-lab)
                         compiled-else)
                 ))


(define (new-if-label then/else)
  (string->symbol
   (string-append ":magic_if_label_"
                  (number->string if-counter)
                  then/else)))

(define (new-bounds-label count pass/fail)
  (string->symbol
   (string-append ":magic_bounds_label_"
                  (number->string count)
                  pass/fail)))


(define (init-tuple values dest)
  (append `((eax <- (allocate ,(encode (length values)) 0)))
          (for/list ((v values)
                     (i (in-naturals)))
          `((mem eax ,(* 4 (+ 1 i))) <- ,(encode v)))
          `((,dest <- eax)))) 

(define/contract (encode x)
  (-> (or/c number? symbol?)
      (or/c number? symbol?))
  (cond 
    ((number? x) (+ 1 (* 2 x)))
    ((symbol? x) x)))



(provide L3->L2)
              
                     
              
                     
               
    