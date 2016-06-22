#lang racket

(require "L5c.rkt" "L4c.rkt" "L3c.rkt" "2.rkt" "compiler.rkt")

(when (= (vector-length (current-command-line-arguments)) 1)
  (call-with-input-file
      (vector-ref (current-command-line-arguments) 0)
    (lambda (x)
      (display (L1->Assembly
                (L2->L1 (read (open-input-string
                 (L3->L2
                                (L4->L3
                                 (L5->L4 (read x))))))

               
               ))))))
;))
               ;(L5->L4 (read x)))
 ;              )))




