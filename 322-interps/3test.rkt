#lang racket
((:main)
 (:main ()
        (let ([x (+ 2 1)])
          x)))

(:main
 (:main 0 0
        (x <- 5)
        (x += 3)
        (x -= 1)
        (rax <- x)
        (return)))

((:main)
 (:main ()
        (let ([v 1])
          (let ([x 2])
            (let ([c (+ v x)])
              c)))))

(:main
 (:main 0 0
        (v <- 3)
        (x <- 5)
        (c <- v)
        (c += x)
        (c -= 1)
        (rax <- c)))

((:main)
 (:main ()
        (let ([x (<= 2 55)])
          (if x x 0))))

(:main
 (:main 0 0
        (x <- 2 <= 55)
        (cjump x = 3 :l1 :l2)
        :l1
        (rax <- x)
        :l2
        (rax <- 0)
        (return)))

((:main)
 (:main ()
        (let ([x (new-array 3 3)])
          (let ([a (a? x)])
            (if a 3 6)))))

(:main
 (:main 0 0
        (rdi <- 7)
        (rsi <- 7)
        (call allocate 2)
        (x <- rax)
        (a <- x)
        (a &= 1)
        (a *= -2)
        (a += 3)
        (cjump a = 3 :l1 :l2)
        :l1
        (rax <- 7)
        (return)
        :l2
        (rax <- 13)
        (return)))

((:main)
 (:main ()
        (let ([x (new-array 3 3)])
          (let ([a (alen x)])
            (let ([b (<= a 5)])
              b)))))

(:main
 (:main 0 0
        (rdi <- 3)
        (rsi <- 3)
        (call allocate 2)
        (x <- rax)
        (a <- (mem rax 1))
        (b <- a <= 5)
        (rax <- b)))


((:main)
 (:main ()
        (let ([a (a? 5)])
          (if a
              a
              (let ([b 4])
                (let ([c (+ b 3)])
                  c))
              ))))

(:main
 (:main 0 0
        (a <- 5)
        (a &= 1)
        (a *= -2)
        (a += 3)
        (cjump a = 3 :l1 :l2)
        :l1
        (rax <- a)
        (return)
        :l2
        (b <- 4)
        (c <- b)
        (c <- 3)
        (rax <- c)
        (return)))

((:main)
 (:main ()
        (let ([a 5])
          (let ([b 3])
            (let ([c (mult a b)])
              c))))
 (:mult (a b)
        (let ([c (* a b)])
          c)))

(:main
 (:main 0 0
        (a <- 11)
        (b <- 7)
        ((mem rsp -8) <- :mult_ret)
        (rdi <- a)
        (rsi <- b)
        (call :mult 2)
        :mult_ret
        (c <- rax)
        (rax <- c)
        (return))
 (:mult 2 0
        (a <- rdi)
        (b <- rsi)
        (c <- a)
        (c *= b)
        (rax <- c)
        (return))
 )

((:main)
 (:main ()
        (let ([a 7])
          (print a))))

(:main
 (:main 0 0
        (a <- 15)
        (rdi <- a)
        (call print 1)
        (return)))

((:main)
 (:main ()
  (let ([a (new-array 5 5)])
   (let ([p (print a)])
    (let ([new_a (:pop a)])
     (print new_a)))))
 (:pop (a)
  (let ([n (alen a)])
   (let ([n (- n 1)])
    (let ([m (aref a 0)])
     (new-array n m))))))

(:main
 (:main 0 0
        (rdi <- 11)
        (rsi <- 11)
        (call allocate 2)
        (a <- rax)
        (rdi <- rax)
        (call print 1)
        ((mem rsp -8) <- :pop_ret)
        (rdi <- a)
        (call :pop 1)
        :pop_ret
        (rdi <- rax)
        (call print 1)
        (return))
 (:pop 1 0
       (n <- (mem rdi 0))
       (n -= 1)
       (n *= 2)
       (n += 1)
       (rdi <- n)
       (rsi <- (mem rdi 8))
       (call allocate 2)
       (return)))


((:main)
 (:main ()
  (:f))
 (:f ()
  (let ([a 1])
   (let ([b (:g)])
    (let ([c a])
     (print c)))))
 (:g ()
  (:h))
 (:h ()
  0))

(:main
 (:main 0 0
        ((mem rsp -8) <- :f_ret)
        (call :f 0)
        :f_ret
        (return))
 (:f 0 1
     (a <- 3)
     ((mem rsp -8) <- :ret2)
     (call :g 0)
     :ret2
     (b <- rax)
     (c <- a)
     (rdi <- c)
     (call print 1)
     (return))
 (:g 0 0
     (tail-call :h 0))
 (:h 0 0
     (rax <- 0)
     (return)))




((:main)
 (:main ()
  (let ([a (:f 1 2 3 4 5 6 7 8)])
   (print a)))
 (:f (a b c d e f g h)
  g))


(:main
 (:main
  0
  0
  (rdi <- 1)
  (rsi <- 2)
  (rdx <- 3)
  (rcx <- 4)
  (r8 <- 5)
  (r9 <- 6)
  ((mem rsp -16) <- 7)
  ((mem rsp -24) <- 8)
  ((mem rsp -8) <- :f_ret)
  (call :f 8)
  :f_ret
  (rax *= 2)
  (rax += 1)
  (rdi <- rax)
  (call print 1)
  (return))
 (:f 8 0 (rax <- (stack-arg 8)) (return)))







