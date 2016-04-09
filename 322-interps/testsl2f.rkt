#lang racket

(:f 0 0 (x <- 1) (rax += x)) x s

(:f 0 0 (a <- 3) (rax <- a)) a s

(:f 0 0 (a <- 5) (a <- a < a)) a s

(:f 0 1 (a <- :jump) ((mem rsp 0) <- a) (a <- 5)) a s

(:f 0 0 (a <- 9) (a <- 7) (a <- 5) (rdi += a) (rbx += a)) a s

(:f 0 0 (a <- 7) (a += 5) (a -= 3) (rax <- a)) a s

(:f 0 0 (a <- 5) (a <- a) (a += 3)) a s

(:f 0 0 (a <- 5) (a += a) (rax <- a)) a s

(:f 0 0 (a <- 3) (rax <- a) (rax *= a) (a <- rax)) a s

(:f 0 1 (a <- 3) ((mem rsp 0) <- 1) (s <- (mem rsp 0)) (a <- s)) a s

(:f 0 1 (a <- :x) (call a 0)) a s

(:f 0 0 (a <- 7) (b <- 7) (cjump a = b :t :f) :t (a += b) :f (a -= b)) a s

(:f 0 0 (a <- 5) (b <- 1) (b <- a < b)) a s

(:f 0 0 (a <- rdi) (a *= a) (a *= a)) a s

(:f 0 0 (a <- a) (s <- s) (a <- 3) (a += 2))


