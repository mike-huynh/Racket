;Mike Huynh 9/15/17 Problem Set 2

"Problem 1a."
(define (odd-sum n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (- (* n 2) 1) (odd-sum (- n 1))))))
"test"
"(odd-sum 4)"
(odd-sum 4) ; output 16
(newline)

"Problem 1b."
"(odd-sum 1)"
(odd-sum 1) ; 1
"(odd-sum 2)"
(odd-sum 2) ; 4
"(odd-sum 3)"
(odd-sum 3) ; 9
"(odd-sum 4)"
(odd-sum 4) ; 16
"(odd-sum 5)"
(odd-sum 5) ; 25
"(odd-sum 6)"
(odd-sum 6) ; 36
"(odd-sum 7)"
(odd-sum 7) ; 49

"This function is exactly like a square function. This makes sense because this is another method to find a square of a number if you don't square"
(newline)

"Problem 1c."
(define (sum-from-to a b)
  (cond ((> a b) 0)
        (else (+ a (sum-from-to (+ a 1) b)))))
"test"
"(sum-from-to 3 5)"
(sum-from-to 3 5) ; output 12
(newline)

"Problem 2"
(define (k-product n)
  (cond ((= n 0) #f)
        ((= n 1) 0)
        ((= n 2) 1/2)
        (else (* (- 1 (/ 1 n)) (k-product (- n 1))))))
"test"
"(k-product 5)"
(k-product 5) ; output 1/5
(newline)

"Problem 3a."
(define (finite-sum-of-powers z k)
  (if (<= 0 z)      
      (if (< z 1)
          (cond ((= k 1) z)
                (else (+ (expt z k) (finite-sum-of-powers z (- k 1)))))
          #f)                
      #f))
"test"
"(finite-sum-of-powers .25 99)"
(finite-sum-of-powers .25 99) ; output .3333
(newline)

"Problem 3b."
(define (terms-needed z tol)
  (define (first-value-k-or-higher z tol k)
    (cond ((< (- (/ z (- 1 z)) (finite-sum-of-powers z k)) tol) k)
          (else (first-value-k-or-higher z tol (+ k 1)))))
  (if (<= 0 z)      
      (if (< z 1)
          (first-value-k-or-higher z tol 1)              
          #f)
      #f))
"test"
"(terms-needed .5 .0001)"
(terms-needed .5 .0001) ; output 14  
(newline)

"Problem 3c."
"The values of k goes up exponentially as you increase z. This approximation works best as you increase z because the output will come very close to the approximation. But this also is bad because as you can increase z, this function would take longer to calculate k."
(newline)

"Problem 4"
(define (new-sin x n)
  
  (define (factorial x)    
    (cond ((= x 0) 0)         
          ((= x 1) 1)
          (else (* x (factorial (- x 1))))))
  
  (cond ((= n 0) x)
        (else (+ (* (expt -1 n) (/ (expt x (+ (* 2 n) 1)) (factorial (+ (* 2 n) 1)))) (new-sin x (- n 1))))))
"test"
"(new-sin 1 3)"
(new-sin 3 3) ; output 4241/5040