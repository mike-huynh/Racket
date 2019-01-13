; Mike Huynh  Problem Set 3  9/22/17

"Problem 1"
(define (harmonic n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (/ 1 n) (harmonic (- n 1))))))
"test"
"(harmonic 10)"
(harmonic 10) ; output 2 2341/2520
"(- (harmonic 1000) (log 1000))"
"output is .577 which is over half and a close estimate to Euler's constant" 
(- (harmonic 1000) (log 1000))
(newline)

"Problem 2"
(define (count-primes t)
  (define (prime? n)
    (define (divisor? k) (= 0 ( modulo n k )))
    (define ( divisors-upto k )
      (and (> k 1)
      (or (divisor? k) (divisors-upto (- k 1)))))
    (not ( divisors-upto (- n 1))))
  (cond ((= t 0) 0)
        (else (if (prime? t)
                  (+ 1 (count-primes (- t 1)))
                  (+ 0 (count-primes (- t 1)))))))
"test"
"(count-prime 11"
(count-primes 11) ; output 6
(newline)

"Problem 3a"
(define (lucas n)
  (cond ((<= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 0)
        (else (+ (lucas (- n 1)) (lucas (- n 2))))))
"test"
"(lucas 7)"
(lucas 7) ; output 5
(newline)

"Problem 3b"
(define (l-ratio n)
  (+ 0.0 (/ (lucas n) (lucas (- n 1)))))
(define (fibonacci-ratio n)
  (define (fibonacci n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          ((= n 2) 1)
          (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
  (+ 0.0 (/ (fibonacci n) (fibonacci (- n 1)))))
"test"
"fibonacci-ratio 20)"
"(l-ratio 20)"
(fibonacci-ratio 20) ; output 1.618
(l-ratio 20) ; output 1.618

"Both ratios are extremely similar to one another and as you increase n, the ratio is around 1.618"
(newline)

"Problem 3c"
; As you increase n, the 'lucas' function takes a much longer time for every iteration
(define (fast-lucas-help n k lucas-a lucas-b)
  (if (= n k)
      lucas-a
      (fast-lucas-help n (+ k 1) lucas-b (+ lucas-a lucas-b))))
(define (fast-lucas n) (fast-lucas-help n 0 2 1))
"test"
"(fast-lucas 50)"
(fast-lucas 50)

;;--------------------------------------

;;| k |   lucas   | fast-lucas-help    |

;;--------------------------------------

;;| 1 |     0     |         1          |

;;| 2 |     2     |         2          |

;;| 3 |     5     |                    |

;;| 4 |           |                    |

;;| 5 |           |                    |

;;| 6 |           |                    |

