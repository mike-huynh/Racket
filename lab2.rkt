; Mike Huynh     Lab 2    9/12/17

"Problem 1a."
(define (pi)
  (* 4 (atan 1)))
"test"
"(pi)"
(pi)
(newline)

"Problem 1b."
(define (square x)
  (* x x))
(define (area-of-circle r)
  (* (pi) (square r)))
"test"
"(area-of-circle 2)"
(area-of-circle 2)
(newline)

"Problem 1c."
(define (surface-area-of-sphere r)
  (* 4 (area-of-circle r)))
"test"
"(surface-area-of-sphere 3)"
(surface-area-of-sphere 3)
(newline)

"Problem 1d."
(define (volume-of-sphere r)
  (/ (* (surface-area-of-sphere r) r) 3))
"test"
"(volume-of-sphere 3)"
(volume-of-sphere 3)
(newline)

"Problem 2"
(define (s n)
  (cond ((= n 1) 1)
        ((= n 2) 2)
        ((= n 3) 3)
        (else (+ (- (s (- n 3)) (s (- n 2))) (s (- n 1))))))
"test"
"(s 10)"
(s 10) ; output 8
(newline)

"Problem 3"
(define (zeno n)
  (if (= n 0)
      0
      (+ (/ 1 (expt 2 n)) (zeno (- n 1)))))
"test"
"(zeno 3)"
(zeno 3) ; output 7/8
(newline)

"Problem 4a."
(define (even-nn-int? n)
  (cond ((= n 0) #t)
        ((= n 1) #f)
        (else (even-nn-int? (- n 2)))))
"test"
"(even-nn-int? 12)"
(even-nn-int? 12) ; output #t
(newline)

"Problem 4b."
(define (even-int? n)
  (cond ((< n 0) (even-nn-int? (abs n)))
        (else (even-nn-int? n))))
"test"
"(even-int? -50)"
(even-int? -50) ; output #t
(newline)

"Problem 4c."
(define (odd-int? n)
  (cond ((= n 0) #f)
        ((= n 1) #t)
        ((< n 0) (odd-int? (abs n)))
        (else (odd-int? (- n 2)))))
"test"
"(odd-int? -99)"
(odd-int? -99) ; output #t