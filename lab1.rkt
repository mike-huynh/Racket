"Lab 1 9/5/17"
(display "\n")

"Problem 1a."
(define (usd-gbp x)
  (* x 0.772))
"(usd-gbp 175)"
(usd-gbp 175)
(display "\n")

"Problem 1b."
(define (gbp-eur x)
  (* x 1.092))
"(gbp-eur 20)"
(gbp-eur 20)
(display "\n")

"Problem 1c."
(define (eur-sek x)
  (* x 9.485))
"(eur-sek 240)"
(eur-sek 240)
(display "\n")

"Problem 1d."
(define (usd-sek x)
  (* (usd-gbp x)
  (gbp-eur x)
  (eur-sek x)))
"(usd-sek 1)"
(usd-sek 1)
(display "\n")

"Problem 2a."
(define (det2x2 a b c d)
  (- (* a d) (* b c)))
"(det2x2 -3 1 2 7)"
(det2x2 -3 1 2 7)
(display "\n")

"Problem 2b."
(define (invertible? a b c d)
  (not(= 0 (det2x2 a b c d))))
"(invertible? (det2x2 -3 1 2 7))"
(invertible? -3 1 2 7)
"(invertible? (det2x2 2 -4 -6 12))"
(invertible?  2 -4 -6 12)
(display "\n")

"Problem 2c."
(define (prod-inv-direct? a b c d e f g h)
  (invertible? (+ (* a e) (* b g)) (+ (* a f) (* b h)) (+ (* c e) (* d g)) (+ (* c f) (* d h))))
"(prod-inv-direct? 1 2 3 4 1 2 3 4)"
(prod-inv-direct? 1 2 3 4 1 2 3 4)

(define (prod-inv-indirect? a b c d e f g h)
  (invertible? (* (det2x2 a b c d) (det2x2 e f g h))))
"(prod-inv-indirect? 1 0 3 0 0 2 3 0)"
(prod-inv-indirect? 1 0 3 0 0 2 3 0)
(display "\n")

"Problem 2d."
(define (det3x3 a b c
                d e f
                g h i)
  (+ (- (* a (det2x2 e f h i)) (* b (det2x2 d f g i))) (* c (det2x2 d e g h))))
"(det3x3 0 5 -6 8 -11 4 5 1 1)"
(det3x3 0 5 -6
        8 -11 4
        5 1 1)