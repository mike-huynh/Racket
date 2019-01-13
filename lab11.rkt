;; Mike Huynh  Lab 11  11/14/17

"Problem 1a"
(define (fact n)
  (let ((sum 1)
        (count 1))
    (define (helper)
      (cond ((> count n) 'done)
            (else (set! sum (* sum count))
                  (set! count (+ count 1))
                  (helper))))
    (helper)
    sum))

"Test"
"(fact 5)"
(fact 5)
(newline)

"Problem 1b"
(define (hailstone n)
  (let ((lst '())
        (value 0))
    (define (helper)
      (cond ((= n 1) (set! lst (cons n lst)))
            ((even? n) (set! lst (cons n lst))
                       (set! value (/ n 2))
                       (set! n value)
                       (helper))
            (else (set! lst (cons n lst))
                  (set! value (+ (* 3 n) 1))
                  (set! n value)
                  (helper))))
    (helper)
    (reverse lst)))

"Test"
"(hailstone 3)"
(hailstone 3)
"(hailstone 1)"
(hailstone 1)
(newline)

"Problem 2"
(define (new-account initial-balance)
  (let ((balance initial-balance))
    (define (deposit f)
      (set! balance (+ balance f))
      balance)
    (define (withdraw f)
      (cond ((> f balance) (display "Insufficient funds. Current Balance: ") balance)
            (else (set! balance (- balance f))
                  (display "Balance: ") balance)))
    (define (bal-inq) balance)
    (define interest .01)
    (define (accrue)
        (set! balance (+ balance (* balance interest)))
      balance)
    (define (setrate f)
      (set! interest f))

    
    (lambda (method)
      (cond ((eq? method 'deposit) deposit)
            ((eq? method 'withdraw) withdraw)
            ((eq? method 'balance-inquire) bal-inq)
            ((eq? method 'accrue) accrue)
            ((eq? method 'setrate) setrate)
            (else 'undefined-operation)))))
(newline)

"Problem 3"
"(define a (new-account 500))"
(define a (new-account 500))
"((a 'deposit) 200)"
((a 'deposit) 200)
"((a 'withdraw) 200)"
((a 'withdraw) 200)
"((a 'balance-inquire))"
((a 'balance-inquire))
"((a 'withdraw) 600)"
((a 'withdraw) 600)
"((a 'accrue))"
((a 'accrue))
"((a 'setrate) 1)"
((a 'setrate) 1)
"((a 'accrue))"
((a 'accrue))
(newline)

"Problem 4a"
(define (make-stack)
  (let ((stack '()))
    (define (is-empty?)
      (null? stack))
    (define (push item)
      (set! stack (cons item stack)))
    (define (top)
      (car stack))
    (define (pop)
      (cond ((is-empty?) stack)
            (else (let ((first (top)))
                    (set! stack (cdr stack))
                    first))))
    (lambda (method)
      (cond ((eq? method 'is-empty) is-empty?)
            ((eq? method 'push) push)
            ((eq? method 'top) top)
            ((eq? method 'pop) pop)
            (else 'undefined-method)))))

"Test"
"(define my-stack (make-stack))"
(define my-stack (make-stack))
"((my-stack 'push) 'a)"
((my-stack 'push) 'a)
"((my-stack 'top))"
((my-stack 'top))
"((my-stack 'push) 'b)"
((my-stack 'push) 'b)
"((my-stack 'top))"
((my-stack 'top))
"((my-stack 'pop))"
((my-stack 'pop))
(newline)

"Problem 5a"
(define (nconc! x y)
  (let ((newlst '()))
    (cond ((null? x) (set! newlst y))
          ((null? y) (set! newlst x))
          ((null? (cdr x)) (set-cdr! x y)
                           (set! newlst (nconc! newlst x)))
          (else (nconc! (cdr x) y)
                (set! newlst x)))
    newlst)) 

(newline)

"5b Testing"
"(define a '(1 2 3))"
(define a '(1 2 3))
"(define b '(4 5 6))"
(define b '(4 5 6))
"(append a b)"
(append a b)
"a"
a
"b"
b
"(nconc! a b)"
(nconc! a b)
"a"
a
"b"
b
"(nconc! '(a b c) '())"
(nconc! '(a b c) '())
      