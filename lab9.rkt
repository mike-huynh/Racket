;; Mike Huynh  Lab 9  10/31/17

"Problem 1"
(define (num-occurs sym lst)
  (define (occur-help sym lst base)
    (cond ((null? lst) base)
          ((eq? sym (car lst)) (occur-help sym (cdr lst) (+ 1 base)))
          (else (occur-help sym (cdr lst) base))))
  (occur-help sym lst 0))

"Test"
"(num-occurs 'uh-huh '(thats the way uh-huh uh-huh i like it uh-huh uh-huh))"
(num-occurs 'uh-huh '(thats the way uh-huh uh-huh i like it uh-huh uh-huh))  ;; 4
"(num-occurs 'a '(a b c (not (c b a))))"
(num-occurs 'a '(a b c (not (c b a))))  ;; 1
(newline)

"Problem 2"
(define (freq-list lst)
  ;; remove all func from lab
  (define (remove-all x l)
      (if (null? l)
          l
          (if (equal? x (car l))
              (remove-all x (cdr l))
              (cons (car l) (remove-all x (cdr l))))))

  (if (null? lst)
      '()
      (cons (cons (car lst) (num-occurs (car lst) lst)) (freq-list (remove-all (car lst) lst)))))

"Test case from lab"
(freq-list '(thats the way uh-huh uh-huh i like it uh-huh uh-huh
             thats the way uh-huh uh-huh i like it uh-huh uh-huh
             thats the way uh-huh uh-huh i like it uh-huh uh-huh
             thats the way uh-huh uh-huh i like it uh-huh uh-huh))
(newline)

"Problem 3a"
(define (create-heap vw-pair left right)
  (list vw-pair left right))

"Problem 3b"
(define (h-min heap)
  (car heap))

(define (value heap)
  (car (h-min heap)))

(define (weight heap)
  (cdr (h-min heap)))

"Problem 3c"
(define (left heap)
  (cadr heap))

"Problem 3d"
(define (right heap)
  (caddr heap))

"Problem 3e"
(define (insert vw-pair heap)
  (if (null? heap)
      (create-heap vw-pair '() '())
      (let ((value (car (h-min heap)))
            (child (max (cdr vw-pair) (cdr (h-min heap))))
            (root (min (cdr vw-pair) (cdr (h-min heap)))))
        (if (= child (cdr vw-pair))
            (create-heap (cons value root)
                         (right heap)
                         (insert (cons (car vw-pair) child) (left heap)))
            (create-heap (cons (car vw-pair) root)
                         (right heap)
                         (insert (cons value child) (left heap)))))))

"Testcase from lab"
(insert '(ooma . 0) '((surfin-bird . 1)
                      ((your . 2) ((boat . 6) () ()) ((row . 3) () ()))
                      ((doo . 4) ((pa . 7) () ()) ((da . 5) () ()))))
(newline)

"Problem 3f"
(define (insert-list-of-pairs vw-pair-list heap)
  (if (null? vw-pair-list)
      heap
      (insert-list-of-pairs (cdr vw-pair-list) (insert (car vw-pair-list) heap))))

"Test case from lab"
"(insert-list-of-pairs '((pa . 7) (da . 4) (doo . 3) (boat . 6) (your . 2) (row . 9)) '((ooma . 10) () ()))"
(insert-list-of-pairs '((pa . 7) (da . 4) (doo . 3) (boat . 6) (your . 2) (row . 9)) '((ooma . 10) () ()))
(newline)

"Problem 3g"
(define (remove-min heap)
  ;; helper which removes the root and combines both the left and right heaps
  (define (new-heap heap1 heap2)
    
    (cond ((null? heap1) heap2)
          ((null? heap2) heap1)
          ((< (weight heap1) (weight heap2))
           (create-heap (h-min heap1)
                        heap2
                        (new-heap (left heap1) (right heap1))))
          (else (create-heap (h-min heap2)
                             heap1
                             (new-heap (left heap2) (right heap2))))))

  (new-heap (left heap) (right heap)))

"test"
"(remove-min '((surfin-bird . 1) ((your . 2) ((boat . 6) () ()) ((row . 3) () ())) ((doo . 4) ((pa . 7) () ()) ((da . 5) () ()))))"
(remove-min '((surfin-bird . 1) ((your . 2) ((boat . 6) () ()) ((row . 3) () ())) ((doo . 4) ((pa . 7) () ()) ((da . 5) () ()))))
(newline)

"Problem 4"
;(define (get-in-order heap)
(define (get-in-order heap)
  (cond ((null? heap) heap)
        ((null? (remove-min heap)) (list (h-min heap)))
        ((= (weight heap) (weight (remove-min heap))) (get-in-order (remove-min heap)))
        ((< (weight heap) (weight (remove-min heap))) (cons (h-min heap) (get-in-order (remove-min heap))))
        (else (cons (get-in-order (remove-min heap)) (h-min heap)))))


; heapsort taken from lab
(define (heapsort pair-list)
  (get-in-order (insert-list-of-pairs pair-list '())))

"Test"
"(freq-list '(row row your boat boat boat row your boat boat boat))"
(freq-list '(row row your boat boat boat row your boat boat boat))
"(heapsort (freq-list '(row row your boat boat boat row your boat boat boat)))"
(heapsort (freq-list '(row row your boat boat boat row your boat boat boat)))
