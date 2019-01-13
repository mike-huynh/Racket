;; Mike Huynh  Lab 8  10/24/17

"Problem 1a"
;; Merge function taken from lab
(define (merge la lb)
   (cond ((null? la) lb)
         ((null? lb) la)
         ((< (car la) (car lb))
          (cons (car la)(merge (cdr la) lb)))
         (else
          (cons (car lb)(merge la (cdr lb))))))

;; Split function
(define (split l)
  ;; helper function
  (define (split-help lst1 lst2 n)
    (if (= n 0)
        (list (reverse lst2) lst1)
        (if (null? lst1)
            lst1
            (split-help (cdr lst1) (cons (car lst1) lst2) (- n 1))))) 

  (split-help l '() (floor (/ (length l) 2))))
  
"Test"
"(split '(1 2 3 4 5 6 7 8))"
(split '(1 2 3 4 5 6 7 8))  ;; output ((1 2 3 4) (5 6 7 8))
(newline)

"Problem 1b"
;; Mergesort function
(define (mergesort l)
  (if (null? l)
      l
      (if (null? (cdr l))
          l
	  (merge (mergesort (car (split l)))
                 (mergesort (cadr (split l)))))))

"Test"
"(mergesort '(10 5 9 1 8))"
(mergesort '(10 5 9 1 8))  ;; output (1 5 8 9 10)
(newline)

"Problem 2a"
(define (make-tree value left right)
  ;; produces a tree with value at the root , and
  ;; left and right as its subtrees .
  (list value left right))

(define (value tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (tree-node-count t)
  (cond ((null? t) 0)
        (else (+ 1 (tree-node-count (left t))
                 (tree-node-count (right t))))))

(define testtree (make-tree 1
                            (make-tree 3
                                       (make-tree 7 '() '())
                                       (make-tree 9 '() '()))
                            (make-tree 5 '() '())))

"Test"
"(tree-node-count testtree)"
(tree-node-count testtree)  ;; output 5
(newline)

"Problem 2b"
(define (tree-node-sum t)
  (cond ((null? t) 0)
        (else (+ (value t) (tree-node-sum (left t))
                 (tree-node-sum (right t))))))

"Test"
"(tree-node-sum testtree)"
(tree-node-sum testtree)  ;; output 25
(newline)

"Problem 2c"
;; tree-height function
(define (tree-height t)
  (cond ((null? t) 0)
        ((null? (left t)) 0)
        ((null? (right t)) 0)
        (else (+ 1 (max (tree-height (left t))
                        (tree-height (right t)))))))

"Test"
"(tree-height testtree)"
(tree-height testtree)  ;; output 2
(newline)

"Problem 2d"
;; tree-map function
(define (tree-map f t)
  (cond ((null? t) t)
        ((make-tree (f (value t)) (tree-map f (left t)) (tree-map f (right t))))))

"Test"
"(tree-map (lambda(x)(* 3 x)) testtree"
(tree-map (lambda(x)(* 3 x)) testtree) ;; output (3 (9 (21 () ()) (27 () ())) (15 () ()))