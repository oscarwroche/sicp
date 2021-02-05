(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))

(define (make-rat n d) (cons n d))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)) (sign (/ (* n d) (* (abs n) (abs d)))))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))

;;; Exercise 2.2

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
	      (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;;; Exercise 2.3

(define make-rectangle cons)

(define (dot-product segment1 segment2)
  (+
   (*
    (- (x-point (end-segment segment1)) (x-point (start-segment segment1)))
    (- (x-point (end-segment segment2)) (x-point (start-segment segment2))))
   (*
    (- (y-point (end-segment segment1)) (y-point (start-segment segment1)))
    (- (y-point (end-segment segment2)) (y-point (start-segment segment2))))))


(define (build-rect point1 point2 point3)
  (let ((segment1
	 (make-segment point1 point2))
	(segment2
	 (make-segment point1 point3)))
    (if (= (dot-product segment1 segment2) 0)
	(make-rectangle segment1 segment2)
	(
	 (newline)
	 (display "Not a rectangle")
	 (newline)))))

(define (build-rect-2 point1 point2 point3)
  (let ((segment1
	 (make-segment point1 point2))
	(segment2
	 (make-segment point2 point3)))
    (if (= (dot-product segment1 segment2) 0)
	(make-rectangle segment1 segment2)
	(
	 (newline)
	 (display "Not a rectangle")
	 (newline)))))

(define first-segment car)
(define second-segment cdr)

(define (length-segment segment)
  (sqrt (dot-product segment segment)))

(define test-rectangle (build-rect (make-point 0 0) (make-point 0 1) (make-point 1 0)))
(define test-rectangle-2 (build-rect-2 (make-point 1 0) (make-point 0 0) (make-point 0 1)))

(define (perimeter rectangle)
  (* 2 (+ (length-segment (first-segment rectangle)) (length-segment (second-segment rectangle)))))

(define (area rectangle)
  (* (length-segment (first-segment rectangle)) (length-segment (second-segment rectangle))))

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))

;;; Exercise 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;;; Exercise 2.5

;;; Unicity of prime decomposition + isomorphism

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car n)
  (if (= (modulo n 2) 0)
      (+ 1 (car (/ n 2)))
      0))

(define (cdr n)
  (if (= (modulo n 3) 0)
      (+ 1 (car (/ n 3)))
      0))

;;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
    (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;;;

(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (let ((y2 (upper-bound y))
	(y1 (lower-bound y)))
    (if (and (<= y1 0)(>= y2 0))
	((newline)
	 (display "Interval contains 0")
	 (newline))
	(mul-interval x 
                (make-interval (/ 1.0 y2)
                               (/ 1.0 y1))))))

;;; Exercise 2.7

(define lower-bound car)

(define upper-bound cdr)

;;; Exercise 2.8

(define (sub-interval a b)
  (add-interval a (make-interval (- (upper-bound b)) (- (lower-bound b)))))

;;; Exercise 2.9

;;; Not true for multiplication / division, example 0

;;; Exercise 2.10 - see above

;;; Finish these exercises later

(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25 36))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(define (length items)
  (define (length-iter items n)
    (if (null? items)
	n
	(length-iter (cdr items) (+ 1 n))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;; Exercise 2.17

(define (last-pair list)
  (if (= (length list) 1)
      (car list)
      (last-pair (cdr list))))

;;; Exercise 2.18

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

;;; Exercise 2.19

(define us-coins (list 50 25 10 5 1))
(define us-coins (list 25 10 5 1 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;;; Exercise 2.20

(define (even n) (= (modulo n 2) 0))

(define (same-parity . args)
  (define test-function
    (if (even (car args))
	even
	(lambda (x) (not (even x)))))
  (define (rec items)
    (cond
     ((null? items) ())
     ((test-function (car items)) (cons (car items) (rec (cdr items))))
     (else (rec (cdr items)))))
  (rec args))
	  
;;;

(define nil ())

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))

;;; Exercise 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;;; Exercise 2.23

(define (for-each f items)
  (cond ((null? items) #t)
	(else (f (car items))
	      (for-each f (cdr items)))))

;;; Exercise 2.24

;;; (1 (2 (3 4)))

;;; Exercise 2.25

(define list1 (list 1 3 (list 5 7) 9))
(define list2 (list (list 7)))
(define list3
  (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;;; Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

;;; Exercise 2.27

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (if (null? items)
      items
      (append (deep-reverse (cdr items))
	      (if (list? (car items))
		  (list (deep-reverse (car items)))
		  (list (car items))))))

;;; Exercise 2.28

(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (cond ((null? tree) ())
        ((list? (car tree))
	 (append (fringe (car tree))
		 (fringe (cdr tree))))
	(else (cons (car tree) (fringe (cdr tree))))))

(define (fringe tree)
  (define (fringe-iter tree result)
    (if (null? tree)
	result
	(fringe-iter (cdr tree) (append result (car tree)))))
  (fringe-iter tree ()))

;;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch (lambda (x) (car (cdr x))))

(define branch-length car)
(define branch-structure (lambda (x) (car (cdr x))))

(define (total-weight mobile)
  (cond
   ((null? mobile) 0)
   ((pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
	 (total-weight (branch-structure (right-branch mobile)))))
   (else mobile)))

(define test-mobile
  (make-mobile
   (make-branch 3 4)
   (make-branch 5 9)))
		     
(define (is-balanced? mobile)
  (cond
   ((null? mobile) true)
   ((pair? mobile)
    (and (is-balanced? (branch-structure (left-branch mobile)))
	 (is-balanced? (branch-structure (right-branch mobile)))
	 (= (* (total-weight (branch-structure (left-branch mobile)))
	       (branch-length (left-branch mobile)))
	    (* (total-weight (branch-structure (right-branch mobile)))
	       (branch-length (right-branch mobile))))))
   (else true)))

;;;

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;;; Exercise 2.30

(define (square-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree factor)
	     (* sub-tree factor)))
       tree))

(define (square-tree tree factor)
  (cond ((null? tree) ())
	((pair? tree) (cons (square-tree (car tree) factor)
			    (square-tree (cdr tree) factor)))
	 (else (* factor tree))))

(define square-test
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;;; Exercise 2.31

(define (tree-map f tree)
  (cond ((null? tree) ())
	((pair? tree) (cons (tree-map f (car tree))
			    (tree-map f (cdr tree))))
	(else (f tree))))

;;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;; Exercise 2.33

(define nil ())

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2) (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

;;; Exercise 2.35

(define (count-leaves x)
  (accumulate
   +
   0
   (map (lambda (t)
	  (cond ((null? t) 0)
		((not (pair? t)) 1)
		(else (count-leaves t))))
	x)))

;;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;;; Exercise 2.37

(define v (list 1 1 1))

(define (dot-product v w) (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;;; Exercise 2.38

(define (fold-left op initial sequence) (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;;; op should be symmetric

;;; Exercise 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse sequence)
  (fold-left (lambda (x y)
	       (cons y x))
	     ()
	     sequence))

;;; Exercise 2.40

(define nil ())

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map
      (lambda (j)
	(list i j))
      (enumerate-interval 1 (- i 1))
     )
   )
   (enumerate-interval 1 n)))

;;; Exercise 2.41

(define (triples-sum n s)
  (let ((interval (enumerate-interval 1 n)))
    (filter
     (lambda (triplet)
       (=
	(+ (car triplet) (cddr triplet) (caddr triplet))
	s))
     (flatmap
      (lambda (i)
	(flatmap
	 (lambda (j)
	   (map
	    (lambda (k) (list i j k))
	    interval))
	 interval))
      interval))))

(define (unique-triplets n)
  (flatmap
   (lambda (i)
    (flatmap
     (lambda (j)
      (map
       (lambda (k) (list i j k))
       (enumerate-interval 1 (- j 1))))
     (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (triplets-sum n s)
  (filter
   (lambda (t) (= (+ (car t) (cadr t) (caddr t)) s))
   (unique-triplets n)))

;;; Exercise 2.42

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define empty-board ())

(define (safe? k positions)
  (fold-left
   (lambda (a b) (and a b))
   #t
   (map
    (lambda (position) (two-safe? (get-position k positions) position))
    (filter-out-position k positions))))

(define (get-position k positions)
  (car 
   (filter
    (lambda (position)
      (is-column k position))
    positions)))

(define (filter-out-position k positions)
  (filter
   (lambda (position)
     (not (is-column k position)))
   positions))

(define (is-column k position)
  (= (cdr position) k))

(define (two-safe? position1 position2)
  (let ((row1 (car position1))
	(col1 (cdr position1))
	(row2 (car position2))
	(col2 (cdr position2)))
    (and
     (not
      (= (abs (- col2 col1))
	 (abs (- row2 row1))))
     (not
      (= row1 row2)))))

;;;

(define a 1)

(define b 2)

(list a b)

(list 'a 'b)

(list 'a b)

(car '(a b c))
(cdr '(a b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))

(memq 'apple '(x (apple sauce) y apple pear))

;;; Exercise 2.53

(list 'a 'b 'c)

(list (list 'george))
(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))

;;; Exercise 2.54

(define (equal? a b)
  (if (and
       (not (pair? a))
       (not (pair? b)))
      (eq? a b)
      (and
       (eq? (car a) (car b))
       (equal? (cdr a) (cdr b)))))

;;; Exercise 2.55

;;; The string after ' is taken as a list of characters

;;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp)
	 (make-product
	  (make-product
	   (exponent exp)
	   (make-exponentiation (base exp) (- (exponent exp) 1)))
	  (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;; Exercise 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define base cadr)

(define exponent caddr)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	(else (list '** base exponent))))

;;; Exercise 2.57

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))

;;; Exercise 2.58

(define (make-sum a1 a2) (list a1 '+ a2))

(define (make-product m1 m2) (list m1 '* m2))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Exercise 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)        
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;;; Exercise 2.60

(define (append set1 set2)
  (if (null? set1)
      set2
      (cons (car set1) (append (cdr set1) set2))))

(define union-set append)

;;;

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61

(define (adjoin-set element set)
  (cond ((null? set) (append set (list element)))
	((= element (car set)) set)
	((> element (car set)) (cons (car set) (adjoin-set element (cdr set))))
        (else (cons element set))))

;;; Exercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((= (car set1) (car set2)) (union-set (cdr set1) set2))
	((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

;;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; Exercise 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree1
  (list 7
	(list 3
	      (list 1
		    ()
		    ())
	      (list 5
		    ()
		    ()))
	(list 9
	      ()
	      (list 11
		    ()
		    ()))))

(define tree2
  (list 3
	(list 1
	      ()
	      ())
	(list 7
	      (list 5
		    ()
		    ())
	      (list 9
		    ()
		    (list 11
			  ()
			  ())))))

(define tree3
  (list 5
	(list 3
	      (list 1
		    ()
		    ())
	      ())
	(list 9
	      (list 7
		    ()
		    ())
	      (list 11
		    ()
		    ()))))

|#
a. The first procedure is recursive, the second one is iterative.
   Both give the same results for the example trees.
b. First : nlog(n), second : log(n)
#|


;;; Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
;;; The number of steps is n*depth(n) ~ n*log(n)

;;; Exercise 2.65

(define tree-a
  (list 3
	(list 2
	      ()
	      ())
	(list 6
	      ()
	      ())))

(define tree-b
  (list 5
	(list 2
	      ()
	      ())
	(list 6
	      ()
	      ())))


(define (union-trees tree1 tree2)
  (list->tree (union-set
	       (tree->list-2 tree1)
	       (tree->list-2 tree2))))

(define (intersection-trees tree1 tree2)
  (list->tree (intersection-set
	       (tree->list-2 tree1)
	       (tree->list-2 tree2))))

;;;

(define (lookup given-key set-of-records) ; For undordered sets
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;;; Exercise 2.66

(define (lookup given-key tree-of-records) ; From above
  (cond ((null? tree-of-records) false)
        ((equal? given-key (key (entry tree-of-records))) true)
        ((< given-key (entry tree-of-records))
         (lookup x (left-branch set)))
        ((> given-key (entry set))
         (lookup x (right-branch set)))))

;;;

#|
Huffman encoding: encode ASCII with variable-codes whose length reflects
how rare it is to encounter a given symbol.
It is designed with prefix codes, i.e. no symbol encoding can begin
with the encoding of another symbol (if A=0 then all other encodings
have to start with 1).
The symbols are represented as a tree, each node contains all the symbols
below it. Each symbol / leaf is assigned a weight that represents its
frequency.
Huffman devised an algorithm for building the optimal tree.
|#

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree (make-code-tree (make-leaf 'A 4) (make-code-tree (make-leaf 'B 2) (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;; Exercise 2.68

a(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let
	  ((left-tree (left-branch tree))
	   (right-tree (right-branch tree)))
	(cond ((element-of-set? symbol (symbols left-tree))
	       (cons '0 (encode-symbol symbol left-tree)))
	      ((element-of-set? symbol (symbols right-tree))
	       (cons '1 (encode-symbol symbol right-tree)))
	      (else (error "symbol not in tree :" symbol))))))
  
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))) 

(define (successive-merge ordered-pairs)
  (if (null? (cddr ordered-pairs))
	(make-code-tree
	 (car ordered-pairs)
	 (cadr ordered-pairs))
      (successive-merge
       (adjoin-set
	(make-code-tree
	 (car ordered-pairs)
	 (cadr ordered-pairs))
	(cddr ordered-pairs)))))

