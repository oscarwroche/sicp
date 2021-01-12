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

