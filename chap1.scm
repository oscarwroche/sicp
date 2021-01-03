(define size 2)

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a) (sum-of-squares (+ a 7) (/ 5 a)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs-1 x)
  (if (< x 0)
      (- x)
      x))

;;; Exercise 1.1

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;;; Exercise 1.2

(/ (+ 5 (/ 1 2) (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;; Exercise 1.3

(define (square-three a b c)
  (if (and (> b a) (> c a))
      (sum-of-squares b c)
      (if (and (> a b) (> c b))
	  (sum-of-squares a c)
	  (sum-of-squares a b))))
	  
;;; Exercise 1.4

;;; The function sums a and the abolute value of b

;;; Exercise 1.5

#|
With applicative order the procedure would crash (stack overflow)
With normal order, the procedure returns 0
|#

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; Exercise 1.6

#|
cond evaluates all of its arguments before returning.
So when the guess is good enough, it will still evaluate
the next iteration of sqrt, and so on.
if won't evaluate it because good-enough returns true.
|#

;;; Exercise 1.7

#|
For very large numbers, there won't be enough digits
to represent the part that is below 0.001.
For very small numbers, no matter what good-enough
value is chose, numbers whose difference to the guess
are smaller can always be found.
|#

(define (sqrt-iter-bis guess last-guess x)
  (if (good-enough?-bis guess last-guess x)
      guess
      (sqrt-iter-bis (improve guess x)
		     guess
		     x)))

(define (good-enough?-bis guess last-guess x)
  (< (abs
      (- guess last-guess))
     (* 0.001 guess)))

(define (sqrt-bis x)
  (sqrt-iter-bis 1.0 0 x))

;;; Exercise 1.8

(define (croot-iter guess last-guess x)
  (if (good-enough?-bis guess last-guess x)
      guess
      (croot-iter (improve-croot guess x)
		     guess
		     x)))

(define (improve-croot guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (croot x)
  (croot-iter 1.0 0 x))

;;;

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-bis n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;;; Exercise 1.9

#|
The first procedure has a recursive process (because dec is at the bottom of the
tree).
The second one is iterative.
|#

;;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

#|
f(n) = 2n
g(n) = 2^n if n>0, 0 if n=0
h(n) = 2^^n
|#

;;; Exercise 1.11

(define (fsum n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f(- n 3)))))))

(define (fsum-iter res counter n fn2 fn3)
  (cond
    ((> counter n) res)
    ((< counter 3)
	 (fsum-iter
	    counter
	    (+ counter 1)
	    n
	    res
            fn2))
    (else (fsum-iter
	     (+ res (* 2 fn2) (* 3 fn3))
	     (+ counter 1)
	     n
	     res
	     fn2))))
  
(define (fsum2 n) (fsum-iter 0 0 n 0 0))

;;; Exercise 1.12

(define (pascal row pos)
  (cond
   ((or (= pos 0) (= pos row)) 1)
   ((or (> pos row) (< pos 0) (< row 0)) 0)
   (else (+ (pascal (- row 1) (- pos 1)) (pascal (- row 1) pos)))))

;;; Exercise 1.14

#|
For cc(n, k), O(n^k)
|#

;;; Exercise 1.15

;;; log3(a) steps

;;; Exercise 1.16

(define (fexp-iter n b a)
  (cond
   ((= n 0) a)
   ((even n) (fexp-iter (/ n 2) (* b b) a))
   (else (fexp-iter (- n 1) b (* a b))))
  )

(define (even n)
  (= (remainder n 2) 0))

(define (fexp b n)
  (fexp-iter n b 1))

;;; Exercise 1.17

(define (fast-mult n k)
  (cond
   ((= k 0) 0)
   ((even k) (double (fast-mult n (halve k))))
   (else (+ n (fast-mult n (- k 1))))))

;;; Exercise 1.18

(define (double n)
  (+ n n ))

(define (halve n)
  (/ n 2))

(define (mult-iter n k res)
  (cond
   ((= k 0) res)
   ((even k) (mult-iter (double n) (halve k) res))
   (else (mult-iter n (- k 1) (+ res n)))))

(define (mult n k)
  (mult-iter n k 0))

;;; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;; Exercise 1.20

;;; The subtlety is that even is remainder is evaluated in the if it's not evaluated in the rest

;;; Exercise 1.21

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

;;; Exercise 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes min max)
  (define (aux-search n)
    (cond ((> n max) (display "search over"))
	  (else (timed-prime-test n) (aux-search (+ 2 n)))))
  (aux-search (if (even min) (+ 1 min) min)))

;;; Exercise 1.23

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2) 3 (+ n 2)))

;;; The speedup is rather around 1.5x because of next overhead ?

;;; Exercise 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))   
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;; Exercise 1.25 - much slower because the numbers get too large

;;; Exercise 1.26 - the expmod is evaluated 2 times with the multiplication
;;; With square, the argument is evaluated before being passed.

;;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (inc n) (+ n 1))

(define (sum-cubes n m) (sum cube n inc m))

(define (identity x) x)

(define (sum-integers n m) (sum identity n inc m))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (* 8 (sum pi-term a pi-next b)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;; Exercise 1.29

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (cond ((= k 0) (f a))
	  ((= k n) (f b))
	  ((even k) (* 2 (f (+ a (* k h)))))
	  (else (* 4 (f (+ a (* k h)))))))
  (define (next k) (+ k 1))
  (* (/ h 3) (sum term 0 next n)))

;;; The results are faster and integer

;;; Exercise 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;; Exercise 1.31

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
  
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n) (product-iter identity 1 inc n))

(define (pi-product n)
  (define (term k)
    (cond ((even k) (/ (+ k 2) (+ k 1)))
	  (else (/ (+ k 1) (+ k 2)))))
  (* 4 (product-iter term 1 inc n)))

;;; Exercise 1.32

(define (accumulate-iter combiner zero term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a zero))
  
(define (accumulate combiner zero term a next b)
  (if (> a b)
      zero
      (combiner (term a)
         (accumulate combiner zero term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;;; Exercise 1.33

(define (accumulate-filter combiner zero term filter a next b)
  (if (> a b)
      zero
      (combiner (cond
		  ((filter a) (term a))
		  (else zero))
		(accumulate-filter combiner zero term filter (next a) next b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-squared-primes n)
  (accumulate-filter + 0 square prime? 0 inc n))

(define (product-relative-primes n)
  (accumulate-filter * 1 square (lambda (k) (relative-primes? k n)) 1 inc n))
;;; To do: write relative-primes?

;;;

(define (f x y)
  (let
    ((a (+ 1 (* x y)))
     (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(let ((x 3)
      (y (+ x 2)))
  (* x y))

;;; Exercise 1.34

(define (f g)
  (g 2))

;;;

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average a b) (/ (+ a b) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;;; Exercise 1.35
;;; Phi is a root of the corresponding polynomial

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (mystery) (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))
