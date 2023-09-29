(ns structure-interpretation-cp.chapter-1.1-7)

(defn square [x] (* x x))
(defn average [a b] (/ (+ a b) 2))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x))
     0.001))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (println "Guess: " guess)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

;; rewritting using internal definitions

(defn sqrt-i [x]
  (let [good-enough? (fn [guess]
                       (< (Math/abs (- (square guess) x))
                          0.001))
        improve (fn [guess]
                  (average guess (/ x guess)))
        sqrt-iter (fn [guess]
                    (println "Guess: " guess)
                    (if (good-enough? guess)
                      guess
                      (recur (improve guess))))]
    (sqrt-iter 1.0)))

(sqrt-i 2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.7: The good-enough? test used in computing
;; square roots will not be very effective for finding the square
;; roots of very small numbers. Also, in real computers, arithmetic 
;; operations are almost always performed with limited precision. 
;; This makes our test inadequate for very large
;; numbers. Explain these statements, with examples showing
;; how the test fails for small and large numbers. 
;; An alternative strategy for implementing good-enough? is to watch
;; how guess changes from one iteration to the next and to
;; stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end
;; test. Does this work better for small and large numbers?

(defn good-enough2? [guess previous-guess]
  (< (/ (Math/abs (- guess previous-guess)) guess) 0.0001))

(defn sqrt-iter2 [guess previous-guess x]
  (println "Guess: " guess previous-guess)
  (if (good-enough2? guess previous-guess)
    guess
    (sqrt-iter2 (improve guess x) guess x)))

(defn sqrt2 [x]
  (sqrt-iter2 1.0 0.0 x))


;; Exercise 1.8: Newton’s method for cube roots is based on
;; the fact that if y is an approximation to the cube root of x,
;; then a beer approximation is given by the value
;; (x/y^2 + 2y) / 3
;; Use this formula to implement a cube-root procedure analogous to the square-root procedure. 
;; (In Section 1.3.4 we will
;; see how to implement Newton’s method in general as an
;; abstraction of these square-root and cube-root procedures.)

(defn improve-cube [guess x]
  (/ (+
      (/ x (square guess))
      (* 2 guess))
     3))

(defn cube-root-iter [guess previous-guess x]
  (println "Guess: " guess previous-guess)
  (if (good-enough2? guess previous-guess)
    guess
    (cube-root-iter (improve-cube guess x) guess x)))

(defn cube-root [x]
  (cube-root-iter 1.0 0.0 x))

(defn cube [x] (* x x x))

(cube (cube-root 999))
