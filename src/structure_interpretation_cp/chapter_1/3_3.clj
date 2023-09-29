(ns structure-interpretation-cp.chapter-1.3-3)

(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (println "FIX")
  (let [close-enough? (fn [v1 v2]
                        (< (Math/abs (- v1 v2))
                           tolerance))
        try-guess (fn [guess]
                    (println "GUESS" guess)
                    (let [next (f guess)]
                      (if (close-enough? guess next)
                        next
                        (recur next))))]
    (try-guess first-guess)))

(fixed-point (fn [x] (Math/cos x)) 1.0)

(fixed-point (fn [x] (+ (Math/sin x) (Math/cos x))) 1.0)


;; the following does not converge. DO NOT execute
(defn sqrt-does-not-converge [x]
  (fixed-point (fn [y] (/ x y)) 1.0))

;; initial 1 -> 2 -> 1 -> 2
(defn sq [y] (/ 2 y))
(sq (sq (sq (sq (sq 1)))))

(defn average [a b] (/ (+ a b) 2))

(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y))) 1.0))

(sqrt 2)

;; golden ratio
(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)



(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn square [x] (* x x))

((average-damp square) 10)

(defn sqrt-d [x]
  (fixed-point (average-damp (fn [y] (/ x y)))
               1.0))

(sqrt-d 2.0)

(defn cube-root [x]
  (fixed-point (average-damp (fn [y] (/ x (square y))))
               1.0))

(cube-root 1000.0)

;; newton's method

(defn deriv [g]
  (let [dx 0.00001]
    (fn [x] (/ (- (g (+ x dx))
                  (g x))
               dx))))

(defn cube [x] (* x x x))
((deriv cube) 5)

(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt-n [x]
  (newtons-method
   (fn [y] (- (square y) x)) 1.0))

(sqrt 2000000.0)
(sqrt-n 2000000.0)

;; Exercise 1.42: Let f and g be two one-argument functions.
;; The composition f after g is defined to be the function x →
;; f(g(x)). 
;; Define a procedure compose that implements composition. 
;; For example, if inc is a procedure that adds 1 to
;; its argument,
;; ((compose square inc) 6)
;; 49

(defn compose [f g]
  (fn [x] (f (g x))))

((compose square inc) 6)

;; Exercise 1.43: If f is a numerical function and n is a positive integer, 
;; then we can form the nth repeated application
;; of f , which is defined to be the function whose value at
;; x is f (f (. . . (f (x)) . . . )). For example, if f is the function
;; x → x + 1, then the nth repeated application of f is the
;; function x → x +n. If f is the operation of squaring a number, 
;; then the nth repeated application of f is the function
;; that raises its argument to the 2n-th power. 
;; Write a procedure that takes as inputs a procedure that computes f and a
;; positive integer n and returns the procedure that computes
;; the nth repeated application of f . Your procedure should be
;; able to be used as follows:
;; ((repeated square 2) 5)
;; 625

(defn repeated [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))

((repeated square 2) 5)

