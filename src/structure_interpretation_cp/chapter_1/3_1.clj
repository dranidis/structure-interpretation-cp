(ns structure-interpretation-cp.chapter-1.3-1)

(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(sum-integers 0 100)
(apply + (range 101))

(defn cube [x] (* x x x))

(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (inc a) b))))

(cube 2)
(sum-cubes 0 3)





(defn sum [term a next b]
  (if (>  a b)
    0
    (+ (term a) (sum term (next a) next b))))



(defn sum-cubes-2 [a b]
  (sum cube a inc b))

(sum-cubes 0 10)
(sum-cubes-2 0 10)

(defn pi-sum [a b]
  (sum (fn [x] (/ 1.0 (* x (+ x 2))))
       a
       (fn [x] (+ x 4))
       b))

;; pi
(* 8 (pi-sum 1 10000))


(defn integral [f a b dx]
  (* (sum f (+ a (/ dx 2.0)) #(+ % dx) b)
     dx))

(integral cube 0 1 0.001)


;; Exercise 1.29: Simpson’s Rule is a more accurate method
;; of numerical integration than the method illustrated above.
;; Using Simpson’s Rule, the integral of a function f between
;; a and b is approximated as
;; (h / 3) * (y0 + 4y1 + 2y2 + 4y3 + 2y4 + · · · + 2yn−2 + 4yn−1 + yn),
;; where h = (b − a)/n, for some even integer n, and yk =
;; f (a + kh). (Increasing n increases the accuracy of the approximation.) 
;; Define a procedure that takes as arguments
;; f , a, b, and n and returns the value of the integral, 
;; computed using Simpson’s Rule. 
;; Use your procedure to integrate cube between 0 and 1 
;; (with n = 100 and n = 1000),
;; and compare the results to those of the integral procedure
;; shown above.

(defn simpson-integral [f a b n]
  (let [h (/ (- b a) (* 1.0 n))
        y (fn [k] (f (+ a (* k h))))
        mul (fn [i] (cond (or (= i 0) (= i n)) 1
                          (even? i) 2
                          :else 4))]
    (* (/ h 3.0)
       (sum #(* (mul %) (y %))
            0
            inc
            n))))

(simpson-integral cube 0 1 1000)

;; Exercise 1.30: The sum procedure above generates a linear
;; recursion. The procedure can be rewritten so that the sum
;; is performed iteratively. Show how to do this by filling in
;; the missing expressions in the following definition:
;; (define (sum term a next b)
;; (define (iter a result)
;; (if ⟨??⟩
;; ⟨??⟩
;; (iter ⟨??⟩ ⟨??⟩)))
;; (iter ⟨??⟩ ⟨??⟩))

(defn sum-2 [term a next b]
  (let [iter (fn [x result]
               (if (> x b)
                 result
                 (recur (next x) (+ result (term x)))))]
    (iter a 0)))

(sum cube 1 inc 10)
(sum-2 cube 1 inc 10)


;; Exercise 1.31:
;; a. The sum procedure is only the simplest of a vast number of 
;; similar abstractions that can be captured as higher
;; order procedures. Write an analogous procedure called
;; product that returns the product of the values of a
;; function at points over a given range. 
;; Show how to define factorial in terms of product. Also use product
;; to compute approximations to π using the formula52
;; π / 4 
;; =
;; 2 · 4 · 4 · 6 · 6 · 8 · · ·
;; ----------------------------
;; 3 · 3 · 5 · 5 · 7 · 7 · · ·
;; .
;; b. If your product procedure generates a recursive process, 
;; write one that generates an iterative process. If
;; it generates an iterative process, write one that 
;; generates a recursive process.

(defn product [term a next b]
  (let [iter (fn [x result]
               (if (> x b)
                 result
                 (recur (next x) (* result (term x)))))]
    (iter a 1)))


(defn factorial [n]
  (product identity 1 inc n))

(map factorial (range 10))

(defn pi [n]
  (* 4.0
     (product (fn [i]
                (if (odd? i)
                  (/ (+ i 1.0)
                     (+ i 2.0))
                  (/ (+ i 2.0)
                     (+ i 1.0))))
              1
              inc
              n)))

(map pi (range 100))

(defn pi-2 [n]
  (* 2.0
     (product (fn [i]
                (let [four-n2 (* 4.0 i i)]
                  (/ four-n2
                   (- four-n2 1.0))))
              1
              inc
              n)))

(map pi-2 (range 20))


;; Exercise 1.32:
;; a. Show that sum and product (Exercise 1.31) are both
;; special cases of a still more general notion called accumulate
;; that combines a collection of terms, using some general accumulation function:
;; ;; (accumulate combiner null-value term a next b)
;; accumulate takes as arguments the same term and
;; range specifications as sum and product, together with
;; a combiner procedure (of two arguments) that specifies 
;; how the current term is to be combined with the
;; accumulation of the preceding terms and a null-value
;; that specifies what base value to use when the terms
;; run out. Write accumulate and show how sum and
;; product can both be defined as simple calls to accumulate.

(defn accumulate [combiner null-value term a next b]
  (let [iter (fn [x result]
               (if (> x b)
                 result
                 (recur (next x) (combiner result (term x)))))]
    (iter a null-value)))

(defn sum-3 [term a next b]
  (accumulate + 0 term a next b))

(sum-2 identity 0 inc 10)
(sum-3 identity 0 inc 10)

(defn prod-3 [term a next b]
  (accumulate * 1 term a next b))

(product identity 1 inc 10)
(prod-3 identity 1 inc 10)
