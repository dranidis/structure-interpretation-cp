(ns structure-interpretation-cp.chapter-1.2-2-tree-recursion)


;;; fibonacci
;; tree recursion
(defn fib-r [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-r (dec n))
                 (fib-r (- n 2)))))

(map fib-r (range 10))

;; linear iteration
(defn fib-i [n]
  (let [fib-iter (fn [a b counter]
                   (if (= 0 counter)
                     b
                     (recur (+ a b) a (dec counter))))]
    (fib-iter 1 0 n)))

(map fib-i (range 10))


;; Exercise 1.11: A function f is defined by the rule that
;; f (n) =
;; n if n < 3,
;; f (n − 1) + 2f (n − 2) + 3f (n − 3) if n ≥ 3.
;; Write a procedure that computes f by means of a recursive
;; process. Write a procedure that computes f by means of an
;; iterative process.

(defn f-1 [n]
  (if (< n 3)
    n
    (+ (f-1 (dec n))
       (* 2 (f-1 (- n 2)))
       (* 3 (f-1 (- n 3))))))

(map f-1 (range 10))


(defn f-iter [a b c n]
  (if (< n 3) n
      (let [f3 (+ a (* 2 b) (* 3 c))]
        (if (= n 3) f3
            (f-iter f3 a b (dec n))))))

(defn f-2 [n] (f-iter 2 1 0 n))


(defn f-iter-2 [a b c count n]
  (cond (< n 3) n
        (<= count 0) a
        :else (f-iter-2 (+ a (* 2 b) (* 3 c)) a b (- count 1) n)))

(defn f-3 [n] (f-iter-2 2 1 0 (- n 2) n))
(map f-3 (range 10))

(defn f [n]
  (let [f-i (fn [a b c count]
              (cond (< n 3) n
                    (<= count 0) a
                    :else (recur (+ a (* 2 b) (* 3 c)) a b (- count 1))))]
    (f-i 2 1 0 (- n 2))))

(map f (range 10))
