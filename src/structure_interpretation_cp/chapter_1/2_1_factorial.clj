(ns structure-interpretation-cp.chapter-1.2-1-factorial)

;;;;;;; recursion

;; recursive process
(defn factorial-1 [n]
  (if (= n 1)
    1
    (* n (factorial-1 (- n 1)))))

(factorial-1 10)

;; (linear) iterative process
(defn factorial-2 [n]
  (let [factorial-iter (fn [product counter]
                         (println product counter)
                         (if (> counter n)
                           product
                           (recur (* product counter)
                                  (inc counter))))]
    (factorial-iter 1 1)))

(factorial-2 6)

;; Exercise 1.9: Each of the following two procedures defines
;; a method for adding two positive integers in terms of the
;; procedures inc, which increments its argument by 1, and
;; dec, which decrements its argument by 1.
(defn add-1 [a b]
  (println a b)
  (if (= a 0)
    b
    (inc (add-1 (dec a) b))))


(defn add-2 [a b]
  (println a b)
  (if (= a 0)
    b
    (add-2 (dec a) (inc b))))

(add-2 3 2)
;; Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5) . Are these
;; processes iterative or recursive?

;; (add-1 4 5)
;; (inc (add-1 3 5))
;; (inc (inc (add-1 2 5)))
;; (inc (inc (inc (add-1 1 5))))
;; (inc (inc (inc (inc (add-1 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; (add-2 4 5)
;; (add-2 3 6)
;; (add-2 2 7)
;; (add-2 1 8)
;; (add-2 0 9)
;; 9
