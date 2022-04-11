(ns structure-interpretation-cp.chapter-1.2-6)

(defn square [x] (* x x))
(defn divides? [a b] (= 0 (rem b a)))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (inc test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(smallest-divisor 1234321)

(defn prime? [n]
  (= n (smallest-divisor n)))




(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
        (rem
         (square (expmod base (quot exp 2) m))
         m)
        :else
        (rem
         (* base (expmod base (dec exp) m))
         m)))

(defn fermat-test [n]
  (let [try-it (fn [a]
                 (= (expmod a n n) a))]
    (try-it (inc (rand-int (dec n))))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (dec times))
        :else false))

(= (filter prime? (map #(+ 2 %) (range 100)))
(filter #(fast-prime? % 10) (map #(+ 2 %) (range 100))))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)


(= (filter prime? (map #(+ 10000000 %) (range 200)))



;; Exercise 1.23: The smallest-divisor procedure shown at
;; the start of this section does lots of needless testing: After it
;; checks to see if the number is divisible by 2 there is no point
;; in checking to see if it is divisible by any larger even numbers. 
;; This suggests that the values used for test-divisor
;; should not be 2, 3, 4, 5, 6, . . ., but rather 2, 3, 5, 7, 9, . . ..
;; To implement this change, define a procedure next that returns 
;; 3 if its input is equal to 2 and otherwise returns its input plus 2. 
;; Modify the smallest-divisor procedure to use
;; (next test-divisor) instead of (+ test-divisor 1).
;; With timed-prime-test incorporating this modified version of 
;; smallest-divisor, run the test for each of the 12
;; primes found in Exercise 1.22. Since this modification halves
;; the number of test steps, you should expect it to run about
;; twice as fast. Is this expectation confirmed? If not, what is
;; the observed ratio of the speeds of the two algorithms, and
;; how do you explain the fact that it is different from 2?

(defn next-num [n]
  (if (= n 2) 3 (+ n 2)))

(defn find-divisor-2 [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor-2 n (next-num test-divisor))))

(defn smallest-divisor-2 [n]
  (find-divisor-2 n 2))
