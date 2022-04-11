(ns structure-interpretation-cp.chapter-1.2-4-a)

;; exponentiation

;; linear recursive
(defn expt [b n]
  (if (= n 0)
    1
    (* b (expt b (dec n)))))

(map #(expt % 2) (range 10))

;; linear iterative
(defn expt-1 [b n]
  (let [expt-i (fn [product counter]
                 (if (= counter 0)
                   product
                   (recur (* b product) (dec counter))))]
    (expt-i 1 n)))

(map #(expt-1 % 2) (range 10))

;; We can compute exponentials in fewer steps by using successive
;; squaring.
(defn square [x] (* x x))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (quot n 2)))
        :else (* b (fast-expt b (dec n)))))

(map #(fast-expt 10 %) (range 10))

;; Exercise 1.16: Design a procedure that evolves an iterative 
;; exponentiation process that uses successive squaring
;; and uses a logarithmic number of steps, as does fast-expt.
;; (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep,
;; along with the exponent n and the base b, an additional
;; state variable a, and define the state transformation in such
;; a way that the product ab^n is unchanged from state to state.
;; At the beginning of the process a is taken to be 1, and the
;; answer is given by the value of a at the end of the process.
;; In general, the technique of defining an invariant quantity
;; that remains unchanged from state to state is a powerful
;; way to think about the design of iterative algorithms.)

;; expt-iter b 2 1 2 => b^2
;; expt-iter b 1 b^2 1
;; expt-iter b 1 b^2 0
;; b^2


;; first solution too many steps
;; (defn expt-iter [b n a counter]
;;   (println b n a counter)
;;   (cond (= 0 counter) a
;;         (even? n) (expt-iter b (quot n 2) (* a b) (dec counter))
;;         :else (expt-iter b n (* a b) (dec counter))))

;; (expt-iter 10 4 1 4)
;; (expt-iter 10 2 (* 1 10) 3)
;; (expt-iter 10 1 (* 10 10) 2)
;; (expt-iter 10 1 (* 100 10) 2)
;; (expt-iter 10 1 (* 1000 10) 1)
;; (expt-iter 10 1 10000 0)
;; 10000

;; the idea is to square the b argument for the next iteration
(defn expt-iter [b n a]
  (println b n a)
  (cond (= 0 n) a
        (even? n) (expt-iter (* b b) (quot n 2) a)
        :else (expt-iter b (dec n) (* a b))))

(defn fast-expt-1 [b n]
  (println "N" n)
  (expt-iter b n 1))

(defn fast-expt-3 [b n]
  (let [fast-expt-iter (fn [B N a]
                         (cond (= 0 N) a
                               (even? N) (recur (* B B) (quot N 2) a)
                               :else (recur B (dec N) (* a B))))]
    (fast-expt-iter b n 1)))

(map #(fast-expt-3 2 %) (range 20))


;; Exercise 1.17: The exponentiation algorithms in this section are based on 
;; performing exponentiation by means of
;; repeated multiplication. In a similar way, one can perform
;; integer multiplication by means of repeated addition. e
;; following multiplication procedure (in which it is assumed
;; that our language can only add, not multiply) is analogous
;; to the expt procedure:
;; (define (* a b)
;; (if (= b 0)
;; 0
;; (+ a (* a (- b 1)))))
;; This algorithm takes a number of steps that is linear in b.
;; Now suppose we include, together with addition, operations double, 
;; which doubles an integer, and halve, which
;; divides an (even) integer by 2. 
;; Using these, design a multiplication procedure analogous to 
;; fast-expt that uses a
;; logarithmic number of steps.

(defn mul-rec [a b]
  (println a b)
  (if (= b 0)
    0
    (+ a (mul-rec a (dec b)))))

(mul-rec 3 13)

(defn mul [a b]
  (let [double (fn [x] (* 2 x))
        halve (fn [x] (quot x 2))
        mul-iter (fn [A B acc]
                   (println A B acc)
                   (cond (= B 0) acc
                         (even? B) (recur (double A) (halve B) acc)
                         :else (recur A (dec B) (+ acc A))))]
    (mul-iter a b 0)))

(mul 3 13)

(defn mul-peasant [a b]
  (let [double (fn [x] (* 2 x))
        halve (fn [x] (quot x 2))
        mul-iter (fn [A B acc]
                   (println A B acc)
                   (cond (= B 0) acc
                         (even? B) (recur (double A) (halve B) acc)
                         :else (recur (double A) (halve B) (+ acc A))))]
    (mul-iter a b 0)))

(mul 10 123456789)
(mul-peasant 3 123456789)
