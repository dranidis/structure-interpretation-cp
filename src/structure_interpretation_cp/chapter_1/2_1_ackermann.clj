(ns structure-interpretation-cp.chapter-1.2-1-ackermann)

;; Exercise 1.10: The following procedure computes a mathematical function called Ackermann’s function.

(defn ackermann [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (ackermann (dec x) (ackermann x (dec y)))))

;; (ackermann 3 3)
;; (ackermann 2 (ackermann 3 2))
;; (ackermann 2 (ackermann 2 (ackermann 3 1)))
;; (ackermann 2 (ackermann 2 2))
;; (ackermann 2 (ackermann 1 (ackermann 2 1)))
;; (ackermann 2 (ackermann 1 2))
;; (ackermann 2 (ackermann 0 (ackermann 1 1)))
;; (ackermann 2 (ackermann 0 2))
;; (ackermann 2 (* 2 2))
;; (ackermann 2 4)
;; (ackermann 1 (ackermann 2 3))
;; (ackermann 1 (ackermann 1 (ackermann 2 2)))
;; (ackermann 1 (ackermann 1 (ackermann 1 (ackermann 2 1))))
;; (ackermann 1 (ackermann 1 (ackermann 1 2)))
;; (ackermann 1 (ackermann 1 (ackermann 0 (ackermann 1 1))))
;; (ackermann 1 (ackermann 1 (ackermann 0 2)))
;; (ackermann 1 (ackermann 1 4))
;; (ackermann 1 (ackermann 0 (ackermann 1 3)))
;; (ackermann 1 (ackermann 0 (ackermann 0 (ackermann 1 2))))
;; ...
;; (ackermann 1 (ackermann 0 (ackermann 0 4)))
;; (ackermann 1 (ackermann 0 8))
;; (ackermann 1 16)


(ackermann 0 123)
(ackermann 1 8)
(map #(ackermann 2 %) (range 5))

(defn f [n] (ackermann 0 n)) ;; 2*n
(map f (range 20))
(defn g [n] (ackermann 1 n)) ;; 2^n n>1
(map g (range 20))
(defn h [n] (ackermann 2 n)) ;; 2^(2^n)
(map h (range 5))
(defn k [n] (ackermann 3 n)) ;; 2^(2^(2^n))
(map k (range 4))

;; (ackermann 3 4)
;; (ackermann 2 (ackermann 3 3))
;; (ackermann 2 65536) ;; 2^(2^65536)

(defn two-power [x]
  (if (= x 0)
    1
    (* 2 (two-power (dec x)))))
(two-power (two-power (two-power 2)))

