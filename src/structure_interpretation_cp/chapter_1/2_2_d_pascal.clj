(ns structure-interpretation-cp.chapter-1.2-2-d-pascal)



;; Exercise 1.12: The following pattern of numbers is called
;; Pascal’s triangle.
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; . . .
;; The numbers at the edge of the triangle are all 1, and each
;; number inside the triangle is the sum of the two numbers
;; above it. Write a procedure that computes elements of
;; Pascal’s triangle by means of a recursive process.

(defn pairs [x & r]
  (cond (nil? r) []
        :else (into [[x (first r)]] (apply pairs r))))

(defn pascal-triangle [n]
  (cond (= n 1) [1]
        :else (into []
                    (concat [1]
                            (map #(+ (first %) (second %))
                                 (apply pairs (pascal-triangle (dec n))))
                            [1]))))

(map #(pascal-triangle (inc %)) (range 10))
((pascal-triangle 50) 3)

(defn pascal-num [r c]
  ((pascal-triangle r) (dec c)))

(pascal-num 50 4)

(defn pascal [r c]
  (if (or (= c 1) (= c r))
    1
    (+ (pascal (- r 1) (- c 1))
       (pascal (- r 1) c))))

;; Testing 
(pascal 1 1)
(pascal 2 2)
(pascal 3 2)
(pascal 4 2)
(pascal 5 2)
(pascal 50 4)
