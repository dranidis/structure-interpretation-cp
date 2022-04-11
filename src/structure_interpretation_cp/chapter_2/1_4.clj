(ns structure-interpretation-cp.chapter-2.1-4)

(defn make-interval [a b]  (if (< a b) [a b] [b a]))
(defn lower-bound [i] (first i))
(defn upper-bound [i] (second i))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; (defn div-interval [x y]
;;   (mul-interval
;;    x
;;    (make-interval (/ 1.0 (upper-bound y))
;;                   (/ 1.0 (lower-bound y)))))

;; after 2.10
(defn div-interval [x y]
  (if (= (upper-bound y) (lower-bound y))
    (.println *err* (str "Div by zero interval: " y))
    (mul-interval
     x
     (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))))



;; Exercise 2.8: Using reasoning analogous to Alyssa’s, 
;; describe how the difference of two intervals may be computed. 
;; Define a corresponding subtraction procedure, called
;; sub-interval.


(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(def i1 (make-interval 4 5))
(def i2 (make-interval 1.5 2))
(add-interval i1 i2)
(mul-interval i1 i2)
(div-interval i1 i2)

(sub-interval i1 i2)


;; Exercise 2.9: The width of an interval is half of the difference 
;; between its upper and lower bounds. The width is a
;; measure of the uncertainty of the number specified by the
;; interval. For some arithmetic operations the width of the
;; result of combining two intervals is a function only of the
;; widths of the argument intervals, whereas for others the
;; width of the combination is not a function of the widths of
;; the argument intervals. Show that the width of the sum (or
;; difference) of two intervals is a function only of the widths
;; of the intervals being added (or subtracted). Give examples
;; to show that this is not true for multiplication or division.

(defn width [i]
  (/ (Math/abs (- (upper-bound i) (lower-bound i))) 2.0))

(width (make-interval -0.5 -1))

(width i1)
(width i2)
(width (add-interval i1 i2))
(width (sub-interval i1 i2))
(width (mul-interval i1 i2))
(width (div-interval i1 i2))

;; Exercise 2.10: Ben Bitdiddle, an expert systems programmer, 
;; looks over Alyssa’s shoulder and comments that it is
;; not clear what it means to divide by an interval that spans
;; zero. Modify Alyssa’s code to check for this condition and
;; to signal an error if it occurs.

(div-interval i1 (make-interval 3 3))

;; Exercise 2.11: In passing, Ben also cryptically comments:
;; “By testing the signs of the endpoints of the intervals, it is
;; possible to break mul-interval into nine cases, only one
;; of which requires more than two multiplications.” Rewrite
;; this procedure using Ben’s suggestion.


(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))


(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(par1 i1 i2)
(par2 i1 i2)

i1
(div-interval i1 i1)

(defn square-interval [i] (mul-interval i i))

(defn add-number [i n]
  (make-interval (+ (lower-bound i) n) (+ (upper-bound i) n)))

(add-number (square-interval (add-number (make-interval -1 1) 0.5)) -0.25)
