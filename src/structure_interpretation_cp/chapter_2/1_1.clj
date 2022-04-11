(ns structure-interpretation-cp.chapter-2.1-1)

(defn gcd [n d]
  (if (= d 0)
    n
    (gcd d (rem n d))))

(gcd 12 11)

(defn make-rat [n d]
  (let [g (gcd n d)]
    (seq [(/ n g) (/ d g)])))


;; Exercise 2.1: Define a better version of make-rat that handles 
;; both positive and negative arguments. make-rat should
;; normalize the sign so that if the rational number is positive,
;; both the numerator and denominator are positive, and if
;; the rational number is negative, only the numerator is negative.

(defn make-rat [n d]
  (if (neg? d) (make-rat (- n) (- d))
      (let [g (Math/abs (gcd n d))]
        (seq [(/ n g) (/ d g)]))))


(make-rat 3 5)
(make-rat -3 5)
(make-rat 3 -5)
(make-rat -3 -5)

(defn numer [r] (first r))
(defn denom [r] (second r))


(def r (make-rat 1 3))
(numer r)
(denom r)

(defn print-rat [r]
  (println (str (numer r) "/" (denom r))))

(print-rat r)


(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))

(add-rat one-half one-third)

(print-rat (add-rat one-half one-third))
(print-rat (add-rat one-third one-third))


