(ns structure-interpretation-cp.chapter-1.2-3)


;; Exercise 1.15: e sine of an angle (specified in radians)
;; can be computed by making use of the approximation sin x ≈ x
;; if x is sufficiently small, and the trigonometric identity
;; sin x = 3 sin
;; x
;; 3
;; − 4 sin3
;; x
;; 3
;; to reduce the size of the argument of sin. (For purposes of
;; this exercise an angle is considered “sufficiently small” if its
;; magnitude is not greater than 0.1 radians.) 

(defn cube [x] (* x x x))

(defn p [x] (- (* 3 x)
               (* 4 (cube x))))

(def sufficiently-small-angle 0.1)

(defn sine [angle]
  (println angle)
  (if (not (> (Math/abs angle) sufficiently-small-angle))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)
(sine 90)