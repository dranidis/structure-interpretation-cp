(ns structure-interpretation-cp.chapter-2.4-3
  (:require [clojure.test :refer [deftest is run-tests testing]]))

;; 2.4.2 Tagged data

(defn attach-tag [type-tag contents]
  {:type type-tag :contents contents})
(defn type-tag [datum] (get datum :type-tag))
(defn contents [datum] (get :contents datum))

(defn rectangular? [z] (= (type-tag z) :rectangular))
(defn polar? [z] (= (type-tag z) :polar))

;; rectangular representation from Section 2.4.1
(defn real-part-rectangular [z] (first z))
(defn imag-part-rectangular [z] (second z))

(defn make-from-real-imag-rectangular [x y]
  (attach-tag :rectangular (list x y)))
(defn make-from-mag-ang-rectangular [r a]
  (attach-tag :rectangular (list
                            (* r (Math/cos a))
                            (* r (Math/sin a)))))

(comment
  (make-from-real-imag-rectangular 1 2)
  (defn degrees-to-radians [d]
    (* d (/ (Math/PI) 180)))
  (make-from-mag-ang-rectangular 1 (degrees-to-radians 90))
  ;
  )
(defn square [x] (* x x))

;; polar representation
(defn magnitude-polar [z] (first z))
(defn angle-polar [z] (second z))
(defn real-part-polar [z] (* (magnitude-polar z)
                             (Math/cos (angle-polar z))))
(defn imag-part-polar [z] (* (magnitude-polar z)
                             (Math/sin (angle-polar z))))

(defn make-from-real-imag-polar [x y]
  (attach-tag :polar (list (Math/sqrt (+ (square x) (square y)))
                           (Math/atan2 y x))))
(defn make-from-mag-ang-polar [r a]
  (attach-tag :polar (list r a)))


(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
        (polar? z) (real-part-polar (contents z))))

(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
        (polar? z) (imag-part-polar (contents z))))

(defn close-enough [x y] (< (Math/abs (- x y)) 0.0000001))
(close-enough 0.000000001 0.000000003)
(close-enough 0.000000001 0.000010003)

(deftest conversion
  (testing "rect->polar"
    (is (close-enough 1 (real-part (make-from-real-imag-polar 1 1))))))

(def p (make-from-real-imag-polar 1 1))
p

(polar? p)
(rectangular? p)
(real-part (make-from-real-imag-polar 1 1))
(run-tests)




;; 2.4.3 Data-Directed Programming and Additivity

;; The general strategy of checking the type of a datum and calling an
;; appropriate procedure is called dispatching on type.