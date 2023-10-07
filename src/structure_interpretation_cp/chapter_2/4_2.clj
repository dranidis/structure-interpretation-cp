(ns structure-interpretation-cp.chapter-2.4-2
  (:require [clojure.test :refer [deftest is run-tests testing]]))


;; 2.4.2 Tagged data

(defn attach-tag [type-tag contents]
  {:type type-tag :contents contents})
(defn type-tag [datum] (get datum :type))
(defn contents [datum] (get datum :contents))

(defn rectangular? [z] (= (type-tag z) :rectangular))
(defn polar? [z] (= (type-tag z) :polar))


(defn square [x] (* x x))

;; rectangular representation from Section 2.4.1
(defn real-part-rectangular [z] (first z))
(defn imag-part-rectangular [z] (second z))
(defn magnitude-rectangular [z]
  (Math/sqrt (+ (square (real-part-rectangular z))
                (square (imag-part-rectangular z)))))
(defn angle-rectangular [z]
  (Math/atan2 (imag-part-rectangular z)
              (real-part-rectangular z)))
(defn make-from-real-imag-rectangular [x y]
  (attach-tag :rectangular (list x y)))
(defn make-from-mag-ang-rectangular [r a]
  (attach-tag :rectangular (list
                            (* r (Math/cos a))
                            (* r (Math/sin a)))))

(defn degrees-to-radians [d]
  (* d (/ (Math/PI) 180)))


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

(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
        (polar? z) (magnitude-polar (contents z))))

(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
        (polar? z) (angle-polar (contents z))))


(defn close-enough [x y] (< (Math/abs (- x y)) 0.0000001))
(close-enough 0.000000001 0.000000003)
(close-enough 0.000000001 0.000010003)


(deftest conversion
  (let [d90 (degrees-to-radians 90)]
    (testing "rect->polar"
      (is (close-enough 1 (real-part (make-from-real-imag-polar 1 2))))
      (is (close-enough 2 (imag-part (make-from-real-imag-polar 1 2))))
      (is (close-enough 1 (real-part (make-from-real-imag-rectangular 1 2))))
      (is (close-enough 2 (imag-part (make-from-real-imag-rectangular 1 2))))
      (is (close-enough 1 (magnitude (make-from-mag-ang-polar 1 d90))))
      (is (close-enough d90 (angle (make-from-mag-ang-polar 1 d90))))
      (is (close-enough 1 (magnitude (make-from-mag-ang-rectangular 1 2))))
      (is (close-enough d90 (angle (make-from-mag-ang-rectangular 1 d90)))))))

(run-tests)