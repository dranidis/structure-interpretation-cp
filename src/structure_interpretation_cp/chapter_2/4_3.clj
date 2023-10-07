(ns structure-interpretation-cp.chapter-2.4-3
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [structure-interpretation-cp.chapter-2.4-2 :refer [attach-tag
                                                               contents
                                                               degrees-to-radians type-tag]]))



;; 2.4.3 Data-Directed Programming and Additivity

;; The general strategy of checking the type of a datum and calling an
;; ;; appropriate procedure is called dispatching on type.

;; This is a powerful
;; strategy for obtaining modularity in system design. On the other hand,
;; implementing the dispatch as in Section 2.4.2 has two significant weaknesses. 
;; One weakness is that the generic interface procedures 
;; (realpart, imag-part, magnitude, and angle) must know about all 
;; the different representations. For instance, suppose we wanted to incorporate
;; a new representation for complex numbers into our complex-number
;; system. We would need to identify this new representation with a type,
;; and then add a clause to each of the generic interface procedures to
;; check for the new type and apply the appropriate selector for that representation.
;; Another weakness of the technique is that even though the individual 
;; representations can be designed separately, we must guarantee
;; that no two procedures in the entire system have the same name. This
;; is why Ben and Alyssa had to change the names of their original 
;; procedures from Section 2.4.1

;; Data-directed programming
;; A programming technique providing a means for modularizing the system design even
;; further. 
;; A table organises the corresponcene between operation name, data type and actual procedure.
;; The interface is implemented as a single procedure that looks up
;; the combination in the table to find the correct procedure to apply.
;; The table can be extended with new procedures without needing to change the interface.

(def table-of-operations (atom {}))
(defn put [op type item]
  (reset! table-of-operations (assoc @table-of-operations (list op type) item)))

(defn install-rectangular-package []
  (letfn [(square [x] (* x x))
          (real-part [z] (first z))
          (imag-part [z] (second z))
          (magnitude [z]
            (Math/sqrt (+ (square (real-part z))
                          (square (imag-part z)))))
          (angle [z]
            (Math/atan2 (imag-part z)
                        (real-part z)))
          (make-from-real-imag [x y]
            (list x y))
          (make-from-mag-ang [r a]
            (list
             (* r (Math/cos a))
             (* r (Math/sin a))))
          (tag [x] (attach-tag :rectangular x))]
    (put :real-part (list :rectangular) real-part)
    (put :imag-part (list :rectangular) imag-part)
    (put :magnitude (list :rectangular) magnitude)
    (put :angle (list :rectangular) angle)
    (put :make-from-real-imag :rectangular
         (fn [x y] (tag (make-from-real-imag x y))))
    (put :make-from-mag-ang :rectangular
         (fn [x y] (tag (make-from-mag-ang x y))))
    :done))

(defn install-polar-package []
  (letfn [(square [x] (* x x))
          (real-part [z] (* (magnitude z)
                            (Math/cos (angle z))))
          (imag-part [z] (* (magnitude z)
                            (Math/sin (angle z))))
          (magnitude [z]
            (first z))
          (angle [z]
            (second z))
          (make-from-real-imag [x y]
            (list (Math/sqrt (+ (square x) (square y)))
                  (Math/atan2 y x)))
          (make-from-mag-ang [r a]
            (list r a))
          (tag [x] (attach-tag :polar x))]
    (put :real-part (list :polar) real-part)
    (put :imag-part (list :polar) imag-part)
    (put :magnitude (list :polar) magnitude)
    (put :angle (list :polar) angle)
    (put :make-from-real-imag :polar
         (fn [x y] (tag (make-from-real-imag x y))))
    (put :make-from-mag-ang :polar
         (fn [r a] (tag (make-from-mag-ang r a))))
    :done))


(defn getop [op & type-tags]
  (let [op (get @table-of-operations (cons op type-tags))]
    (if op
      op
      (println (str "No entry in the table for" op type-tags)))))

;; The complex-arithmetic selectors access the table by means of a
;; general “operation” procedure called apply-generic, which applies a
;; generic operation to some arguments. 
;; apply-generic looks in the table under the name of the operation 
;; and the types of the arguments and
;; applies the resulting procedure if one is present
(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (getop op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (RuntimeException. (str "No method for type: " (cons op type-tags)))))))

;; Using apply-generic, we can define our generic selectors as follows
;; Observe that these do not change at all if a new representation is added
;; to the system.
(defn real-part [z] (apply-generic :real-part z))
(defn imag-part [z] (apply-generic :imag-part z))
(defn magnitude [z] (apply-generic :magnitude z))
(defn angle [z] (apply-generic :angle z))

(defn make-from-real-imag [x y]
  ((getop :make-from-real-imag :rectangular) x y))

(defn make-from-mag-ang [x y]
  ((getop :make-from-mag-ang :polar) x y))


(install-rectangular-package)
(install-polar-package)
@table-of-operations

(defn close-enough [x y] (< (Math/abs (- x y)) 0.0000001))
(close-enough 0.000000001 0.000000003)
(close-enough 0.000000001 0.000010003)


(deftest conversion
  (let [d90 (degrees-to-radians 90)]
    (testing "rect->polar"
      (is (close-enough 1 (real-part (make-from-real-imag 1 2))))
      (is (close-enough 2 (imag-part (make-from-real-imag 1 2))))
      (is (close-enough 1 (real-part (make-from-real-imag 1 2))))
      (is (close-enough 2 (imag-part (make-from-real-imag 1 2))))
      (is (close-enough 1 (magnitude (make-from-mag-ang 1 d90))))
      (is (close-enough d90 (angle (make-from-mag-ang 1 d90))))
      (is (close-enough 1 (magnitude (make-from-mag-ang 1 2))))
      (is (close-enough d90 (angle (make-from-mag-ang 1 d90))))
      ;
      )))

(run-tests)

(real-part (make-from-real-imag 1 2))