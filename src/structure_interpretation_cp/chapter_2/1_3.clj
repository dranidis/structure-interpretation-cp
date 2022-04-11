(ns structure-interpretation-cp.chapter-2.1-3)

;; procedural representation of data

;; message passing

;; int,int -> (int -> int)
(defn cons-1 [x y]
  (fn [m]
    (cond (= m 0) x
          (= m 1) y
          :else (.println *err* "Argument not 0 or 1: cons" m))))

(defn car [z] (z 0))
(defn cdr [z] (z 1))

(def cc (cons-1 3 5))
cc
(car cc)
(cdr cc)


;; Exercise 2.4: Here is an alternative procedural representation of pairs. 
;; For this representation, verify that (car (cons
;; x y)) yields x for any objects x and y.
;; (define (cons x y)
;; (lambda (m) (m x y)))
;; (define (car z)
;; (z (lambda (p q) p)))
;; What is the corresponding definition of cdr?

;; cons-2: int,int =>  ( ((int,int) -> c) -> c)
(defn cons-2 [x y]
  (fn [m]
    (m x y)))

(defn car-2 [z]
  (z (fn [x _] x)))

(defn cdr-2 [z]
  (z (fn [_ y] y)))

(car-2 (cons-2 4 5))
(cdr-2 (cons-2 4 5))

;; Exercise 2.5: Show that we can represent pairs of nonnegative 
;; integers using only numbers and arithmetic operations if we 
;; represent the pair a and b as the integer that is
;; the product 2^a3^b. Give the corresponding definitions of the
;; procedures cons, car, and cdr.

(defn power [b n]
  (cond (= n 0) 1
        :else (* b (power b (dec n)))))

(defn find-base [z n]
  (if (= 0 (rem z n))
    (inc (find-base (quot z n) n))
    0))



(defn cons-n [x y]
  (* (power 2 x) (power 3 y)))

(defn car-n [z]
  (find-base z 2))

(defn cdr-n [z]
  (find-base z 3))

(def z (cons-n 2 5))
(car-n z)
(cdr-n z)

;; Exercise 2.6: In case representing pairs as procedures wasn’t
;; mind-boggling enough, consider that, in a language that
;; can manipulate procedures, we can get by without numbers
;; (at least insofar as nonnegative integers are concerned) by
;; implementing 0 and the operation of adding 1 as
;; (define zero (lambda (f) (lambda (x) x)))
;; (define (add-1 n)
;; (lambda (f) (lambda (x) (f ((n f) x)))))
;; This representation is known as Church numerals, after its
;; inventor, Alonzo Church, the logician who invented the λcalculus.
;; Define one and two directly (not in terms of zero and add1). 
;; (Hint: Use substitution to evaluate (add-1 zero)). Give
;; a direct definition of the addition procedure + (not in terms
;; of repeated application of add-1).

(def zero (fn [f] (fn [x] x)))

(defn add-1 [n]
  (fn [f] (fn [x] (f (n f) x))))

;; (add-1 zero)
;; (fn [f] (fn [x] (f
;;                  ((fn [f] (fn [x] x)) f)
;;                  x)))

;; (fn [f] (fn [x]
;;           (f (fn [x] x) x)))

;; (fn [f] (fn [x] (f x)))


(def one (fn [f] (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

(defn church->int [c]
  ((c inc) 0))

(church->int two)