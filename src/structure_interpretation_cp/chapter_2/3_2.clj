(ns structure-interpretation-cp.chapter-2.3-2)

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))


(defn =number? [exp num]
  (and (number? exp) (= exp num)))


;; (defn make-sum [a1 a2] (list '+ a1 a2))
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2))
        (+ a1 a2)
        :else (list '+ a1 a2)))

(make-sum 'x 'y)
(make-sum 1 2)
(eval (make-sum 1 2))

(defn sum? [x]
  (and (list? x) (= (first x) '+)))

(defn addend [s]
  (nth s 1))

;; (defn augend [s]
;;   (nth s 2))

;; Exercise 2.57 Extend the differentiation program to handle sums and 
;; products of arbitrary numbers of (two or more)
;; terms. 
(defn augend [s]
  (if (= (count s) 3)
    (nth s 2)
    (conj (rest (rest s)) '+)))

(addend (make-sum 'x 'y))
(augend (make-sum 'x 'y))

(addend '(+ 1 2 3))
(augend '(+ 1 2 3))

(def add3 '(+ 1 2 3))
(count add3)
(conj (rest (rest add3)) '+)


;; (defn make-product [a1 a2] (list '* a1 a2))
(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2))
        (* m1 m2)
        :else (list '* m1 m2)))

(defn product? [x]
  (and (list? x) (= (first x) '*)))

(defn multiplier [s]
  (nth s 1))

;; (defn multiplicand [s]
;;   (nth s 2))

;; Exercise 2.57 Extend the differentiation program to handle sums and 
;; products of arbitrary numbers of (two or more)
;; terms. 
(defn multiplicand [s]
  (if (= (count s) 3)
    (nth s 2)
    (conj (rest (rest s)) '*)))

(number? 1)
(number? 'x)


;; Exercise 2.56: Show how to extend the basic differentiator
;; to handle more kinds of expressions.
(defn exponentiation? [x]
  (and (list? x) (= (first x) '**)))

(defn base [x]
  (nth x 1))

(defn exponent [x]
  (nth x 2))

(defn make-exponentiation [b e]
  (cond (=number? e 0) 1
        (=number? e 1) b
        :else (list '** b e)))



(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp)
        (make-sum
         (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
         (make-product (deriv (multiplier exp) var)
                       (multiplicand exp)))
        (exponentiation? exp)
        (make-product (exponent exp)
                      (make-product (make-exponentiation
                                     (base exp)
                                     (dec (exponent exp)))
                                    (deriv (base exp) var)))
        :else (throw (Exception. (str "Unknown expression type: " exp)))))

(deriv 0 'x)
(deriv 'x 'x)
(deriv 'x 'y)
(deriv '(+ x x) 'x)
(deriv '(* x x) 'x)
(try (deriv '(/ x x) 'x) (catch Exception e (println (.getMessage e))))

(deriv '(* (* x y) (+ x (+ y 3))) 'x)

(deriv '(** x 3) 'x)

(make-exponentiation 'x 3)
(base (make-exponentiation 'x 3))
(exponent (make-exponentiation 'x 3))
(exponentiation? '(** x 3))
(base '(** x 3))
(exponent '(** x 3))

(deriv '(** x 2) 'x)
(deriv '(** (+ x x) 3) 'x)
(deriv '(+ x x x x) 'x)

