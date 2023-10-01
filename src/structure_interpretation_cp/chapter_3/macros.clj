(ns structure-interpretation-cp.chapter-3.macros)


;; defining macros
(defmacro infix
  [infixed]
  (list (second infixed) (first infixed) (last infixed)))

(infix (1 + 1))

(macroexpand '(infix (1 + 1)))

(macroexpand '(and (and true false) y z))

(lazy-seq (cons 1 [2 3 4]))




(defmacro cons-stream [a b]
  (list 'lazy-seq (list 'cons a (list 'lazy-seq b))))

(cons-stream 1 [2 3])

(macroexpand '(cons-stream 1 [2 3]))

(def ones (cons-stream 1 ones))
ones
(nth ones 1000000)
(first ones)
(rest ones)



(defn positive-numbers
  ([] (positive-numbers 1))
  ([n] (lazy-seq (cons n (positive-numbers (inc n))))))

(take 5 (positive-numbers))