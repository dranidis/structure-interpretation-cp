(ns structure-interpretation-cp.chapter-3.5-1)

(def the-empty-stream '())

(defn stream-null? [s] (= s '()))

(stream-null? the-empty-stream)

;; macro for cons-stream
;; works for sieve up to 9871

;; (defmacro cons-stream [a b]
;;   (list 'cons a (list 'lazy-seq b)))

;; works for sieve up to 10141
(defmacro cons-stream [a b]
  (list 'lazy-seq (list 'cons a (list 'lazy-seq b))))


(defn stream-car [s] (first s))
(defn stream-cdr [s] (rest s))


(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (dec n))))

(stream-ref (cons-stream 1 the-empty-stream) 0)
(stream-ref (cons-stream 1 the-empty-stream) 1)
(macroexpand '(cons-stream 1 the-empty-stream))


(defn stream-map [proc s]
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(def example-stream (cons-stream 1 (cons-stream 2 the-empty-stream)))
(def mapped (stream-map inc example-stream))
(stream-ref mapped 0)
(stream-ref mapped 1)
(stream-ref mapped 2)
(stream-ref mapped 3)

(defn stream-for-each [proc s]
  (if (stream-null? s)
    'done
    (do (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(defn display-stream [s]
  (stream-for-each println s))

(stream-for-each println example-stream)
(display-stream mapped)

(defn stream-enumerate-interval [low high]
  (if (> low high)
    the-empty-stream
    (cons-stream
     low
     (stream-enumerate-interval (inc low) high))))

(def s1-10 (stream-enumerate-interval 1 10))
s1-10
(display-stream s1-10)

(defn stream-filter [pred stream]
  (cond (stream-null? stream) the-empty-stream
        (pred (stream-car stream))
        (cons-stream (stream-car stream)
                     (stream-filter
                      pred
                      (stream-cdr stream)))
        :else (stream-filter pred (stream-cdr stream))))

(display-stream (stream-filter odd? s1-10))


;; (defn prime? [num]
;;   (nil? (seq (filter
;;               (fn [x] (= (rem num x) 0))
;;               (range 2 num)))))

(defn square [x] (* x x))
(defn divides? [a b] (= 0 (rem b a)))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (inc test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(smallest-divisor 1234321)

(defn prime? [n]
  (= n (smallest-divisor n)))



(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval
                  10000 100000))))


(defn integers-starting-from [n]
  (cons-stream n (integers-starting-from (inc n))))

(def integers (integers-starting-from 1))
integers

(defn divisible? [x y] (= (rem x y) 0))

(defn sieve [stream]
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (fn [x]
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(def primes (sieve (integers-starting-from 2)))
primes
(nth primes 1244)
(nth primes 1217)
;; => 10141

;; results in stack overflow
(nth primes 1245)





