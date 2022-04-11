(ns structure-interpretation-cp.chapter-1.2-4-b)

;; fibonacci
;; state variables a, b

;; Tranformation T:
;; a <- a + b
;; b <- a

;; starting with a 1 and b 0
;; a 1
;; b 0
;;
;; a 1
;; b 1
;;
;; a 2
;; b 1
;;
;; a 3
;; b 2
;;
;; a 5
;; b 3

;; linear iteration
(defn fib [n]
  (let [fib-iter (fn [a b counter]
                  ;;  (println "a: " a "b: " b "counter: " counter)
                   (if (= 0 counter)
                     b
                     (recur (+ a b) a (dec counter))))]
    (fib-iter 1 0 n)))

(map fib (range 10))

;; T(pq) (a, b) => (b*q + a*q + a*p, b*p + a*q)

(defn t [p q]
  (fn [a b]
    [(+ (* b q)
        (* a q)
        (* a p))
     (+ (* b p)
        (* a q))]))

((t 0 1) 1 0)

(defn apply-n [t n x]
  (if (= 0 n)
    x
    (apply-n t (dec n) (apply t x))))

(apply-n (t 0 1) 5 [1 0])

(defn fib-app [n]
  (second (apply-n (t 0 1) n [1 0])))

(map fib-app (range 10))



;; Exercise 1.19: There is a clever algorithm for computing
;; the Fibonacci numbers in a logarithmic number of steps.
;; Recall the transformation of the state variables a and b in
;; the fib-iter process of Section 1.2.2: a ← a +b and b ← a.
;; Call this transformation T , and observe that applying T
;; over and over again n times, starting with 1 and 0, produces
;; the pair Fib(n + 1) and Fib(n). In other words, the Fibonacci
;; numbers are produced by applying T^n, the nth power of the
;; transformationT , starting with the pair (1, 0). Now consider
;; T to be the special case of p = 0 and q = 1 in a family of
;; transformations Tpq , where Tpq transforms the pair (a, b)
;; according to a ← bq + aq + ap and b ← bp + aq. Show
;; that if we apply such a transformation Tpq twice, the effect
;; is the same as using a single transformation Tp′q' of the
;; same form, and compute p′ and q′ in terms of p and q. This
;; gives us an explicit way to square these transformations,
;; and thus we can compute T^n using successive squaring, as
;; in the fast-expt procedure. Put this all together to 
;; complete the following procedure, which runs in a logarithmic
;; number of steps:
;; (define (fib n)
;;   (fib-iter 1 0 0 1 n))
;; (define (fib-iter a b p q count)
;;   (cond ((= count 0) b)
;;         ((even? count)
;;          (fib-iter a
;;                    b
;;                    ⟨??⟩ ; compute p′
;;                    ⟨??⟩ ; compute q′
;;                    (/ count 2)))
;;         (else (fib-iter (+ (* b q) (* a q) (* a p))
;;                         (+ (* b p) (* a q))
;;                         p
;;                         q
;;                         (- count 1)))))

(defn fib-iter [a b p q count]
  (println a b p q count)
  (cond (= count 0) b
        (even? count)
        (fib-iter a
                  b
                  (+ (* p p) (* q q))
                  (+ (* 2 p q) (* q q))
                  (quot count 2))
        :else (fib-iter (+ (* b q)
                           (* a q)
                           (* a p))
                        (+ (* b p)
                           (* a q))
                        p
                        q
                        (dec count))))

(defn fib-t [n] (fib-iter 1 0 0 1 n))

(defn fib-fast [n]
  (let [fib-iter (fn  [a b p q count]
                   (println a b p q count)
                   (cond (= count 0) b
                         (even? count)
                         (recur a
                                b
                                (+ (* p p) (* q q))
                                (+ (* 2 p q) (* q q))
                                (quot count 2))
                         :else (recur (+ (* b q)
                                         (* a q)
                                         (* a p))
                                      (+ (* b p)
                                         (* a q))
                                      p
                                      q
                                      (dec count))))]
    (fib-iter 1 0 0 1 n)))

(fib 50)
(fib-t 50)
(fib-fast 50)

(time (dotimes [n 10000] (fib 50))) ;;"Elapsed time: 34.230583 msecs"
(time (dotimes [n 10000] (fib-fast 50))) ;;"Elapsed time: 34.230583 msecs"



