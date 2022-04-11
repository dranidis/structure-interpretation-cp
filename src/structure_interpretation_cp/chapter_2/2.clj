(ns structure-interpretation-cp.chapter-2.2)

;; Use this notation to write a procedure same-parity that
;; takes one or more integers and returns a list of all the arguments 
;; that have the same even-odd parity as the first
;; argument. For example,
;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)
;; (same-parity 2 3 4 5 6 7)
;; (2 4 6)


(defn same-parity [x & r]
  (into [x] (filter (if (even? x) even? odd?) r)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

(defn for-each [f [x & r]]
  (f x)
  (when (not (nil? r)) (for-each f r)))

(for-each (fn [x] (println x)) [1 2 3 5])



;; (defn make-tree [node left right]
;;   [node left right])

;; (defn tree-node [t] (first t))
;; (defn tree-left [t] (second t))
;; (defn tree-right [t] (last t))

(def tree [[1 2] 3 4])
;; tree

;; (defn count-leaves [t]
;;   (if (vector? t) )
;;   )


;; tree represented as a hierarchical list
(def tree
  '(("a"
     "b")
    "c"))

tree


;; (s/def ::tree (s/or
;;                :non-leaf (s/coll-of ::tree :count 2)
;;                :leaf #(not (coll? %))))
;; binary tree

(defn tree-left [tree] (first tree))
(defn tree-right [tree] (second tree))
(defn is-leaf? [tree] (not (seq? tree)))

(defn count-leaves [t]
  (if (is-leaf? t) 1
      (+ (count-leaves (tree-left t))
         (count-leaves (tree-right t)))))

(count-leaves tree)


;; many branches
(defn count-leaves [t]
  (cond (nil? t) 0
        (is-leaf? t) 1
        (= 0 (count t)) 0 ;; it's a seq because it's not a leaf
        :else (+ (count-leaves (tree-left t))
                 (count-leaves (rest t)))))


(def t '((1 2) 3 4))
(count-leaves t)


