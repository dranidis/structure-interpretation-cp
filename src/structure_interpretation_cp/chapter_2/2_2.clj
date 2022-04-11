(ns structure-interpretation-cp.chapter-2.2-2)

(defn make-mobile [left right]
  [left right])

(defn make-branch [length structure]
  [length structure])

(defn left-branch [m]
  (first m))

(defn right-branch [m]
  (second m))

(defn branch-length [b]
  (first b))

(defn branch-structure [b]
  (second b))



(declare total-weight)

(defn branch-weight [b]
  (let [bs (branch-structure b)]
    (if (number? bs)
      bs
      (total-weight bs))))

(defn total-weight [m]
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(def mobile (make-mobile (make-branch 1 6)
                         (make-branch 1 (make-mobile (make-branch 1 3)
                                                     (make-branch 1 3)))))

(defn torque-branch [b]
  (* (branch-length b) (branch-weight b)))

(declare balanced?)

(defn balanced-branch? [b]
  (let [bs (branch-structure b)]
    (if (number? bs)
      true
      (balanced? bs))))

(total-weight mobile)

(defn balanced? [m]
  (and (= (torque-branch (left-branch m))
          (torque-branch (right-branch m)))
       (balanced-branch? (left-branch m))
       (balanced-branch? (right-branch m))))

(balanced? mobile)


