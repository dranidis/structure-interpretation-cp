(ns structure-interpretation-cp.chapter-2.1-2)

;; Exercise 2.2: Consider the problem of representing line
;; segments in a plane. Each segment is represented as a pair
;; of points: a starting point and an ending point. Define a
;; constructor make-segment and selectors start-segment and
;; end-segment that define the representation of segments in
;; terms of points. Furthermore, a point can be represented
;; as a pair of numbers: the x coordinate and the y coordinate. 
;; Accordingly, specify a constructor make-point and
;; selectors x-point and y-point that define this representation. 
;; Finally, using your selectors and constructors, define a
;; procedure midpoint-segment that takes a line segment as
;; argument and returns its midpoint (the point whose coordinates 
;; are the average of the coordinates of the endpoints).
;; To try your procedures, you’ll need a way to print points:
;; (define (print-point p)
;; (newline)
;; (display "(")
;; (display (x-point p))
;; (display ",")
;; 1(display (y-point p))
;; (display ")"))




(defn make-point [x y]
  [x y])

(defn x-point [p]
  (first p))

(defn y-point [p]
  (second p))

(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))




(defn square [x] (* x x))

(defn distance [p1 p2]
  (Math/sqrt (+ (square (- (x-point p1) (x-point p2)))
                (square (- (y-point p1) (y-point p2))))))



(defn make-segment [p1 p2]
  [p1 p2])

(defn start-segment [s]
  (first s))

(defn end-segment [s]
  (second s))

(defn average [x y]
  (/ (+ x y) 2.0))

(defn midpoint-segment [s]
  (make-point
   (average (x-point (start-segment s))
            (x-point (end-segment s)))
   (average (y-point (start-segment s))
            (y-point (end-segment s)))))

(defn length-segment [s]
  (distance (start-segment s) (end-segment s)))

(def p1 (make-point 1 2))
(def p2 (make-point 4 4))
(print-point (midpoint-segment (make-segment p1 p2)))


;; Exercise 2.3: Implement a representation for rectangles in
;; a plane. (Hint: You may want to make use of Exercise 2.2.) In
;; terms of your constructors and selectors, create procedures
;; that compute the perimeter and the area of a given rectangle. 
;; Now implement a different representation for rectangles. 
;; Can you design your system with suitable abstraction
;; barriers, so that the same perimeter and area procedures
;; will work using either representation?




(defn make-square [p1 p2]
  [p1 p2])

(defn p1-square [r]
  (first r))

(defn p2-square [r]
  (second r))

(defn side-square [r]
  (distance (p1-square r)
            (p2-square r)))

(defn perimeter-square [r]
  (* 4 (side-square r)))

(defn area-square [r] (square (side-square r)))

(def s (make-square (make-point 0 3)
                    (make-point 4 0)))

(perimeter-square s)
(area-square s)

;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-rectangle [p1 p2 h]
  [p1 p2 h])

(defn p1-rectangle [r]
  (first r))

(defn p2-rectangle [r]
  (second r))

(defn h-rectangle [r]
  (last r))

(defn w-rectangle [r]
  (distance (p1-rectangle r)
            (p2-rectangle r)))

;;;;;;;;;;;;;;;;;;;

(defn perimeter-rectangle [r]
  (* 2 (+ (w-rectangle r) (h-rectangle r))))

(defn area-rectangle [r] (* (w-rectangle r) (h-rectangle r)))

(def r (make-rectangle (make-point 0 3)
                       (make-point 4 0)
                       6))

(perimeter-rectangle r)
(area-rectangle r)


;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;

(defn make-triangle [p1 p2 p3]
  [p1 p2 p3])

(defn point-a [t]
  (first t))

(defn point-b [t]
  (second t))

(defn point-c [t]
  (last t))

(defn perimeter-triangle [t]
  (+ (distance (point-a t)
               (point-b t))
     (distance (point-a t)
               (point-c t))
     (distance (point-c t)
               (point-b t))))

(defn distance-point-from-line [p s]
  (/ (Math/abs (- (*
                (- (x-point (start-segment s))
                   (x-point (end-segment s)))
                (- (y-point (start-segment s))
                   (y-point p))
                )
               (*
                (- (y-point (start-segment s))
                   (y-point (end-segment s)))
                (- (x-point (start-segment s))
                   (x-point p)))
               ))
     (length-segment s)))

(defn area-triangle [t]
  (let [base-segment (make-segment (point-a t) (point-b t))
        height (distance-point-from-line (point-c t) base-segment)
        base (length-segment base-segment)]
    (/ (* base height) 2)))

(def triangle (make-triangle (make-point 0 0)
                             (make-point 0 3)
                             (make-point 4 0)))

(perimeter-triangle triangle)
(area-triangle triangle)



