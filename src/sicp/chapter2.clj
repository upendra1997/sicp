(ns sicp.chapter2)

(defn linear-combination [a b x y]
  (+ (* a x) (* b y)))

#_(defn linear-combination [a b x y]
  (add (mul a x) (mul b y)))

;; Exercise 2.1
(defn make-rat [num den]
  (let [sign (cond
               (and (< num 0) (< den 0)) :positive
               (and (>= num 0) (>= den 0)) :positive
               :else :negative)
        n (Math/abs num)
        d (Math/abs den)
        c (int (.gcd (BigInteger. (str n)) (BigInteger. (str d))))]
    (if (= sign :negative)
      (cons (- (/ n c)) (cons (/ d c) nil)))
    (cons (/ n c) (cons (/ d c) nil))))

(def numer first)

(def denom second)

(defn add-rat [x y]
  (make-rat
   (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat
   (- (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat
   (* (numer x) (numer y)) 
   (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat
   (* (numer x) (denom y)) 
   (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (=
   (* (numer x) (denom y)) 
   (* (denom x) (numer y))))

(defn rat->string [rat]
  (str (numer rat) "/" (denom rat)))

(rat->string (make-rat 1 2))
;; => "1/2"

(def one-half (make-rat 1 2))

(def one-third (make-rat 1 3))

(rat->string (add-rat one-third one-third))

(rat->string (make-rat 2 4))

;; Exercise 2.2
(defn make-point [x y]
  (cons x (cons y nil)))

(def x-point first)

(def y-point second)

(defn display-point [point]
  (str (x-point point) "," (y-point point)))

(defn make-segment [start end]
  (cons start (cons end nil)))

(def start-segment first)

(def end-segment second)

(defn midpoint-segment [segment]
  (let [start (start-segment segment)
        end    (end-segment segment)
        x1    (x-point start)
        y1    (y-point start)
        x2    (x-point end)
        y2    (y-point end)
        x     (/ (+ x1 x2) 2)
        y     (/ (+ y1 y2) 2)]
    (make-point x y)))

;; Exercise 2.3
(defn make-rectangle [left-point right-point]
  (cons left-point (cons right-point nil)))

(defn min-point [rectangle]
 (let [lp (first rectangle)
        rp (second rectangle)
        x1 (x-point lp)
        y1 (y-point lp)
        x2 (x-point rp)
        y2 (y-point rp)
        min-x (min x1 x2)
        min-y (min y1 y2)]
    (make-point min-x min-y)) )

(defn max-point [rectangle]
 (let [lp (first rectangle)
        rp (second rectangle)
        x1 (x-point lp)
        y1 (y-point lp)
        x2 (x-point rp)
        y2 (y-point rp)
        max-x (max x1 x2)
        max-y (max y1 y2)]
    (make-point max-x max-y)) )

(defn rectangle-area [rectangle]
  (let [mx       (max-point rectangle)
        mn       (min-point rectangle)
        max-x    (x-point mx)
        max-y    (y-point mx)
        min-x    (x-point mn)
        min-y    (y-point mn)
        length-x (- max-x min-x)
        length-y (- max-y min-y)]
    (* length-x length-y)))

(defn rectangle-perimeter [rectangle]
  (let [mx       (max-point rectangle)
        mn       (min-point rectangle)
        max-x    (x-point mx)
        max-y    (y-point mx)
        min-x    (x-point mn)
        min-y    (y-point mn)
        length-x (- max-x min-x)
        length-y (- max-y min-y)]
    (+ length-x length-y)))
