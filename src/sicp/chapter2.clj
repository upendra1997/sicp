(ns sicp.chapter2
  (:require [sicp.chapter1 :as chap1]
            [sicp.pictureLang :as pict]))

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
    (make-point min-x min-y)))

(defn max-point [rectangle]
  (let [lp (first rectangle)
        rp (second rectangle)
        x1 (x-point lp)
        y1 (y-point lp)
        x2 (x-point rp)
        y2 (y-point rp)
        max-x (max x1 x2)
        max-y (max y1 y2)]
    (make-point max-x max-y)))

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

(defn cons'' [x y]
  (fn [m]
    (cond
      (= m 0) x
      (= m 1) y
      :else (throw (ex-info "Incorrect argument for selector" {:selector m})))))


(defn car'' [z] (z 0))

(defn cdr'' [z] (z 1))

;; Exercise 2.4

(defn cons' [x y]
  (fn [m] (m x y)))

(defn car' [z] (z (fn [x y] x)))

(defn cdr' [z] (z (fn [x y] y)))

;; Exercise 2.5
(defn cons''' [a b]
  (* (Math/pow 2 a) (Math/pow 3 b)))

(defn keep-dividing [n d]
  (->> (iterate #(/ %1 d) (int n))
       (take-while #(= (int %1) %1))
       (rest)))

(defn car''' [pair]
  (count (keep-dividing pair 2)))

(defn cdr''' [pair]
  (count (keep-dividing pair 3)))

(let [res (cons''' 12 3)]
  [(car''' res) (cdr''' res)])

;; Exercise 2.6
;; Church Numerals
(defn zero [f]
  (fn [x] x))

(defn add-1 [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

;; by manually expanding these formulas

(defn one [f]
  (fn [x]
    (f x)))

(defn two [f]
  (fn [x]
    (f (f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-interval [a b]
  (cons a (cons b nil)))

;; Exercise 2.7

(defn lower-bound [i]
  (let [a (first i)
        b (second i)]
    (min a b)))

(defn upper-bound [i]
  (let [a (first i)
        b (second i)]
    (max a b)))

(defn add-interval [a b]
  (make-interval
   (+ (lower-bound a) (lower-bound b))
   (+ (upper-bound a) (upper-bound b))))

;; Exercise 2.9
;; Here multiply and divide is not function of add/subtract
;; Mulitply is function of both min/max of a/b
(defn mul-interval [a b]
  (let [res (for [mn [(lower-bound a) (lower-bound b)]
                  mx [(upper-bound a) (upper-bound b)]]
              (* mn mx))]
    (make-interval (reduce min ##Inf res) (reduce max ##-Inf res))))

;; Exercise 2.10
;; Divide is function of interval of reciprocal of b
(defn div-interval [a b]
  (let [ub (upper-bound a)
        lb (lower-bound b)]
    (if (or (= ub 0) (= lb 0))
      (throw (ex-info "divide by zero" {:a a :b b}))
      (mul-interval
       a
       (make-interval
        (/ 1.0 ub)
        (/ 1.0 lb))))))


;; Exercise 2.8

(defn sub-interval [a b]
  (add-interval
   a
   (mul-interval
    (make-interval -1 -1)
    b)))

;; Exercise 2.11
;; Case where mulitply would require two mulitplaction, and somehow the order of multipication may not be sorted or easily trackable, so that would required more than two mulitiplication and we would need to find which is the bigger interval
;; a = [-2 5]
;; b = [-6 3]
;; multiples = [12 -6 -30 15]
;; res = [-30 15]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [c]
  (/ (+ (lower-bound c) (upper-bound c)) 2))

(defn width [c]
  (/ (- (upper-bound c) (lower-bound c)) 2))

;; Exercise 2.12
(defn make-center-percentage [c p]
  (let [w (* c p)]
    (make-center-width c w)))

;; Exercise 2.13
;; Checked the solution from the Eli Bendersky
;; If there are small percentages and we know that all numbers are positive
;; a = x+dx
;; b = y+dy
;; res = (x + dx) * (y + dy)
;; res = xy + xdy + ydx + dxdy
;; if perecntage is small then dxdy ~ 0
;; res = xy + xdy + ydx
;; res = xy + dRes
;; dRes = xdy + ydx
;; dRes/res = (xdy + ydx) / xy
;; dRes/res = dy/y + dx/x
;; Error percentage

;; Exercise 2.14
;; Insights from the Eli Bendersky
(comment
  (def aa (make-center-percentage 1 0.05))

  ;;If we divide same aa/aa; we get 1 but that's not the case when we divide the intervals
  (div-interval aa aa))
;; => (0.9974999999999999 1.0025062656641603)


;; Exercise 2.15/2.16
;; https://eli.thegreenplace.net/2007/07/27/sicp-section-214
;; basically to check make it work properly that we need to ensure that divide works correctly if the elements are same and we get the same elements, and I think it seems impossible, because if we are going to divide and if the interval is same, we still cannot know if they belong to diffirent variable or the same variable, if we can find that they belong to the same variable, we can get 1/or some interval which doesn't have any width.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 2.17
(defn last-pair [list]
  (let [head (first list)
        res  (rest list)]
    (if (= res '()) head (recur res))))

(last-pair '(23 72 149 34))
;; => 34

;; Exercise 2.18
(defn reverse [list]
  (let [head (first list)
        res  (rest list)]
    (if (= res '()) (cons head nil) (concat (reverse res) (cons head nil)))))

(reverse '(1 4 9 16 25))


;; Exercise 2.19
;; The order of coins doesn't matter, because the first condition of else where we are trying every possible combinations

(def us-coins '(50 25 10 5 1))

(def uk-coins '(100 50 20 10 5 2 1 0.5))

(def first-denomination first)
(def except-first-denomination rest)
(def no-more? empty?)

(defn cc [amount kind-of-coins]
  (cond
    (= amount 0) 1
    (or (< amount 0) (empty? kind-of-coins)) 0
    :else (+
           (cc amount (rest kind-of-coins))
           (cc (- amount (first kind-of-coins)) kind-of-coins))))

(cc 100 us-coins)
;; => 292
(cc 100 (reverse us-coins))
;; => 292

;; Exercise 2.20
(defn same-parity [& args]
  (if (empty? args) '()
      (let [head   (first args)
            result (->> (rest args)
                        (drop-while
                         (comp not #(= (mod %1 2) (mod head 2)))))]
        (cons head (apply same-parity result)))))

(same-parity 1 2 3 4 5 6 7)
;; => (1 3 5 7)

(same-parity 2 3 4 5 6 7)
;; => (2 4 6)

(same-parity 2 4 5 6)
;; => (2 4 6)

;; Exercise 2.21
(defn square-list [xs]
  (if (empty? xs)
    nil
    (let [head (first xs)
          r (rest xs)]
      (cons (* head head) (square-list r)))))

(defn square-list [xs]
  (map #(* %1 %1) xs))

(square-list '(1 2 3 4))
;; => (1 4 9 16)

;; Exercise 2.22
(defn square-list [xs]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (recur
       (rest things)
       (cons (* (first things) (first things)) answer))))
  (iter xs '()))

(square-list '(1 2 3 4))
;; => (16 9 4 1)

(defn square-list [xs]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (recur
       (rest things)
       (cons answer (cons (* (first things) (first things)) nil)))))
  (iter xs '()))

(square-list '(1 2 3 4))
;; => ((((() 1) 4) 9) 16)

(defn square-list [xs]
  (defn iter [things answer]
    (if (empty? things)
      answer
      (recur
       (rest things)
       (concat answer (cons (* (first things) (first things)) nil)))))
  (iter xs '()))

(square-list '(1 2 3 4))
;; => (1 4 9 16)

;; Exercise 2.23
(defn for-each [f xs]
  (if (empty? xs) true
      (let [head (first xs)
            r    (rest xs)]
        (f head)
        (for-each f r))))

(for-each (fn [x]
            (println x))
          '(57 321 88))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-leaves [xs]
  (cond ;; (empty? xs) 0 ;; Not required;; anti pattern for checking the emptiness of seq
    (not (seq? xs)) 1
    :else (->> xs
               (map count-leaves)
               (reduce + 0))))

;; Exercise 2.24
;; (list 1 (list 2 (list 3 4)))
;; X -> 1
;;      X -> 2
;;           X -> 3
;;                4

;; Exercise 2.25
(comment
  (def a (list 1 3 (list 5 7) 9))
  (def b (list (list 7)))
  (def c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

  (->> a
       (rest)
       (rest)
       (first)
       (second))

  (->> b
       (first)
       (first))

  (->> c
       (second)
       (second)
       (second)
       (second)
       (second)
       (second)))

;; Exercise 2.26
(comment
  (def x (list 1 2 3))
  (def y (list 4 5 6))

  (concat x y)
  ;; => (1 2 3 4 5 6)

  (cons x y)
  ;; => ((1 2 3) 4 5 6)

  (list x y))
;; => ((1 2 3) (4 5 6))


;; Exercise 2.27
(defn deep-reverse [xs]
  (cond (not (seq? xs)) xs
        (empty? xs) nil
        :else (reverse (map deep-reverse xs))))

(comment
  (def x (list (list 1 2) (list 3 4)))

  (reverse x)

  (deep-reverse x))

;; Exercise 2.28
(defn fringe [xs]
  (cond (not (seq? xs)) (list xs)
        (empty? xs) nil
        :else (->> xs
                   (map fringe)
                   (reduce concat))))

(comment
  (def x (list (list 1 2) (list 3 4)))

  (fringe x)

  (fringe (list x x)))

;; Exercise 2.29
(def make-mobile list)

(def make-branch list)

(def left-branch first)

(def right-branch second)

(def branch-length first)

(def branch-structure second)

(def mobile? seq?)

(defn total-weight [mobile]
  (cond (not (mobile? mobile)) mobile
        :else (+
               (total-weight (branch-structure (left-branch mobile)))
               (total-weight (branch-structure (right-branch mobile))))))

(defn torque [branch]
  (* (branch-length branch) (total-weight (branch-structure branch))))

(defn balanced? [mobile]
  (cond (not (mobile? mobile)) true
        :else (let [l (left-branch mobile)
                    r (right-branch mobile)]
                (and (balanced? (branch-structure l))
                     (balanced? (branch-structure r))
                     (= (torque l)
                        (torque r))))))

(comment
  (def m (make-mobile
          (make-branch 1 2)
          (make-branch 3 4)))

  (total-weight m)
  ;; => 6

  (def m' (make-mobile
           (make-branch 1
                        (make-mobile
                         (make-branch 1 2)
                         (make-branch 3 4)))
           (make-branch 2
                        (make-mobile
                         (make-branch 2 2)
                         (make-branch 3 1)))))

  (total-weight m')
  ;; => 9

  (def m'' (make-mobile
            (make-branch 1 2)
            (make-branch 2 1)))

  (balanced? m'')
  ;; => true

  (def m''' (make-mobile
             (make-branch 1 2)
             (make-branch 2 2)))

  (balanced? m'''))
;; => false


;; We don't have to change our program, because in clojure list of two elements is still equal to cons x (cons y nil)

(defn scale-tree [tree factor]
  (cond (not (seq? tree)) (* tree factor)
        (empty? tree) nil
        :else (cons (scale-tree (first tree) factor)
                    (scale-tree (rest tree) factor))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;; Exercise 2.31
(defn map-tree [f tree]
  (map (fn [x]
         (if (not (seq? x))
           (f x)
           (map-tree f x)))
       tree))

(map-tree (partial * 10) (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; => (10 (20 (30 40) 50) (60 70))

;; Exercise 2.30
(defn square-tree [tree]
  (map-tree #(* %1 %1) tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; => (1 (4 (9 16) 25) (36 49))

;; Exercise 2.32
(defn subset [l]
  (if (empty? l) (list (list))
      (let [x (first l)
            xs (rest l)
            r (subset xs)
            res (map (partial cons x) r)]
        (concat r res))))

(subset (list 1 2 3))
;; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; Exercise 2.33
(defn map' [f l]
  (reduce (fn [result item]
            (println item result)
            (concat result (list (f item)))) nil l))

(map' (partial + 1) (list 1 2 3))

(defn append' [l1 l2]
  (reduce (fn [res item]
            (concat res (list item)))
          l1 l2))

(append' (list 1 2 3) (list 4 5 6))

(defn length' [l]
  (reduce (fn [res item] (+ res 1)) 0 l))

(length' (list 1 2 3))

;; Exercise 2.34
(defn horner-eval [x l]
  (reduce (fn [res coeff]
            (+ (* res x) coeff))
          0
          (reverse l)))

(horner-eval 2 [1 3 0 5 0 1])
;; => 79

;; Exercise 2.34
(defn count-leaves'
  [xs]
  (reduce + 0 (map #(if (seq? %1) (count-leaves' %1) 1) xs)))

(count-leaves' (list 1 (list 2 (list 3 4))))
;; => 4

;; Exercise 2.36
(defn accumulate-n
  [op init seqs]
  (cond (not (seq? seqs)) nil
        (empty? (first seqs)) nil
        :else (cons (reduce op init (map first seqs))
                    (accumulate-n op init (map rest seqs)))))

(comment
  (def ll '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
  (accumulate-n + 0 ll))
;; => (22 26 30)


;; Exercise 2.37
(defn dot-product [v w]
  (reduce + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product v %1) m))

(comment
  (def m '((1 2 3) (4 5 6) (7 8 9)))
  (def v '(1 2 3))
  (matrix-*-vector m v))
;; => (14 32 50)


(defn transpose [m]
  (accumulate-n #(concat %1 (list %2)) nil m))

(comment
  (def m '((1 2 3) (4 5 6) (7 8 9)))
  (transpose m))
;; => ((1 4 7) (2 5 8) (3 6 9))



(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (partial matrix-*-vector cols) m)))

(comment
  (def m '((1 2 3) (4 5 6) (7 8 9)))
  (def n '((1 2 3) (4 5 6) (7 8 9)))
  (matrix-*-matrix m n))
;; => ((30 36 42) (66 81 96) (102 126 150))



;; Ex 2.8
;; It's called commutative property, i.e. a + b = b + a, a * b = b * a
;; (define (fold-left op initial sequence)
;; (define (iter result rest)
;;   (if (null? rest)
;;     result
;;     (iter (op result (car rest))
;;       (cdr rest))))
;; (iter initial sequence))

(def fold-left reduce)

(defn fold-right [f init coll]
  (if (seq coll)
    (f (first coll) (fold-right f init (rest coll)))
    init))

;; (fold-right / 1 (list 1 2 3))
;; 3/2

;; (fold-left / 1 (list 1 2 3))
;; 1/6

;; (fold-right list nil (list 1 2 3))
;; (1 (2 (3 nil)))

;; (fold-left list nil (list 1 2 3))
;; (((nil 1) 2) 3)


;; Ex 2.39
(defn reverse-right [l]
  (fold-right #(concat %2 (list %1)) nil l))

(defn reverse-left [l]
  (fold-left #(cons %2 %1) nil l))

(comment
  (reverse-left (list 1 2 3))
  ;;=> (3 2 1)
  (reverse-right (list 1 2 3)))
;;=> (3 2 1)


;; Ex 2.40
(defn unique-pairs [n]
  (for [i (range 1 (inc n))
        j (range 1 i)]
    [i j]))

(defn prime-sum? [[x y]]
  (chap1/prime? (+ x y)))

(defn prime-sum-pairs [n]
  (let [up (unique-pairs n)]
    (->> up
         (filter prime-sum?)
         (map #(conj %1 (reduce + 0 %1))))))


(comment
  (prime-sum-pairs 6))
;;=> ([2 1 3] [3 2 5] [4 1 5] [4 3 7] [5 2 7] [6 1 7] [6 5 11])


;; ex 2.41
(defn find-ordered-triplets [n s]
  (for [i (range 1 (inc n))
        j (range 1 i)
        k (range 1 j)
        :when (and
               (not= i j k)
               (= (+ i j k) s))]
    [i j k]))

(find-ordered-triplets 10 10)
;;=> ([5 3 2] [5 4 1] [6 3 1] [7 2 1])

;; ex 2.42
(defn queens [board-size]
  (defn safe? [positions]
    (let [rc           (map #(identity [%1 %2]) positions (range board-size))
          [rest laast] (split-at (dec (count positions)) rc)
          [x y]         (first laast)]
      (and (= (count positions) (count (set positions)))
           (not (some (fn [[nx ny]]
                        (= (abs (- nx x))
                           (abs (- ny y))))
                      rest)))))
  (defn queen-cols [k]
    (if (= k 0)
      [[]]
      (for [rest-of-board (queen-cols (dec k))
            position      (range board-size)
            :let          [result (conj rest-of-board position)]
            :when         (safe? result)]
        result)))
  (queen-cols board-size))

(comment
  (take 10 (queens 8)))
;;=> ([0 4 7 5 2 6 1 3]
;;    [0 5 7 2 6 3 1 4]
;;    [0 6 3 5 7 1 4 2]
;;    [0 6 4 7 1 3 5 2]
;;    [1 3 5 7 2 0 6 4]
;;    [1 4 6 0 2 7 5 3]
;;    [1 4 6 3 0 7 5 2]
;;    [1 5 0 6 3 7 2 4]
;;    [1 5 7 2 0 3 6 4]
;;    [1 6 2 5 7 4 0 3])


;; ex 2.43
;; the queen-cols function is inside the innermost loop :(
;; so if the queen thing takes times T
;; so then the loop would be (n-1)!T (very slow)

;; ex 2.46

(defn make-vect [x y]
  {:x x :y y})

(defn add-vect [v1 v2]
  (make-vect (+ (:x v1) (:x v2))
             (+ (:y v1) (:y v2))))

(defn scale-vect [s v]
  (make-vect (* s (:x v))
             (* s (:y v))))

(defn sub-vect [v1 v2]
  (make-vect (- (:x v1) (:x v2))
             (- (:y v1) (:y v2))))

;; ex 2.47
(defn make-frame [origin edge1 edge2]
  {:origin origin
   :edge1 edge1
   :edge2 edge2})

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (:origin frame)
     (add-vect (scale-vect (:x v) (:edge1 frame))
               (scale-vect (:y v) (:edge2 frame))))))

;; ex 2.48
(defn make-segment [start end]
  {:start start :end end})

(defn segments->painter [segments]
  (fn [frame]
    (let [m (frame-coord-map frame)]
      (doseq [segment segments]
        (pict/draw-line
         (m (:start segment))
         (m (:end segment)))))))

#_(defn file->painter [file-name origin size]
    (fn [frame]
      (let [m (frame-coord-map frame)]
        (pict/draw-img file-name (m origin) (m size)))))

#_(def rogers
    (file->painter
     "img/william-barton-rogers.jpg"
     (make-vect 0.0 0.0)
     (make-vect 1.0 1.0)))

;; ex 2.49
(def outline
  (segments->painter
   [(make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))]))

(def cross
  (segments->painter
   [(make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0))]))

(def diamond
  (segments->painter
   [(make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
    (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
    (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))]))

(def wave
  (segments->painter [   ;; Head
                      (make-segment (make-vect 0.40 1.00) (make-vect 0.35 0.85))
                      (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
                      (make-segment (make-vect 0.40 0.65) (make-vect 0.60 0.65))
                      (make-segment (make-vect 0.60 0.65) (make-vect 0.65 0.85))
                      (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))

                      ;; Left Arm / Upper Torso
                      (make-segment (make-vect 0.35 0.85) (make-vect 0.15 0.60))
                      (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85))
                      (make-segment (make-vect 0.00 0.65) (make-vect 0.15 0.40))
                      (make-segment (make-vect 0.15 0.40) (make-vect 0.30 0.60))

                      ;; Right Arm / Upper Torso
                      (make-segment (make-vect 0.65 0.85) (make-vect 0.85 0.60))
                      (make-segment (make-vect 0.85 0.60) (make-vect 1.00 0.35))
                      (make-segment (make-vect 1.00 0.15) (make-vect 0.85 0.45))
                      (make-segment (make-vect 0.85 0.45) (make-vect 0.70 0.60))

                      ;; Torso and Left Leg
                      (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.50))
                      (make-segment (make-vect 0.35 0.50) (make-vect 0.25 0.00))
                      (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))

                      ;; Right Leg and Lower Torso
                      (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
                      (make-segment (make-vect 0.65 0.00) (make-vect 0.70 0.60))]))

(comment
  (pict/paint outline :file "img/border.png"))
(comment
  (pict/paint cross :file "img/cross.png"))
(comment
  (pict/paint diamond :file "img/diamond.png"))
(comment
  (pict/paint wave :file "img/wave.png"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame
                new-origin
                (sub-vect (m corner1) new-origin)
                (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0) ; new origin 
                     (make-vect 1.0 1.0) ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(defn shrink-to-upper-right [painter]
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn squash-inwards [painter]
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)
        paint-left (transform-painter
                    painter1
                    (make-vect 0.0 0.0)
                    split-point
                    (make-vect 0.0 1.0))
        paint-right (transform-painter
                     painter2
                     split-point
                     (make-vect 1.0 0.0)
                     (make-vect 0.5 1.0))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

;; ex 2.50
(def flip-horiz
  (comp rotate90 rotate90 flip-vert))

(def rotate180
  (comp rotate90 rotate90))

(def rotate270
  (comp rotate180 rotate90))

(def example1
  (beside
   (squash-inwards wave)
   (shrink-to-upper-right wave)))

(comment
  (pict/paint example1 :file "img/ex1.png"))

(def example2 (flip-horiz example1))

(comment
  (pict/paint example2 :file "img/ex2.png"))

(def example3 (rotate270 example2))

(comment
  (pict/paint example3 :file "img/ex3.png"))

;; ex 2.51
(defn below [painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)
        paint-top (transform-painter
                   painter1
                   split-point
                   (make-vect 1.0 0.5)
                   (make-vect 0.0 1.0))
        paint-bottom (transform-painter
                      painter2
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 0.0)
                      split-point)]
    (fn [frame]
      (paint-top frame)
      (paint-bottom frame))))

(defn below' [painter1 painter2]
  (rotate90
   (beside
    (rotate270 painter2)
    (rotate270 painter1))))

(def example4
  (below
   (rotate90 wave)
   wave))

(def example5
  (below'
   (rotate90 wave)
   wave))

(comment
  (pict/paint example4 :file "img/ex4.png"))
(comment
  (pict/paint example5 :file "img/ex5.png"))

;; Ex 2.52
(defn overlay [painter1 painter2]
  (fn [frame]
    (painter1 frame)
    (painter2 frame)))

(def wave-box
  (overlay
   outline
   wave))

(comment
  (pict/paint wave-box :file "img/wave-box.png"))

;; ex 2.44
(defn right-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (right-split painter (- n 1))]
      (beside painter (below smaller smaller)))))

(defn up-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (- n 1))]
      (below painter (beside smaller smaller)))))

;; ex 2.45

(defn split [outer-order recurse-order]
  (fn [painter n]
    (if (= n 0) painter
        (let [smaller ((split outer-order recurse-order) painter (- n 1))]
          (outer-order painter (recurse-order smaller smaller))))))

(defn corner-split [painter n]
  (if (= n 0)
    painter
    (let [up (up-split painter (- n 1))
          right (right-split painter (- n 1))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split painter (- n 1))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

;; ex 2.52 a

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))

(def ex6 (square-limit wave 2))
(comment
  (pict/paint ex6 :file "img/ex6.png"))

;; (def ex7 rogers)
(comment
  (pict/paint ex7 :file "img/ex7.png"))

;; ex 2.52 b

(def right-split' (split beside below))
(def up-split' (split below beside))

(defn corner-split' [painter n]
  (if (= n 0)
    painter
    (let [up (up-split' painter (- n 1))
          right (right-split' painter (- n 1))
          corner (corner-split' painter (- n 1))]
      (beside (below painter (flip-vert up))
              (below (flip-horiz right) (rotate90 corner))))))
;; ex 2.52 c 

(defn square-limit' [painter n]
  (let [quarter (corner-split' painter n)
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))

(def ex7 (square-limit' wave 2))
(comment
  (pict/paint ex7 :file "img/ex7.png"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn memq [item xs]
  (cond (empty? xs) nil
        (= item (first xs)) xs
        :else (recur item (rest xs))))

(comment
  (memq 'apple '(pear banana prune))
  ;;=> nil
  (memq 'apple '(x (apple sauce) y apple pear)))
;;=> (apple pear)


(def car first)
(def cdr next)
(def cadr (comp first rest))
(defn caddr [coll] (nth coll 2))
(defn pair? [xs]
  (and (coll? xs)
       (<= 3 (count xs))))
(defn =number? [exp num]
  (and (number? exp)
       (= exp num)))

;; ex 2.53
(list 'a 'b 'c)
;;=> (a b c)
(list (list 'george))
;;=> ((george))
(cdr '((x1 x2) (y1 y2)))
;;=> ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
;;=> (y1 y2)
(pair? (car '(a short list)))
;;=> false
(pair? (car '((a b c) short list)))
;;=> true
(memq 'red '((red shoes) (blue socks)))
;;=> nil
(memq 'red '(red shoes blue socks))
;;=> (red shoes blue socks)

;; ex 2.54
(= '(this is a list) '(this is a list))
;;=> true
(= '(this is a list) '(this (is a) list))
;;=> false

;; ex 2.55
;; (car (quote (quate abracadbra)))
(car ''abracadabra)
;;=> quote

(defn variable? [e] (symbol? e))

(defn same-variable? [v1 v2]
  (and
   (variable? v1)
   (variable? v2)
   (= v1 v2)))

(defn make-sum'
  ([a1 a2]
   (cond (=number? a1 0) a2
         (=number? a2 0) a1
         (and (number? a1)
              (number? a2)) (+ a1 a2)
         :else (list '+ a1 a2))))

(defn make-sum
  ([& forms]
   (reduce make-sum' 0 forms)))

(defn sum? [e]
  (and
   (pair? e)
   (= (car e) '+)))

(defn addend [e]
  (cadr e))

(defn augend [e]
  (apply make-sum ((comp cdr cdr) e)))

(defn make-product' [m1 m2]
  (cond
    (or (=number? m1 0) (=number? m2 0)) 0
    (=number? m1 1) m2
    (=number? m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

(defn make-product
  ([& forms]
   (reduce make-product' 1 forms)))

(defn product? [e]
  (and
   (pair? e)
   (= (car e) '*)))

(defn multiplier [e]
  (cadr e))

(defn multiplicand [e]
  (apply make-product ((comp cdr cdr) e)))

;; ex 2.56
(defn make-exponent [u n]
  (cond (=number? n 1) u
        (=number? n 0) 1
        (and (number? u) (number? n)) (Math/pow u n)
        :else (list '** u n)))

(defn exponentiation? [e]
  (and
   (pair? e)
   (= (car e) '**)))

(defn base [e]
  (cadr e))

(defn exponent [e]
  (caddr e))

(defn simplify [expr]
  (if (and (seq? expr)
           (some (partial = (car expr)) ['* '+ '**]))
    expr
    (cond (not (seq? expr)) expr
          (memq '+ expr) (let [[a1 a2] (split-with (partial not= '+) expr)]
                           (make-sum (simplify a1)
                                     (simplify (rest a2))))
          (memq '* expr) (let [[m1 m2] (split-with (partial not= '*) expr)]
                           (make-product
                            (simplify m1)
                            (simplify (rest m2))))
          (memq '** expr) (let [[u n] (split-with (partial not= '**) expr)]
                            (make-exponent
                             (simplify u)
                             (simplify (rest n))))
          (and (coll? expr)
               (= 1 (count expr))) (simplify (first expr))
          :else expr)))

(defn deriv [exp var]
  (let [exp (simplify exp)]
    (cond (number? exp) 0
          (variable? exp) (if (same-variable? exp var) 1 0)
          (sum? exp) (make-sum (deriv (addend exp) var)
                               (deriv (augend exp) var))
          (product? exp) (make-sum
                          (make-product (multiplier exp)
                                        (deriv (multiplicand exp) var))
                          (make-product (deriv (multiplier exp) var)
                                        (multiplicand exp)))
          (exponentiation? exp) (make-product
                                 (make-product
                                  (exponent exp)
                                  (make-exponent
                                   (base exp)
                                   (make-sum (exponent exp) -1)))
                                 (deriv (base exp) var))
          :else (throw (ex-info "unknown expression type: DERIV" {:expr exp})))))

(deriv '(+ x 3) 'x)
;;=> (+ 1 0)
;;=> 1

(deriv '(* x y) 'x)
;;=> (+ (* x 0) (* 1 y))
;;=> y

(deriv '(* (* x y) (+ x 3)) 'x)
;;=> (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))
;;=> (+ (* x y) (* y (+ x 3)))

;; ex 2.56
(deriv '(** x 3) 'x)
;;=> (* 3 (** x 2))

;; ex 2.57
(deriv '(* x y (+ x 3)) 'x)
;;=> (+ (* x y) (* y (+ x 3)))

;; ex 2.58
(deriv '(x + (3 * (x + (y + 2)))) 'x)
;;=> 4

(deriv '(x + 3 * (x + (y + 2))) 'x)
;;=> 4

;;;;;;;;;;;;;;;;;;;;;;;

(defn element-of-set? [x set]
  (cond (nil? set) false
        (= (car set) x) true
        :else (element-of-set? x (cdr set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond
    (or (nil? set1) (nil? set2)) (list)
    (element-of-set? (car set1) set2) (cons
                                       (car set1)
                                       (intersection-set
                                        (cdr set1)
                                        set2))
    :else (intersection-set (cdr set1) set2)))

;; Ex 2.59
(defn union-set [set1 set2]
  (reduce #(adjoin-set %2 %1) set1 set2))

;; ex 2.60
(comment
  (defn element-of-set? [x set]
    (cond (nil? set) false
          (= (car set) x) true
          :else (element-of-set? x (cdr set))))

  (defn adjoin-set [x set] ;; faster O(1)
    (cons x set))

  (defn intersection-set [set1 set2]
    (filter #(element-of-set? %1 set2) set1))

  (defn union-set [set1 set2] ;; faster O(n)
    (reduce #(adjoin-set %2 %1) set1 set2)))

(defn element-of-set? [x set]
  (cond (nil? set) false
        (= x (car set)) true
        (< x (car set)) false
        :else (element-of-set? x (cdr set))))

(defn intersection-set [set1 set2]
  (if (or (nil? set1) (nil? set2))
    (list)
    (let [x1 (car set1)
          x2 (car set2)]
      (cond (= x1 x2) (cons x1 (intersection-set (cdr set1)
                                                 (cdr set2)))
            (< x1 x2) (intersection-set (cdr set1) set2)
            (< x2 x1) (intersection-set set1 (cdr set2))))))

;; ex 2.61
(defn adjoin-set [x set]
  (if (empty? set)
    (list)
    (let [elem (car set)]
      (cond (= elem x) set
            (< elem x) (cons x set)
            :else (cons elem (adjoin-set x (cdr set)))))))
;; by the same reasoing that element-of-set? requires n/2 on
;; average as the element I am searching might be in the middle of
;; of the list, the place where I need to merge the x might be
;; in middle

;; ex 2.62
(defn union-set [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    :else (let [x1 (car set1)
                x2 (car set2)]
            (cond (= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2)))
                  (< x1 x2) (cons x1 (union-set (cdr set1) set2))
                  (> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))

;;;;;;;;