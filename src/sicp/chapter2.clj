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
;; dRes/res = (xdy + ydx) /  (xy(
;; dRes/res = dy/y + dx/x
;; Error percentage

;; Exercise 2.14
;; Insights from the Eli Bendersky
(comment
  (def aa (make-center-percentage 1 0.05))

;;If we divide same aa/aa; we get 1 but that's not the case when we divide the intervals
  (div-interval aa aa)
;; => (0.9974999999999999 1.0025062656641603)
)

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
  (cond (empty? xs) nil
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
     (second))
)

;; Exercise 2.26
(comment
  (def x (list 1 2 3))
  (def y (list 4 5 6))

  (concat x y)
  ;; => (1 2 3 4 5 6)

  (cons x y)
  ;; => ((1 2 3) 4 5 6)

  (list x y)
  ;; => ((1 2 3) (4 5 6))
)

;; Exercise 2.27
(defn deep-reverse [xs]
  (cond (not (seq? xs)) xs
        (empty? xs) nil
        :else (reverse (map deep-reverse xs))))

(comment
  (def x (list (list 1 2) (list 3 4)))

  (reverse x)

  (deep-reverse x)
)

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

  (fringe (list x x))
)

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

  (balanced? m''')
  ;; => false
)

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
  (reduce (fn [higher-terms coeff]
            (* higher-terms coeff))))
