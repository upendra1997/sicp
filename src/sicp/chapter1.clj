(ns sicp.chapter1)

(def pi 3.14159)
(defn area-circle [radius] (* radius radius pi))
(defn circumference [radius] (* 2 radius pi))
(defn square [x] (* x x))
(defn sum-of-squares [x y] (+ (square x) (square y)))
(defn abs [x] (cond
                (> x 0) x
                (= x 0) 0
                (< x 0) (- x)))


;; Excercise Questions
;; 1.1


10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(def a 3)
(def b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
  b a)
(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)
(+ 2 (if (> b a) b a))
(* (cond (> a b) a
         (< a b) b
         :else -1) (+ a 1))

;; Result
;; sicp.core=> 10
;; 10
;; (def a 3)
;; (def b (+ a 1))
;; (+ a b (* a b))
;; (= a b)
;; (if (and (> b a) (< b (* a b)))
;;   b a)
;; (cond (= a 4) 6
;;       (= b 4) (+ 6 7 a)
;;       :else 25)
;; (+ 2 (if (> b a) b a))
;; (* (cond (> a b) a
;;          (< a b) b
;;          :else -1) (+ a 1))sicp.core=> (+ 5 3 4)
;; 12
;; sicp.core=> (- 9 1)
;; 8
;; sicp.core=> (/ 6 2)
;; 3
;; sicp.core=> (+ (* 2 4) (- 4 6))
;; 6
;; sicp.core=> (def a 3)
;; #'sicp.core/a
;; sicp.core=> (def b (+ a 1))
;; #'sicp.core/b
;; sicp.core=> (+ a b (* a b))
;; 19
;; sicp.core=> (= a b)
;; false
;; sicp.core=> (if (and (> b a) (< b (* a b)))
;;        #_=>   b a)
;; 4
;; sicp.core=> (cond (= a 4) 6
;;        #_=>       (= b 4) (+ 6 7 a)
;;        #_=>       :else 25)
;; 16
;; sicp.core=> (+ 2 (if (> b a) b a))
;; 6
;; sicp.core=> (* (cond (> a b) a
;;        #_=>          (< a b) b
;;        #_=>          :else -1) (+ a 1))
;; 16
;; sicp.core=> 

;; 1.2
(def result1_2 (/
                (+ 5 4 (- 2 3 (+ 6 (/ 4 5))))
                (* 3 (- 6 2) (- 2 7))))

;; sicp.core=> result1_2
;; -1/50

;; 1.3
(defn sum-of-squares-of-largest-two [x y z] (cond
                                              (and (< x y) (< x z)) (sum-of-squares y z)
                                              (and (< y x) (< y z)) (sum-of-squares x z)
                                              (and (< z y) (< z x)) (sum-of-squares x y)))

;; 1.4                                            
(defn a-plus-abs-b [a b] ((if (> b 0) + -) a b))

;; 1.5
;; (define (p) (p)) (define (test x y)
;; (if (= x 0) 0 y))

;; sicp.core=> (test 0 (p))
;; Syntax error compiling at (/private/var/folders/ty/zpp9txdj7g9_50nbtzkf_zg80000gr/T/form-init7711180228770296803.clj:1:9).
;; Unable to resolve symbol: p in this context

;; clojure and scheme uses applictive order whereas haskell uses normal order
;; ❯ ghci
;; GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
;; Prelude> p = p
;; Prelude> test x y = if (x == 0) then 0 else y
;; Prelude> test 0 p
;; 0

(defn average [x y] (/ (+ x y) 2))
(defn improve [guess x] (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn new-if [predicate then-clause else-clause] (cond (predicate then-clause)
                                                       (:else else-clause)))
;; 1.6
;;  sicp.core=> (sqrt 30)
;;  Execution error (NullPointerException) at sicp.chapter1/good-enough? (chapter1.clj:120).
;;  null
;; seems to be stuck in loop as it uses applicative order evalution and as function it will evaluate both of them, and we know that at certain point we don't have to improve square root, but the function will keep evaluating both argyments, the guess and the improved value.

(defn sqrt-iter [guess x] (if (good-enough? guess x)
                            guess
                            (sqrt-iter (improve guess x) x)))

(defn sqrt [x] (sqrt-iter 1.0 x))

;; 1.7
;; sicp.core=> (sqrt 99999999999)
;; 316227.76601525676
;; sicp.core=> (* 316227.76601525676 316227.76601525676)
;; 9.999999999899997E10
;; sicp.core=> (abs (- 9.999999999899997E10 99999999999))
;; 3.0517578125E-5
;; sicp.core=> 

;; sicp.core=> (sqrt 0.000004)
;; 0.03129261341049664
;; sicp.core=> (* 0.03129261341049664 0.03129261341049664)
;; 9.792276540587942E-4

(defn good-enough? [guess x]
  (< (abs (- (improve guess x) guess)) (* guess 0.001)))

;; worked very well for small numbers, but for large numbers, but for large numbers, previous strategy was better as it gives mixed result.
;; sicp.core=> (sqrt 99999999999)
;; 316228.8643696895
;; sicp.core=> (sqrt 0.000004)
;; 0.002001107733030763
;; sicp.core=> (* 316228.8643696895 316228.8643696895)
;; 1.0000069466054347E11
;; sicp.core=> (* 0.002001107733030763 0.002001107733030763)
;; 4.004432159195519E-6
;; sicp.core=> (abs (- 1.0000069466054347E11 99999999999))
;; 694661.54347229
;; mixed result
;; sicp.core=> (sqrt 10000) 
;; 100.00000025490743
;; sicp.core=> (* 100.00000025490743 100.00000025490743)
;; 10000.000050981487 ;; new
;; sicp.core=> (* 100.00714038711746 100.00714038711746)
;; 10001.428128408621 ;; prev

;; 1.8
(defn improve [guess x] (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(def cuberoot sqrt)
;; sicp.core=> (cuberoot 27)
;; 3.001274406506175

(defn factorial-rec [n] (if (= n 1)
                          1
                          (* n (factorial-rec (- n 1)))))
(defn factorial [n]
  (defn factorial-iter [product counter max-count]
    (if (> counter max-count)
      product
      (factorial-iter (* counter product) (+ counter 1) max-count)))
  (factorial-iter 1 1 n))

;; 1.9
;; first way -> recursive
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; second way -> iterative
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;; 1.10
(defn A [x y] (cond
                (= y 0) 0
                (= x 0) (* 2 y)
                (= y 1) 2
                :else (A (- x 1) (A x (- y 1)))))

(A 1 10)
(A 2 4)
(A 3 3)
;; sicp.core=> (A 1 10)
;; 1024
;; sicp.core=>       (A 2 4)
;; 65536
;; sicp.core=>       (A 3 3)
;; 65536

(defn f [n] (A 0 n))                                        ;; $$ f(n) = 2y $$
(defn g [n] (A 1 n))
;; (A 0 (A 1 (- n 1)))
;; (A 0 (A 0 (A 1 (- n 1 1)))) 
;; (A 0 (A 0 (A 1 (- n 1 1 1 ....)))) 
;; (* 2 2 2 2 1)
;; $$ g(n) = 2^n $$
(defn h [n] (A 2 n))
;; (A 2 n)
;; (A 1 (A 2 (- n 1)))
;; (2 ^ (A 2 (- n 1)))
;; (2 ^ 2 ^ (A 2 (- n 2)))
;; ....
;; (2 ^ 2 ^ 2 ^ 2.. n ^ 2)
;; $$ h(n) = 2^{2^{2^{2^{2^{2^2...n}}}}} $$
;; also called tetrations
;; $$ h(n) = {^{n}2} $$
(defn k [n] (* 5 n n))                                      ;; $$k(n) = 5n^2$$


(defn fib-recur [n] (cond
                      (= n 0) 0
                      (= n 1) 1
                      :else (+ (fib-recur (- n 1)) (fib-recur (- n 2)))))

(defn fib [n]
  (defn fib-iter [a b count] (if (= count 0N) b (recur (+' a b) a (-' count 1N))))
  (fib-iter 1N 0N n))

(defn ^:dynamic count-change [amount]
  (defn ^:dynamic first-denomination [kinds-of-coins]
    (cond (= kinds-of-coins 1) 1
          (= kinds-of-coins 2) 5
          (= kinds-of-coins 3) 10
          (= kinds-of-coins 4) 25
          (= kinds-of-coins 5) 50))
  (defn ^:dynamic cc [amount kinds-of-coins] (cond
                                               (= amount 0) 1
                                               (or (< amount 0) (= kinds-of-coins 0)) 0
                                               :else (+ (cc amount
                                                            (- kinds-of-coins 1)) (cc (- amount
                                                                                         (first-denomination kinds-of-coins))
                                                                                      kinds-of-coins))))

  (cc amount 5))

;; sicp.core=> (count-change 100)
;; 292

;; 1.11
(defn f-recur [n] (cond
                    (< n 3) n
                    :else (+
                           (f-recur (- n 1))
                           (* 2 (f-recur (- n 2)))
                           (* 3 (f-recur (- n 3))))))

(defn f-iter ([n] (f-iter 0 1 2 n))
  ([a b c n] (cond
               (< n 3) n
               (= 3 n) (+ c (* 2 b) (* 3 a))
               :else (f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1)))))

;; 1.12
(defn pascal-triangle [line position] (cond
                                        (= line 1) 1
                                        (= position 1) 1
                                        (= position line) 1
                                        :else (+
                                               (pascal-triangle (- line 1) (- position 1))
                                               (pascal-triangle (- line 1) position))))

;; 1.13 use markdown with latex support to view the proof:
;; prove that $Fib(n)$ is closest integer to $\varphi^n/\sqrt{5}$, where $\varphi=(1+\sqrt{5})/2$  
;; Let $\psi = (1 - \sqrt{5})/2$.  
;; Use induction and the definition of the fibonacchi numbers to prove that $Fib(n) = (\varphi^n - \psi^n)/\sqrt{5}$
;; 
;; Proof:  
;; let $n = 0$  
;; $Fib(0) = (\varphi^0 - \psi^0)/\sqrt{5} = (1 - 1)/\sqrt{5} = 0$ 
;; 
;; let $n = 1$  
;; $Fib(1) = (\varphi^1 - \psi^1)/\sqrt{5}$  
;; $Fib(1) = \frac{\frac{1+\sqrt{5}}{2} - (\frac{(1 - \sqrt{5}}{2})}{\sqrt{5}}$  
;; $Fib(1) = \frac{{1+\sqrt{5}} - 1 + \sqrt{5}}{2\times\sqrt{5}}$  
;; $Fib(1) = \frac{2\times\sqrt{5}}{2\times\sqrt{5}} = 1$  
;; let $Fib(n-1) = \frac{(\varphi^{n-1} - \psi^{n-1})}{\sqrt{5}}$  
;; let $Fib(n-2) = \frac{(\varphi^{n-2} - \psi^{n-2})}{\sqrt{5}}$  
;; and by definition $Fib(n) = Fib(n-1) + Fib(n-2)$
;; and subtituting above values we get  
;; $Fib(n) = \frac{(\varphi^{n-1} - \psi^{n-1})}{\sqrt{5}} + \frac{(\varphi^{n-2} - \psi^{n-2})}{\sqrt{5}}$  
;; $Fib(n) = \frac{(\varphi^{n-1} - \psi^{n-1}) + (\varphi^{n-2} - \psi^{n-2})}{\sqrt{5}}$  
;; $Fib(n) = \frac{\varphi^{n-1} + \varphi^{n-2} - (\psi^{n-1} +\psi^{n-2})}{\sqrt{5}}$  
;; $Fib(n) = \frac{\varphi^{n-2}(\varphi + 1) - \psi^n(\psi^{-1} + \psi^{-2})}{\sqrt{5}}$  
;; using golden ration property $\varphi^2 = \varphi + 1$  
;; $Fib(n) = \frac{\varphi^n - \psi^n(\psi^{-1} + \psi^{-2})}{\sqrt{5}}$  
;; and we find that $\psi^{-1} + \psi^{-2} = 1$  
;; hence $Fib(n) = (\varphi^n - \psi^n)/\sqrt{5}$ proved.

;; 1.14 
;; if we only trace cc calls
;; TRACE t2462: (count-change 11)
;; TRACE t2463: | (cc 11 5)
;; TRACE t2464: | | (cc 11 4)
;; TRACE t2465: | | | (cc 11 3)
;; TRACE t2466: | | | | (cc 11 2)
;; TRACE t2467: | | | | | (cc 11 1)
;; TRACE t2468: | | | | | | (cc 11 0)
;; TRACE t2468: | | | | | | => 0
;; TRACE t2469: | | | | | | (cc 10 1)
;; TRACE t2470: | | | | | | | (cc 10 0)
;; TRACE t2470: | | | | | | | => 0
;; TRACE t2471: | | | | | | | (cc 9 1)
;; TRACE t2472: | | | | | | | | (cc 9 0)
;; TRACE t2472: | | | | | | | | => 0
;; TRACE t2473: | | | | | | | | (cc 8 1)
;; TRACE t2474: | | | | | | | | | (cc 8 0)
;; TRACE t2474: | | | | | | | | | => 0
;; TRACE t2475: | | | | | | | | | (cc 7 1)
;; TRACE t2476: | | | | | | | | | | (cc 7 0)
;; TRACE t2476: | | | | | | | | | | => 0
;; TRACE t2477: | | | | | | | | | | (cc 6 1)
;; TRACE t2478: | | | | | | | | | | | (cc 6 0)
;; TRACE t2478: | | | | | | | | | | | => 0
;; TRACE t2479: | | | | | | | | | | | (cc 5 1)
;; TRACE t2480: | | | | | | | | | | | | (cc 5 0)
;; TRACE t2480: | | | | | | | | | | | | => 0
;; TRACE t2481: | | | | | | | | | | | | (cc 4 1)
;; TRACE t2482: | | | | | | | | | | | | | (cc 4 0)
;; TRACE t2482: | | | | | | | | | | | | | => 0
;; TRACE t2483: | | | | | | | | | | | | | (cc 3 1)
;; TRACE t2484: | | | | | | | | | | | | | | (cc 3 0)
;; TRACE t2484: | | | | | | | | | | | | | | => 0
;; TRACE t2485: | | | | | | | | | | | | | | (cc 2 1)
;; TRACE t2486: | | | | | | | | | | | | | | | (cc 2 0)
;; TRACE t2486: | | | | | | | | | | | | | | | => 0
;; TRACE t2487: | | | | | | | | | | | | | | | (cc 1 1)
;; TRACE t2488: | | | | | | | | | | | | | | | | (cc 1 0)
;; TRACE t2488: | | | | | | | | | | | | | | | | => 0
;; TRACE t2489: | | | | | | | | | | | | | | | | (cc 0 1)
;; TRACE t2489: | | | | | | | | | | | | | | | | => 1
;; TRACE t2487: | | | | | | | | | | | | | | | => 1
;; TRACE t2485: | | | | | | | | | | | | | | => 1
;; TRACE t2483: | | | | | | | | | | | | | => 1
;; TRACE t2481: | | | | | | | | | | | | => 1
;; TRACE t2479: | | | | | | | | | | | => 1
;; TRACE t2477: | | | | | | | | | | => 1
;; TRACE t2475: | | | | | | | | | => 1
;; TRACE t2473: | | | | | | | | => 1
;; TRACE t2471: | | | | | | | => 1
;; TRACE t2469: | | | | | | => 1
;; TRACE t2467: | | | | | => 1
;; TRACE t2490: | | | | | (cc 6 2)
;; TRACE t2491: | | | | | | (cc 6 1)
;; TRACE t2492: | | | | | | | (cc 6 0)
;; TRACE t2492: | | | | | | | => 0
;; TRACE t2493: | | | | | | | (cc 5 1)
;; TRACE t2494: | | | | | | | | (cc 5 0)
;; TRACE t2494: | | | | | | | | => 0
;; TRACE t2495: | | | | | | | | (cc 4 1)
;; TRACE t2496: | | | | | | | | | (cc 4 0)
;; TRACE t2496: | | | | | | | | | => 0
;; TRACE t2497: | | | | | | | | | (cc 3 1)
;; TRACE t2498: | | | | | | | | | | (cc 3 0)
;; TRACE t2498: | | | | | | | | | | => 0
;; TRACE t2499: | | | | | | | | | | (cc 2 1)
;; TRACE t2500: | | | | | | | | | | | (cc 2 0)
;; TRACE t2500: | | | | | | | | | | | => 0
;; TRACE t2501: | | | | | | | | | | | (cc 1 1)
;; TRACE t2502: | | | | | | | | | | | | (cc 1 0)
;; TRACE t2502: | | | | | | | | | | | | => 0
;; TRACE t2503: | | | | | | | | | | | | (cc 0 1)
;; TRACE t2503: | | | | | | | | | | | | => 1
;; TRACE t2501: | | | | | | | | | | | => 1
;; TRACE t2499: | | | | | | | | | | => 1
;; TRACE t2497: | | | | | | | | | => 1
;; TRACE t2495: | | | | | | | | => 1
;; TRACE t2493: | | | | | | | => 1
;; TRACE t2491: | | | | | | => 1
;; TRACE t2504: | | | | | | (cc 1 2)
;; TRACE t2505: | | | | | | | (cc 1 1)
;; TRACE t2506: | | | | | | | | (cc 1 0)
;; TRACE t2506: | | | | | | | | => 0
;; TRACE t2507: | | | | | | | | (cc 0 1)
;; TRACE t2507: | | | | | | | | => 1
;; TRACE t2505: | | | | | | | => 1
;; TRACE t2508: | | | | | | | (cc -4 2)
;; TRACE t2508: | | | | | | | => 0
;; TRACE t2504: | | | | | | => 1
;; TRACE t2490: | | | | | => 2
;; TRACE t2466: | | | | => 3
;; TRACE t2509: | | | | (cc 1 3)
;; TRACE t2510: | | | | | (cc 1 2)
;; TRACE t2511: | | | | | | (cc 1 1)
;; TRACE t2512: | | | | | | | (cc 1 0)
;; TRACE t2512: | | | | | | | => 0
;; TRACE t2513: | | | | | | | (cc 0 1)
;; TRACE t2513: | | | | | | | => 1
;; TRACE t2511: | | | | | | => 1
;; TRACE t2514: | | | | | | (cc -4 2)
;; TRACE t2514: | | | | | | => 0
;; TRACE t2510: | | | | | => 1
;; TRACE t2515: | | | | | (cc -9 3)
;; TRACE t2515: | | | | | => 0
;; TRACE t2509: | | | | => 1
;; TRACE t2465: | | | => 4
;; TRACE t2516: | | | (cc -14 4)
;; TRACE t2516: | | | => 0
;; TRACE t2464: | | => 4
;; TRACE t2517: | | (cc -39 5)
;; TRACE t2517: | | => 0
;; TRACE t2463: | => 4
;; TRACE t2462: => 4
;; 4
;; sicp.core=> 

;; sicp.core=> (clojure.tools.trace/dotrace [count-change cc] (count-change 10))
;; TRACE t2526: (count-change 10)
;; TRACE t2527: | (cc 10 5)
;; TRACE t2528: | | (cc 10 4)
;; TRACE t2529: | | | (cc 10 3)
;; TRACE t2530: | | | | (cc 10 2)
;; TRACE t2531: | | | | | (cc 10 1)
;; TRACE t2532: | | | | | | (cc 10 0)
;; TRACE t2532: | | | | | | => 0
;; TRACE t2533: | | | | | | (cc 9 1)
;; TRACE t2534: | | | | | | | (cc 9 0)
;; TRACE t2534: | | | | | | | => 0
;; TRACE t2535: | | | | | | | (cc 8 1)
;; TRACE t2536: | | | | | | | | (cc 8 0)
;; TRACE t2536: | | | | | | | | => 0
;; TRACE t2537: | | | | | | | | (cc 7 1)
;; TRACE t2538: | | | | | | | | | (cc 7 0)
;; TRACE t2538: | | | | | | | | | => 0
;; TRACE t2539: | | | | | | | | | (cc 6 1)
;; TRACE t2540: | | | | | | | | | | (cc 6 0)
;; TRACE t2540: | | | | | | | | | | => 0
;; TRACE t2541: | | | | | | | | | | (cc 5 1)
;; TRACE t2542: | | | | | | | | | | | (cc 5 0)
;; TRACE t2542: | | | | | | | | | | | => 0
;; TRACE t2543: | | | | | | | | | | | (cc 4 1)
;; TRACE t2544: | | | | | | | | | | | | (cc 4 0)
;; TRACE t2544: | | | | | | | | | | | | => 0
;; TRACE t2545: | | | | | | | | | | | | (cc 3 1)
;; TRACE t2546: | | | | | | | | | | | | | (cc 3 0)
;; TRACE t2546: | | | | | | | | | | | | | => 0
;; TRACE t2547: | | | | | | | | | | | | | (cc 2 1)
;; TRACE t2548: | | | | | | | | | | | | | | (cc 2 0)
;; TRACE t2548: | | | | | | | | | | | | | | => 0
;; TRACE t2549: | | | | | | | | | | | | | | (cc 1 1)
;; TRACE t2550: | | | | | | | | | | | | | | | (cc 1 0)
;; TRACE t2550: | | | | | | | | | | | | | | | => 0
;; TRACE t2551: | | | | | | | | | | | | | | | (cc 0 1)
;; TRACE t2551: | | | | | | | | | | | | | | | => 1
;; TRACE t2549: | | | | | | | | | | | | | | => 1
;; TRACE t2547: | | | | | | | | | | | | | => 1
;; TRACE t2545: | | | | | | | | | | | | => 1
;; TRACE t2543: | | | | | | | | | | | => 1
;; TRACE t2541: | | | | | | | | | | => 1
;; TRACE t2539: | | | | | | | | | => 1
;; TRACE t2537: | | | | | | | | => 1
;; TRACE t2535: | | | | | | | => 1
;; TRACE t2533: | | | | | | => 1
;; TRACE t2531: | | | | | => 1
;; TRACE t2552: | | | | | (cc 5 2)
;; TRACE t2553: | | | | | | (cc 5 1)
;; TRACE t2554: | | | | | | | (cc 5 0)
;; TRACE t2554: | | | | | | | => 0
;; TRACE t2555: | | | | | | | (cc 4 1)
;; TRACE t2556: | | | | | | | | (cc 4 0)
;; TRACE t2556: | | | | | | | | => 0
;; TRACE t2557: | | | | | | | | (cc 3 1)
;; TRACE t2558: | | | | | | | | | (cc 3 0)
;; TRACE t2558: | | | | | | | | | => 0
;; TRACE t2559: | | | | | | | | | (cc 2 1)
;; TRACE t2560: | | | | | | | | | | (cc 2 0)
;; TRACE t2560: | | | | | | | | | | => 0
;; TRACE t2561: | | | | | | | | | | (cc 1 1)
;; TRACE t2562: | | | | | | | | | | | (cc 1 0)
;; TRACE t2562: | | | | | | | | | | | => 0
;; TRACE t2563: | | | | | | | | | | | (cc 0 1)
;; TRACE t2563: | | | | | | | | | | | => 1
;; TRACE t2561: | | | | | | | | | | => 1
;; TRACE t2559: | | | | | | | | | => 1
;; TRACE t2557: | | | | | | | | => 1
;; TRACE t2555: | | | | | | | => 1
;; TRACE t2553: | | | | | | => 1
;; TRACE t2564: | | | | | | (cc 0 2)
;; TRACE t2564: | | | | | | => 1
;; TRACE t2552: | | | | | => 2
;; TRACE t2530: | | | | => 3
;; TRACE t2565: | | | | (cc 0 3)
;; TRACE t2565: | | | | => 1
;; TRACE t2529: | | | => 4
;; TRACE t2566: | | | (cc -15 4)
;; TRACE t2566: | | | => 0
;; TRACE t2528: | | => 4
;; TRACE t2567: | | (cc -40 5)
;; TRACE t2567: | | => 0
;; TRACE t2527: | => 4
;; TRACE t2526: => 4
;; 4
;; sicp.core=> 

;; sicp.core=> (clojure.tools.trace/dotrace [count-change cc] (count-change 12))
;; TRACE t2576: (count-change 12)
;; TRACE t2577: | (cc 12 5)
;; TRACE t2578: | | (cc 12 4)
;; TRACE t2579: | | | (cc 12 3)
;; TRACE t2580: | | | | (cc 12 2)
;; TRACE t2581: | | | | | (cc 12 1)
;; TRACE t2582: | | | | | | (cc 12 0)
;; TRACE t2582: | | | | | | => 0
;; TRACE t2583: | | | | | | (cc 11 1)
;; TRACE t2584: | | | | | | | (cc 11 0)
;; TRACE t2584: | | | | | | | => 0
;; TRACE t2585: | | | | | | | (cc 10 1)
;; TRACE t2586: | | | | | | | | (cc 10 0)
;; TRACE t2586: | | | | | | | | => 0
;; TRACE t2587: | | | | | | | | (cc 9 1)
;; TRACE t2588: | | | | | | | | | (cc 9 0)
;; TRACE t2588: | | | | | | | | | => 0
;; TRACE t2589: | | | | | | | | | (cc 8 1)
;; TRACE t2590: | | | | | | | | | | (cc 8 0)
;; TRACE t2590: | | | | | | | | | | => 0
;; TRACE t2591: | | | | | | | | | | (cc 7 1)
;; TRACE t2592: | | | | | | | | | | | (cc 7 0)
;; TRACE t2592: | | | | | | | | | | | => 0
;; TRACE t2593: | | | | | | | | | | | (cc 6 1)
;; TRACE t2594: | | | | | | | | | | | | (cc 6 0)
;; TRACE t2594: | | | | | | | | | | | | => 0
;; TRACE t2595: | | | | | | | | | | | | (cc 5 1)
;; TRACE t2596: | | | | | | | | | | | | | (cc 5 0)
;; TRACE t2596: | | | | | | | | | | | | | => 0
;; TRACE t2597: | | | | | | | | | | | | | (cc 4 1)
;; TRACE t2598: | | | | | | | | | | | | | | (cc 4 0)
;; TRACE t2598: | | | | | | | | | | | | | | => 0
;; TRACE t2599: | | | | | | | | | | | | | | (cc 3 1)
;; TRACE t2600: | | | | | | | | | | | | | | | (cc 3 0)
;; TRACE t2600: | | | | | | | | | | | | | | | => 0
;; TRACE t2601: | | | | | | | | | | | | | | | (cc 2 1)
;; TRACE t2602: | | | | | | | | | | | | | | | | (cc 2 0)
;; TRACE t2602: | | | | | | | | | | | | | | | | => 0
;; TRACE t2603: | | | | | | | | | | | | | | | | (cc 1 1)
;; TRACE t2604: | | | | | | | | | | | | | | | | | (cc 1 0)
;; TRACE t2604: | | | | | | | | | | | | | | | | | => 0
;; TRACE t2605: | | | | | | | | | | | | | | | | | (cc 0 1)
;; TRACE t2605: | | | | | | | | | | | | | | | | | => 1
;; TRACE t2603: | | | | | | | | | | | | | | | | => 1
;; TRACE t2601: | | | | | | | | | | | | | | | => 1
;; TRACE t2599: | | | | | | | | | | | | | | => 1
;; TRACE t2597: | | | | | | | | | | | | | => 1
;; TRACE t2595: | | | | | | | | | | | | => 1
;; TRACE t2593: | | | | | | | | | | | => 1
;; TRACE t2591: | | | | | | | | | | => 1
;; TRACE t2589: | | | | | | | | | => 1
;; TRACE t2587: | | | | | | | | => 1
;; TRACE t2585: | | | | | | | => 1
;; TRACE t2583: | | | | | | => 1
;; TRACE t2581: | | | | | => 1
;; TRACE t2606: | | | | | (cc 7 2)
;; TRACE t2607: | | | | | | (cc 7 1)
;; TRACE t2608: | | | | | | | (cc 7 0)
;; TRACE t2608: | | | | | | | => 0
;; TRACE t2609: | | | | | | | (cc 6 1)
;; TRACE t2610: | | | | | | | | (cc 6 0)
;; TRACE t2610: | | | | | | | | => 0
;; TRACE t2611: | | | | | | | | (cc 5 1)
;; TRACE t2612: | | | | | | | | | (cc 5 0)
;; TRACE t2612: | | | | | | | | | => 0
;; TRACE t2613: | | | | | | | | | (cc 4 1)
;; TRACE t2614: | | | | | | | | | | (cc 4 0)
;; TRACE t2614: | | | | | | | | | | => 0
;; TRACE t2615: | | | | | | | | | | (cc 3 1)
;; TRACE t2616: | | | | | | | | | | | (cc 3 0)
;; TRACE t2616: | | | | | | | | | | | => 0
;; TRACE t2617: | | | | | | | | | | | (cc 2 1)
;; TRACE t2618: | | | | | | | | | | | | (cc 2 0)
;; TRACE t2618: | | | | | | | | | | | | => 0
;; TRACE t2619: | | | | | | | | | | | | (cc 1 1)
;; TRACE t2620: | | | | | | | | | | | | | (cc 1 0)
;; TRACE t2620: | | | | | | | | | | | | | => 0
;; TRACE t2621: | | | | | | | | | | | | | (cc 0 1)
;; TRACE t2621: | | | | | | | | | | | | | => 1
;; TRACE t2619: | | | | | | | | | | | | => 1
;; TRACE t2617: | | | | | | | | | | | => 1
;; TRACE t2615: | | | | | | | | | | => 1
;; TRACE t2613: | | | | | | | | | => 1
;; TRACE t2611: | | | | | | | | => 1
;; TRACE t2609: | | | | | | | => 1
;; TRACE t2607: | | | | | | => 1
;; TRACE t2622: | | | | | | (cc 2 2)
;; TRACE t2623: | | | | | | | (cc 2 1)
;; TRACE t2624: | | | | | | | | (cc 2 0)
;; TRACE t2624: | | | | | | | | => 0
;; TRACE t2625: | | | | | | | | (cc 1 1)
;; TRACE t2626: | | | | | | | | | (cc 1 0)
;; TRACE t2626: | | | | | | | | | => 0
;; TRACE t2627: | | | | | | | | | (cc 0 1)
;; TRACE t2627: | | | | | | | | | => 1
;; TRACE t2625: | | | | | | | | => 1
;; TRACE t2623: | | | | | | | => 1
;; TRACE t2628: | | | | | | | (cc -3 2)
;; TRACE t2628: | | | | | | | => 0
;; TRACE t2622: | | | | | | => 1
;; TRACE t2606: | | | | | => 2
;; TRACE t2580: | | | | => 3
;; TRACE t2629: | | | | (cc 2 3)
;; TRACE t2630: | | | | | (cc 2 2)
;; TRACE t2631: | | | | | | (cc 2 1)
;; TRACE t2632: | | | | | | | (cc 2 0)
;; TRACE t2632: | | | | | | | => 0
;; TRACE t2633: | | | | | | | (cc 1 1)
;; TRACE t2634: | | | | | | | | (cc 1 0)
;; TRACE t2634: | | | | | | | | => 0
;; TRACE t2635: | | | | | | | | (cc 0 1)
;; TRACE t2635: | | | | | | | | => 1
;; TRACE t2633: | | | | | | | => 1
;; TRACE t2631: | | | | | | => 1
;; TRACE t2636: | | | | | | (cc -3 2)
;; TRACE t2636: | | | | | | => 0
;; TRACE t2630: | | | | | => 1
;; TRACE t2637: | | | | | (cc -8 3)
;; TRACE t2637: | | | | | => 0
;; TRACE t2629: | | | | => 1
;; TRACE t2579: | | | => 4
;; TRACE t2638: | | | (cc -13 4)
;; TRACE t2638: | | | => 0
;; TRACE t2578: | | => 4
;; TRACE t2639: | | (cc -38 5)
;; TRACE t2639: | | => 0
;; TRACE t2577: | => 4
;; TRACE t2576: => 4
;; 4
;; sicp.core=> 


;; this shows that space usage is O(amount) and number of steps seems to be O(n^5)

;; excerise 1.15
(defn cube [x] (* x x x))
(defn ^:dynamic p [x] (- (* 3 x) (* 4 (cube x))))
(defn sine [angle]
  (if (not (> (abs angle) 0.1)) angle
      (p (sine (/ angle 3.0)))))

;; sicp.core=> (require 'clojure.tools.trace)
;; nil
;; sicp.core=> (clojure.tools.trace/dotrace [p] (sine 12.15))
;; TRACE t2353: (p 0.049999999999999996)
;; TRACE t2353: => 0.1495
;; TRACE t2354: (p 0.1495)
;; TRACE t2354: => 0.4351345505
;; TRACE t2355: (p 0.4351345505)
;; TRACE t2355: => 0.9758465331678772
;; TRACE t2356: (p 0.9758465331678772)
;; TRACE t2356: => -0.7895631144708228
;; TRACE t2357: (p -0.7895631144708228)
;; TRACE t2357: => -0.39980345741334
;; -0.39980345741334
;; sicp.core=> 

;; amount of times p is called is 5
;; order of space is o(1) and order of steps is O(log_3(a)) => O(log(a))

(defn expt [b n] (if (= n 0)
                   1
                   (* b (expt b (- n 1)))))

(defn expt-iter [b counter product] (if (= counter 0)
                                      product (expt-iter b
                                                         (- counter 1) (* b product))))

(defn expt [b n] (expt-iter b n 1))

(defn fast-expt [b n] (cond (= n 0) 1
                            (even? n) (square (fast-expt b (/ n 2)))
                            :else (* b (fast-expt b (- n 1)))))
;; exercise 1.16
(defn fast-expt [b n]
  (defn ^:dynamic fast-expt-iter [b n a] (cond
                                           (= n 0) a
                                           (= n 1) (*' a b)
                                           (even? n) (recur (*' b b) (quot n 2) a)
                                           :else (recur (*' b b) (quot n 2) (*' a b))))
  (fast-expt-iter b n 1N))

;; exercise 1.17
(defn doubleN [x] (*' x 2N))
(defn halveN [x] (quot x 2N))
(defn ^:dynamic fast-mul [a b] (cond
                                 (> a b) (fast-mul b a)
                                 (or (= a 0) (= b 0)) 0
                                 (= a 1) b
                                 (even? a) (fast-mul (halveN a) (doubleN b))
                                 :else (+' b (fast-mul (halveN a) (doubleN b)))))

;; exercise 1.18
(defn ^:dynamic fast-mul ([a b] (fast-mul a b 0N))
  ([a b c] (cond
             (> a b) (fast-mul b a c)
             (or (= a 0) (= b 0)) c
             (= a 1) (+' b c)
             (even? a) (recur (halveN a) (doubleN b) c)
             :else (recur (halveN a) (doubleN b) (+' c b)))))

;; exercise 1.19

(defn ^:dynamic fib-iter [a b p q count] (cond (= count 0) b
                                               (even? count) (recur a
                                                                    b
                                                                    (+' (*' p p) (*' q q))
                                                                    (+' (*' q q) (*' 2 p q))
                                                                    (quot count 2))
                                               :else (recur (+' (*' b q) (*' a q) (*' a p))
                                                            (+' (*' b p) (*' a q))
                                                            p
                                                            q
                                                            (-' count 1N))))
(defn fib [n] (fib-iter 1N 0N 0N 1N n))

;; transformation $T_{pq}$ is
;; $$a \larr bq + aq + ap$$  
;; $$b \larr bp + aq$$
;; Let $p=0$ and $q=1$ 
;; $$ a \larr a + b$$
;; $$ b \larr a $$
;; This is what happens in the fib's iteraive algorithm.  
;; lets apply transformation $T_{pq}$ twice to get $T_{p'q'}$  
;; iteration 0
;; $$ a_1 \larr b_0q + a_0q + a_0p $$
;; $$ b_1 \larr b_0p + a_0q$$
;; iteration 2  
;; For transformations on a
;; $$ a_2 \larr b_1q + a_1q + a_1p $$
;; $$ a_2 \larr (b_0p + a_0q).q + (b_0q + a_0q + a_0p).q + (b_0q + a_0q + a_0p).p $$
;; $$ a_2 \larr b_0pq + a_0qq + b_0qq + a_0qq + a_0pq + b_0pq + a_0pq + a_0pp $$
;; $$ a_2 \larr b_0pq + b_0qq + b_0pq + a_0qq + a_0qq + a_0pq + a_0pq + a_0pp $$
;; $$ a_2 \larr b_0.(q^2 + 2pq) + a_0.(q^2 + 2pq + q^2 + p^2) $$
;; $$ a_2 \larr b_0.(q^2 + 2pq) + a_0.(q^2 + 2pq) + a_0.(p^2 + q^2) $$   
;; For transformations on b
;; $$ b_2 \larr b_1p + a_1q $$
;; $$ b_2 \larr (b_0p + a_0q).p + (b_0q+a_0q+a_0p).q $$
;; $$ b_2 \larr b_0pp + a_0pq + b_0qq + a_0qq + a_0pq $$
;; $$ b_2 \larr b_0pp + b_0qq + a_0pq + a_0qq + a_0pq $$
;; $$ b_2 \larr b_0.(p^2 + q^2) + a_0.(q^2 + 2pq) $$
;; This shows that
;; $$ p' \larr p^2 + q^2 $$
;; $$ q' \larr q^2 + 2pq $$

;; running in logarithmic steps
;; sicp.core=> (clojure.tools.trace/dotrace [fib-iter] (fib 32))
;; TRACE t2413: (fib-iter 1N 0N 0N 1N 32)
;; TRACE t2414: | (fib-iter 1N 0N 1N 1N 16)
;; TRACE t2415: | | (fib-iter 1N 0N 2N 3N 8)
;; TRACE t2416: | | | (fib-iter 1N 0N 13N 21N 4)
;; TRACE t2417: | | | | (fib-iter 1N 0N 610N 987N 2)
;; TRACE t2418: | | | | | (fib-iter 1N 0N 1346269N 2178309N 1)
;; TRACE t2419: | | | | | | (fib-iter 3524578N 2178309N 1346269N 2178309N 0N)
;; TRACE t2419: | | | | | | => 2178309N
;; TRACE t2418: | | | | | => 2178309N
;; TRACE t2417: | | | | => 2178309N
;; TRACE t2416: | | | => 2178309N
;; TRACE t2415: | | => 2178309N
;; TRACE t2414: | => 2178309N
;; TRACE t2413: => 2178309N
;; 2178309N
;; sicp.core=> (clojure.tools.trace/dotrace [fib-iter] (fib 33))
;; TRACE t2425: (fib-iter 1N 0N 0N 1N 33)
;; TRACE t2426: | (fib-iter 1N 1N 0N 1N 32N)
;; TRACE t2427: | | (fib-iter 1N 1N 1N 1N 16N)
;; TRACE t2428: | | | (fib-iter 1N 1N 2N 3N 8N)
;; TRACE t2429: | | | | (fib-iter 1N 1N 13N 21N 4N)
;; TRACE t2430: | | | | | (fib-iter 1N 1N 610N 987N 2N)
;; TRACE t2431: | | | | | | (fib-iter 1N 1N 1346269N 2178309N 1N)
;; TRACE t2432: | | | | | | | (fib-iter 5702887N 3524578N 1346269N 2178309N 0N)
;; TRACE t2432: | | | | | | | => 3524578N
;; TRACE t2431: | | | | | | => 3524578N
;; TRACE t2430: | | | | | => 3524578N
;; TRACE t2429: | | | | => 3524578N
;; TRACE t2428: | | | => 3524578N
;; TRACE t2427: | | => 3524578N
;; TRACE t2426: | => 3524578N
;; TRACE t2425: => 3524578N
;; 3524578N

;; exercise 1.20
(defn ^:dynamic mod' [a b] (mod a b))

(defn ^:dynamic gcd-applicative [a b] (if (= b 0) a (recur b (mod' a b))))

(defmacro gcd-normal [a b] (if (= (eval b) 0) (eval a) `(gcd-normal ~b (mod' ~a ~b))))

;;  (clojure.tools.trace/dotrace [mod'] (gcd-applicative 206 40))
;;  TRACE t1993: (mod' 206 40)
;;  TRACE t1993: => 6
;;  TRACE t1994: (mod' 40 6)
;;  TRACE t1994: => 4
;;  TRACE t1995: (mod' 6 4)
;;  TRACE t1995: => 2
;;  TRACE t1996: (mod' 4 2)
;;  TRACE t1996: => 0
;;  => 2


;;  (take 5 (iterate macroexpand-1 '(gcd-normal 206 40)))
;;  =>
;;  ((gcd-normal 206 40)
;;   (sicp.chapter1/gcd-normal 40 (sicp.chapter1/mod' 206 40))
;;   (sicp.chapter1/gcd-normal (sicp.chapter1/mod' 206 40) (sicp.chapter1/mod' 40 (sicp.chapter1/mod' 206 40)))
;;   (sicp.chapter1/gcd-normal
;;     (sicp.chapter1/mod' 40 (sicp.chapter1/mod' 206 40))
;;     (sicp.chapter1/mod' (sicp.chapter1/mod' 206 40) (sicp.chapter1/mod' 40 (sicp.chapter1/mod' 206 40))))
;;   (sicp.chapter1/gcd-normal
;;     (sicp.chapter1/mod' (sicp.chapter1/mod' 206 40) (sicp.chapter1/mod' 40 (sicp.chapter1/mod' 206 40)))
;;     (sicp.chapter1/mod'
;;       (sicp.chapter1/mod' 40 (sicp.chapter1/mod' 206 40))
;;       (sicp.chapter1/mod' (sicp.chapter1/mod' 206 40) (sicp.chapter1/mod' 40 (sicp.chapter1/mod' 206 40))))))

;; there are around 21 mod' operations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn divides? [a b] (= (mod b a) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n] (find-divisor n 2))

(defn prime? [n] (= n (smallest-divisor n)))

(defn exp-mod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (mod (square (exp-mod base (/ exp 2) m)) m)
        :else (mod (* base (exp-mod base (- exp 1) m)) m)))

(defn fermet-test [n]
  (let [try-it (fn [a] (= (exp-mod a n n) a))]
    (try-it (+ 1 (rand-int (- n 1))))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermet-test n) (fast-prime? n (- times 1))
        :else false))

;; Exercise 1.21
(smallest-divisor 199)
;=> 199
(smallest-divisor 1999)
;=> 1999
(smallest-divisor 19999)
;=> 7