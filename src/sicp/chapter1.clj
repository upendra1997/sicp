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

(def ^:dynamic **tolerance** 0.00001)

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) **tolerance**))

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
  (let [next-divisor (fn [test-divisor]
                       (if (even? test-divisor)
                         (+ 1 test-divisor)
                         (+ 2 test-divisor)))]
    (cond (> (square test-divisor) n) n
          (divides? test-divisor n) test-divisor
          :else (find-divisor n (next-divisor test-divisor)))))

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

;; Exercise 1.22
(defn search-for-primes [list-of-primes]
  (filter prime? list-of-primes))

(time (take 3 (search-for-primes (iterate inc 1000))))
;"Elapsed time: 0.064145 msecs"
;=> (1009 1013 1019)
(time (take 3 (search-for-primes (iterate inc 10000))))
;"Elapsed time: 0.035271 msecs"
;=> (10007 10009 10037)
(time (take 3 (search-for-primes (iterate inc 100000))))
;"Elapsed time: 0.117023 msecs"
;=> (100003 100019 100043)

(time (search-for-primes '(1009 1013 1019)))
(time (search-for-primes '(10007 10009 10037)))
(time (search-for-primes '(100003 100019 100043)))

;"Elapsed time: 0.024031 msecs"
;=> (1009 1013 1019)
;"Elapsed time: 0.022534 msecs"
;=> (10007 10009 10037)
;"Elapsed time: 0.023422 msecs"
;=> (100003 100019 100043)

;; timing is not matching out, as the \sqrt(n) is the complexity of checking prime but it does not tell how many primes
;; it have to check before finding all three primes.

;; Exercise 1.23
;; implemented next-divisor
(time (take 3 (search-for-primes (iterate inc 1000))))
;"Elapsed time: 0.037455 msecs"
;=> (1009 1013 1019)
(time (take 3 (search-for-primes (iterate inc 10000))))
;"Elapsed time: 0.036846 msecs"
;=> (10007 10009 10037)
(time (take 3 (search-for-primes (iterate inc 100000))))
;"Elapsed time: 0.032178 msecs"
;=> (100003 100019 100043)

(time (search-for-primes '(1009 1013 1019)))
(time (search-for-primes '(10007 10009 10037)))
(time (search-for-primes '(100003 100019 100043)))

;"Elapsed time: 0.024031 msecs"
;=> (1009 1013 1019)
;"Elapsed time: 0.022534 msecs"
;=> (10007 10009 10037)
;"Elapsed time: 0.023422 msecs"
;=> (100003 100019 100043)

;; time is almost halved; the other inconsistency can be due to jvm?

;; Exercise 1.24
(time (map #(fast-prime? %1 100) '(1009 1013 1019)))
(time (map #(fast-prime? %1 100) '(10007 10009 10037)))
(time (map #(fast-prime? %1 100) '(100003 100019 100043)))

;"Elapsed time: 0.098246 msecs"
;=> (true true true)
;"Elapsed time: 0.10523 msecs"
;=> (true true true)
;"Elapsed time: 0.618247 msecs"
;=> (true true true)

;; Exercise 1.25
; this would give correct answer, but it would be needlessly slow.
; as numbers might get very big and would need extra time to calculate exp
; which can be reduced as we only need modulus of answer.

;; Exercise 1.26
; This would make it O(n) as we are calling expmod two times.

;; Exercise 1.27
(defn fermet-test' [n]
  (let [try-it (fn [a] (= (exp-mod a n n) a))]
    (every? try-it (range 1 (- n 1)))))

(= (fermet-test' 561) (prime? 561))
(= (fermet-test' 1105) (prime? 1105))
(= (fermet-test' 1729) (prime? 1729))
(= (fermet-test' 2465) (prime? 2465))
(= (fermet-test' 2821) (prime? 2821))
(= (fermet-test' 6601) (prime? 6601))

;=> #'sicp.chapter1/fermet-test'
;=> false
;=> false
;=> false
;=> false
;=> false
;=> false

;; Exercise 1.28
(defn non-trivial-sqrt? [n m]
  (cond (= n 1) false
        (= n (- m 1)) false
        :else (= (mod (square n) m) 1)))

(defn exp-mod' [base exp m]
  (cond (= exp 0) 1
        (even? exp) (let
                     [result (exp-mod' base (/ exp 2) m)]
                      (if (non-trivial-sqrt? result m)
                        1
                        (mod (square result) m)))
        :else (mod (* base (exp-mod' base (- exp 1) m)) m)))

(defn miller-rabin [n]
  (let [try-it (fn [a] (let [result (exp-mod' a (- n 1) n)] (and (not (= result 0)) (= result 1))))]
    (every? try-it (range 2 (min (- n 1) 100)))))

(= (miller-rabin 561) (prime? 561))
(= (miller-rabin 1105) (prime? 1105))
(= (miller-rabin 1729) (prime? 1729))
(= (miller-rabin 2465) (prime? 2465))
(= (miller-rabin 2821) (prime? 2821))
(= (miller-rabin 6601) (prime? 6601))
(= (miller-rabin 4) (prime? 4))
(= (miller-rabin 5) (prime? 5))
(= (miller-rabin 3) (prime? 3))
(= (miller-rabin 9) (prime? 9))
(every? #(= (miller-rabin %1) (prime? %1)) (range 3 10000))

(defn sum [term a next b]
  (if (> a b) 0 (+ (term a) (sum term (next a) next b))))

;; Exercise 1.30
(defn sum [term a next b]
  (let [iter (fn [a result] (if
                             (> a b)
                              result
                              (recur (next a) (+ result (term a)))))]
    (iter a 0)))

(defn sum-integers [a b] (sum identity a inc b))
(defn sum-cubes [a b] (sum cube a inc b))

(defn pi-sum [a b] (sum
                    #(/ 1.0 (* %1 (+ %1 2)))
                    a
                    #(+ 4 %1)
                    b))

(* 8 (pi-sum 1 100000000))

(defn integral [f a b dx]
  (* dx (sum
         f
         (+ a (/ dx 2.0))
         #(+ dx %1)
         b)))

;; Exercise 1.29
(defn integral [f a b n]
  (let [h (/ (- b a) (* 2 n))
        add-2h #(+ %1 h h)]
    (* (/ h 3.0)
       (+
        (f a)
        (* 2 (sum f a add-2h b))
        (* 4 (sum f (+ h a) add-2h b))
        (f b)))))

(integral cube 0 1 1000)

;; Exercise 1.31
(defn product [term a next b]
  (if (> a b) 1 (* (term a) (product term (next a) next b))))

(defn factorial [n] (product identity 1 inc n))
(factorial 10)

(defn product [term a next b]
  (let [iter (fn [a result] (if
                             (> a b)
                              result
                              (recur (next a) (* result (term a)))))]
    (iter a 1)))

(defn factorial [n] (product identity 1 inc n))
(factorial 10)

(defn pi [n] (let [term (fn [n] (* (/ (* 2 n)
                                      (- (* 2 n) 1))
                                   (/ (* 2 n)
                                      (+ (* 2 n) 1))))]
               (* 2 (product term 1.0 inc n))))

(pi 100000)

;; Exercise 1.32
(defn accumulate [combiner empty term a next b]
  (if (> a b) empty (combiner
                     (term a)
                     (accumulate
                      combiner
                      empty
                      term
                      (next a)
                      next
                      b))))

(defn sum-integers [a b] (accumulate + 0 identity a inc b))
(sum-integers 0 100)

(defn accumulate [combiner empty term a next b]
  (let [iter (fn [a result] (if
                             (> a b)
                              result
                              (recur
                               (next a)
                               (combiner result (term a)))))]
    (iter a empty)))

(defn sum-integers [a b] (accumulate + 0 identity a inc b))
(sum-integers 0 100)

(defn filtered-accumulate [combiner pred empty term a next b]
  (let [iter (fn [a result] (cond
                              (> a b) result
                              :else (recur
                                     (next a)
                                     (combiner
                                      result
                                      (if (pred a) (term a) empty)))))]
    (iter a empty)))

(defn sum-square-primes [a b] (filtered-accumulate
                               +
                               prime?
                               0
                               #(* %1 %1)
                               a
                               inc
                               b))

(=
 (sum-square-primes 2 10)
 (reduce + (map #(* %1 %1) (filter prime? (range 2 10)))))

(defn product-relatively-prime [n] (filtered-accumulate
                                    *
                                    #(= 1 (gcd-applicative %1 n))
                                    1
                                    identity
                                    1
                                    inc
                                    n))

(= (product-relatively-prime 10)
   (reduce * (filter #(= 1 (gcd-applicative %1 10)) (range 1 10))))

;; Exercise 1.34

(defn f [g] (g 2))
;=> #'sicp.chapter1/f
(f square)
;=> 4
(f #(* %1 (+ %1 1)))
;=> 6
#_(f f)
;Execution error (ClassCastException) at sicp.chapter1/f (chapter1.clj:1).
;java.lang.Long cannot be cast to clojure.lang.IFn

(defn close-enough? [x y] (< (abs (- x y)) **tolerance**))

(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond (pos? test-value) (recur f neg-point midpoint)
              (neg? test-value) (recur f midpoint pos-point)
              :else midpoint)))))

(defn half-interval [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond
      (and (neg? a-value) (pos? b-value)) (search f a b)
      (and (neg? b-value) (pos? a-value)) (search f b a)
      :else (throw (Exception. "Values are opposite sign")))))

(half-interval sine 2.0 4.0)
(half-interval #(- (* %1 %1 %1) (* 2 %1) 3) 1.0 2.0)

(defn fixed-point
  ([f first-guess cnt]
   (defn try-guess [guess n]
     (let [next (f guess)]
       (println "guess" guess)
       (cond
         (= n cnt) (throw (ex-info "Could not guess" {:guess  guess}))
         (close-enough? guess next) next
         :else (recur next (+ n 1)))))
   (try-guess first-guess 0))
  ([f first-guess]
   (fixed-point f first-guess 100)))

(fixed-point #(Math/cos %1) 1.0)
(fixed-point #(+ (Math/sin %1) (Math/cos %1)) 1.0)
(defn sqrt [x] (fixed-point #(average %1 (/ x %1)) 1.0))
(sqrt 2)

;; Exercise 1.35
(def phi (fixed-point #(+ 1 (/ 1.0 %1)) 1.0))

;; Exercise 1.36
(println (fixed-point #(/ (Math/log 1000) (Math/log %1)) 2)) ;; 35 steps
(println (fixed-point #(average %1 (/ (Math/log 1000) (Math/log %1))) 2)) ;; 10 steps

;; Exercise 1.37
(defn cont-frac
  ([n d k] (cont-frac n d k 0))
  ([n d k cnt]
   (cond
     (= k cnt) (+ (d cnt) (/ (n cnt) (d cnt)))
     :else (/ (n cnt) (+ (d cnt) (cont-frac n d k (+ 1 cnt)))))))

(cont-frac (constantly 1.0) (constantly 1.0) 10)

(def ^:dynamic golden-ratio 1.61803398875M)

(->> (range 1 1000)
     (map (partial cont-frac (constantly 1.0) (constantly 1.0)))
     (take-while #(not (close-enough? (/ 1.0 golden-ratio) %1)))
     (count))
;; => 11


;; Excercise 1.38
(def ones (repeat 1))

(def two-table (iterate (partial + 2) 2))

(def d-series
  (->> (interleave two-table ones ones)
       (cons 1)))

(->> (range 2 10)
     (map (partial cont-frac (constantly 1.0) (partial nth d-series)))
     (take-while #(not (close-enough? 2.71828M (+ 2 %1))))
     (last)
     (+ 2))
;; => 2.718031032637774


;; Excercise 1.39
(defn tan [x k]
  (let [n-series (cons x (repeat (- (* x x))))
        d-series (iterate (partial + 2) 1)]
    #_(map #(identity [%1 %2]) n-series d-series)
    (cont-frac #(nth n-series %1) #(nth d-series %1) k)))

(double (tan 100 200))
;; => -0.5872139151569291


(Math/tan 100)
;; => -0.5872139151569291

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn average-damp [f]
  (fn [x] (average x (f x))))

((average-damp square) 10)
;; => 55


(defn sqrt [x]
  (fixed-point (average-damp #(/ x %1)) 1.0))

(sqrt 2.0)
;; => 1.4142135623746899


(defn cube-root [x]
  (fixed-point (average-damp #(/ x (square %1))) 1.0))

(cube-root 28)
;; => 3.0365861982520808


(defn derive [f]
  (fn [& args]
    (/
     (- (apply f (map (partial + **tolerance**) args)) (apply f args))
     **tolerance**)))

((derive cube) 5)
;; => 75.00014999664018

(defn newton-transform [f]
  (fn [x]
    (- x (/ (f x) ((derive f) x)))))

(defn newton-method [f guess]
  (fixed-point (newton-transform f) guess))

(defn sqrt [x]
  (newton-method #(- (square %1) x) 1.0))

(sqrt 2.0)
;; => 1.4142135623822438

(defn fixed-point-of-transform [f transform guess]
  (fixed-point (transform f) guess))

(defn sqrt [x]
  (fixed-point-of-transform #(/ x %1) average-damp 1.0))

(sqrt 2.0)
;; => 1.4142135623746899

(defn sqrt [x]
  (fixed-point-of-transform #(- (square %1) x) newton-transform 1.0))

(sqrt 2.0)
;; => 1.4142135623822438


;; Excercise 1.40

(defn cubic [a b c]
  (fn [x]
    (+ (* x x x) (* a x x) (* b x) c)))

#_(newton-method (cubic 4 1 1) 1.0)
;; => Execution error (ExceptionInfo) at sicp.chapter1/fixed-point$try-guess (chapter1.clj:1175).
;;    Could not guess

(newton-method (cubic 1 1 1) 1.0)
;; => -0.9999999999997796


;; Excercise 1.41
(defn dble [f]
  (fn [x]
    (f (f x))))

(((dble (dble dble)) inc) 5)
;; => 21


;; Excercise 1.42
((comp square inc) 6)
;; => 49


;; Excercise 1.43
(defn repeated [f n]
  (fn [x]
    (last (take (+ 1 n) (iterate f x)))))

((repeated square 2) 5)
;; => 625


;; Excercise 1.44
(defn mean [coll]
  (/ (reduce + 0 coll) (count coll)))

(defn smooth [f]
  (fn [x]
    (mean
     (f (- x **tolerance**))
     (f x)
     (f (+ x **tolerance**)))))

(defn n-fold-smooth [f n]
  (repeated smooth n))


;; Exercise 1.45
(defn nth-root-damped
  ([x nth damp]
   (fixed-point
    ((repeated average-damp damp) #(/ x (Math/pow %1 (- nth 1))))
    1.0))
  ([x nth]
   (let [damp (int (Math/floor (/ (Math/log10 nth) (Math/log10 2))))]
     (nth-root-damped x nth damp))))

(def result
  (for [nth (range 2 64)
        damp (range 1 10)]
    (try
      {:guess (nth-root-damped 13 nth damp)
       :nth nth
       :damp damp}
      (catch clojure.lang.ExceptionInfo e
        (merge {:error (:data (Throwable->map e))}
               {:nth nth :damp damp})))))

(->> result
     (filter (comp not :error))
     (group-by :nth)
     (map (fn [[k v]] (apply (partial min-key :damp) v)))
     (concat)
     (sort-by :nth))
;; => ({:guess 3.6055512754639905, :nth 2, :damp 1}
;;     {:guess 2.3513320063295247, :nth 3, :damp 1}
;;     {:guess 1.898828922115942, :nth 4, :damp 2}
;;     {:guess 1.6702759745502398, :nth 5, :damp 2}
;;     {:guess 1.533409125204947, :nth 6, :damp 2}
;;     {:guess 1.4425669097232419, :nth 7, :damp 2}
;;     {:guess 1.3779800151366282, :nth 8, :damp 3}
;;     {:guess 1.329754727118197, :nth 9, :damp 3}
;;     {:guess 1.2923915772110441, :nth 10, :damp 3}
;;     {:guess 1.2626028430787632, :nth 11, :damp 3}
;;     {:guess 1.2383056058801811, :nth 12, :damp 3}
;;    {:guess 1.218117810596386, :nth 13, :damp 3}
;;     {:guess 1.201063597550211, :nth 14, :damp 3}
;;     {:guess 1.1864911164364123, :nth 15, :damp 3}
;;     {:guess 1.1738739350270155, :nth 16, :damp 4}
;;     {:guess 1.1628563336988473, :nth 17, :damp 4}
;;     {:guess 1.153150602202086, :nth 18, :damp 4}
;;     {:guess 1.1445324720451917, :nth 19, :damp 4}
;;     {:guess 1.1368359631566203, :nth 20, :damp 4}
;;     {:guess 1.129910498368128, :nth 21, :damp 4}
;;     {:guess 1.123655795155952, :nth 22, :damp 4}
;;     {:guess 1.1179780042236576, :nth 23, :damp 4}
;;    {:guess 1.1127952408198776, :nth 24, :damp 4}
;;     {:guess 1.1080491760348847, :nth 25, :damp 4}
;;     {:guess 1.1036783996182629, :nth 26, :damp 4}
;;     {:guess 1.0996596446353006, :nth 27, :damp 4}
;;     {:guess 1.0959356147579495, :nth 28, :damp 4}
;;     {:guess 1.0924797061886298, :nth 29, :damp 4}
;;     {:guess 1.0892638641829306, :nth 30, :damp 4}
;;     {:guess 1.0862597285041695, :nth 31, :damp 5}
;;     {:guess 1.083454629849435, :nth 32, :damp 5}
;;     {:guess 1.0808261729590432, :nth 33, :damp 5}
;;     {:guess 1.0783580370493482, :nth 34, :damp 5}
;;    {:guess 1.0760359256809549, :nth 35, :damp 5}
;;     {:guess 1.0738472917249036, :nth 36, :damp 5}
;;     {:guess 1.071781107234561, :nth 37, :damp 5}
;;     {:guess 1.0698276676433345, :nth 38, :damp 5}
;;     {:guess 1.067978421783664, :nth 39, :damp 5}
;;     {:guess 1.0662258210804825, :nth 40, :damp 5}
;;     {:guess 1.0645566123324288, :nth 41, :damp 5}
;;     {:guess 1.0629746309237527, :nth 42, :damp 5}
;;     {:guess 1.0614672777837049, :nth 43, :damp 5}
;;     {:guess 1.0600252506381391, :nth 44, :damp 5}
;;    {:guess 1.0586559091428376, :nth 45, :damp 5}
;;     {:guess 1.0573461053785946, :nth 46, :damp 5}
;;     {:guess 1.056087920015785, :nth 47, :damp 5}
;;     {:guess 1.05489180771415, :nth 48, :damp 5}
;;     {:guess 1.053743569993768, :nth 49, :damp 5}
;;     {:guess 1.052634252738676, :nth 50, :damp 5}
;;     {:guess 1.0515827549327312, :nth 51, :damp 5}
;;     {:guess 1.0505588985637766, :nth 52, :damp 5}
;;     {:guess 1.049582508888288, :nth 53, :damp 5}
;;     {:guess 1.048648998313552, :nth 54, :damp 5}
;;     {:guess 1.047743634094918, :nth 55, :damp 5}
;;     {:guess 1.046871811011422, :nth 56, :damp 5}
;;    {:guess 1.046023098273773, :nth 57, :damp 5}
;;     {:guess 1.0452200864453416, :nth 58, :damp 5}
;;     {:guess 1.0444283188731014, :nth 59, :damp 5}
;;     {:guess 1.0436802048874596, :nth 60, :damp 5}
;;     {:guess 1.0429493644133796, :nth 61, :damp 5}
;;     {:guess 1.0422426554603408, :nth 62, :damp 5}
;;    {:guess 1.0415537939042658, :nth 63, :damp 6})

;; Relationship seems to be min-damp = log2 nth

(nth-root-damped 11 3)
;; => 2.22398319382933


;; Exercise 1.46
(defn iterative-improve [good-enough? iterative-improve]
  (fn [guess]
    (if (good-enough? guess)
      guess
      (recur (iterative-improve guess)))))

(defn sqrt [x]
  ((iterative-improve
    (fn [guess]
      (good-enough? guess x))
    (fn [guess]
      (improve guess x)))
   1.0))

(sqrt 11.0)

(defn iterative-fixed-point [f first-guess]
  (defn close-enough? [guess next]
    (< (abs (- guess next))
       **tolerance**))

  (defn improve-guess [guess]
    (f guess))

  (defn compute [guess]
    (let [next (improve-guess guess)]
      (if (close-enough? guess next)
        next
        (recur next))))

  (compute first-guess))

(defn sqrt-fixed-point [x]
  (iterative-fixed-point
   (average-damp #(/ x %1))
   1.0))

(sqrt-fixed-point 11.0)
;; => 3.3166247903554
