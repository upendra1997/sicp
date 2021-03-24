(ns sicp.chapter1-test
  (:require [clojure.test :refer :all]
            [sicp.chapter1 :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest sum-of-squares-of-largest-two-test
  (testing "pass when smallest is first argument"
    (is (= (sum-of-squares-of-largest-two 1 2 3) 13)))
  (testing "pass when smallest is second argument"
    (is (= (sum-of-squares-of-largest-two 2 1 3) 13)))
  (testing "pass when smallest is third argument"
    (is (= (sum-of-squares-of-largest-two 2 3 1) 13))))

(deftest a-plus-abs-b-test
  (testing "pass when b is negative"
    (is (= (a-plus-abs-b 1 (- 2)) 3)))
  (testing "pass when b is positive"
    (is (= (a-plus-abs-b 1 2) 3)))
  (testing "pass when b is zero"
    (is (= (a-plus-abs-b 1 0) 1))))

(defspec fast-mul-is-correct 100
  (prop/for-all [a gen/nat
                 b gen/nat]
                (== (fast-mul a b) (* a b))))