(ns sicp.chapter3.core
  (:require
   [clojure.math :as math]))

;; ex 3.1
(defn make-accumulator [acct]
  (let [res (atom acct)]
    (fn [amount]
      (swap-vals! res (partial + amount))
      @res)))

(def A (make-accumulator 5))

(A 10)
;;=> 15

(A 10)
;;=> 25

;; ex 3.2
(defn make-monitored [f]
  (let [cnt (atom 0)]
    (fn [& args]
      (prn f args)
      (if (= :how-many-calls? (first args))
        @cnt
        (do
          (swap-vals! cnt inc)
          (apply f args))))))

(def s (make-monitored math/sqrt))

(s 100.0)
;;=> 10.0

(s :how-many-calls?)
;;=> 1

(defprotocol IAccount
  (withdraw [this amount])
  (deposit [this amount]))

(defrecord Account [balance]
  IAccount
  (withdraw [this amount]
    (swap! (:balance this) #(- %1 amount)))
  (deposit [this amount]
    (swap! (:balance this) #(+ %1 amount))))

(defprotocol IPassword
  (unlock [this password]))

(defrecord PasswordProtectedAccount [^Account account password call-cops incorrect-password-count]
  IPassword
  (unlock [this p]
    (if (= p (:password this))
      (do
        (reset! (:incorrect-password-count this) 0)
        (:account this))
      (do
        (swap! (:incorrect-password-count this) inc)
        (if (>= @(:incorrect-password-count this) 7)
          (call-cops))
        (reify IAccount
          (withdraw [this amount] "Incorrect Password")
          (deposit [this amount] "Incorrect Password"))))))

(defn make-account [balance password call-cops]
  (->PasswordProtectedAccount (->Account (atom balance)) password call-cops (atom 0)))

;; ex 3.3
(def account (make-account 100 "secret-password" (fn [] (prn "called the cops"))))

(.. account (unlock "secret-password") (withdraw 40))
;;=> 60

(.. account (unlock "wrong-secret-password") (deposit 40))
;;=> "Incorrect Password"

(.. account (unlock "wrong-secret-password") (deposit 40))
;;=> "Incorrect Password"

(.. account (unlock "wrong-secret-password") (deposit 40))
;;=> "Incorrect Password"

(.. account (unlock "wrong-secret-password") (deposit 40))
;;=> "Incorrect Password"

(.. account (unlock "wrong-secret-password") (deposit 40))
;;=> "Incorrect Password"

(.. account (unlock "wrong-secret-password") (deposit 40))
;;=> "Incorrect Password"

(.. account (unlock "wrong-secret-password") (deposit 40))
;; "called the cops"
;;=> "Incorrect Password"
