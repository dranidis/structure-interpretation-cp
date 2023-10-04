(ns structure-interpretation-cp.chapter-3.1-1
  (:require [clojure.test :refer [deftest is run-tests testing]]))

(def new-withdraw
  (let [balance (atom 100)]
    (fn [amount]
      (if (>= @balance amount)
        (do
          (reset! balance (- @balance amount))
          @balance)
        "Insuficcient funds"))))

(new-withdraw 10)

;; The following procedure, make-withdraw, creates “withdrawal processors.” 
;; The formal parameter balance in make-withdraw specifies the
;; initial amount of money in the account
(defn make-withdraw [initial-balance]
  (let [balance (atom initial-balance)]
    (fn [amount]
      (if (>= @balance amount)
        (do
          (reset! balance (- @balance amount))
          @balance)
        "Insuficcient funds"))))

(def W1 (make-withdraw 100))
(def W2 (make-withdraw 100))
(W1 10)
(W2 20)



;; Each call to make-account sets up an environment with a local state
;; variable balance. 
;; Within this environment, make-account defines procedures deposit and withdraw 
;; that access balance and an additional
;; procedure dispatch that takes a “message” as input and returns one of
;; the two local procedures. The dispatch procedure itself is returned as
;; the value that represents the bank-account object. This is precisely the
;; message-passing style of programming that we saw in Section 2.4.3, 
;; although here we are using it in conjunction with the ability to modify
;; local variables.
(defn make-account [initial-balance]
  (let [balance (atom initial-balance)]
    (letfn [(withdraw [amount]
              (if (>= @balance amount)
                (do
                  (reset! balance (- @balance amount))
                  @balance)
                "Insuficcient funds"))
            (deposit [amount]
              (reset! balance (+ @balance amount)))
            (dispatch [m]
              (cond (= m :withdraw) withdraw
                    (= m :deposit) deposit
                    :else (throw (RuntimeException.
                                  (str "Unknown request: make-account"
                                       m)))))]
      dispatch)))

(def acc (make-account 100))
((acc :withdraw) 50)
((acc :withdraw) 60)
((acc :deposit) 40)
(def acc2 (make-account 100))
((acc2 :withdraw) 30)

;; Exercise 3.1: An accumulator is a procedure that is called
;; repeatedly with a single numeric argument and accumulates its arguments into a sum. 
;; Each time it is called, it
;; returns the currently accumulated sum. Write a procedure
;; make-accumulator that generates accumulators, each maintaining an independent sum.
;; The input to make-accumulator
;; should specify the initial value of the sum.
(declare make-accumulator)

(deftest make-accumulator-test
  (testing "make-accumulator"
    (let [A (make-accumulator 5)]
      (is (= 15 (A 10)))
      (is (= 25 (A 10))))))

(defn make-accumulator [initial-sum]
  (let [sum (atom initial-sum)]
    (fn [number]
      (reset! sum (+ @sum number))
      @sum)))


;; Exercise 3.2: In software-testing applications, it is useful
;; to be able to count the number of times a given procedure
;; is called during the course of a computation. 
;; Write a procedure make-monitored that takes as input a procedure, f,
;; that itself takes one input. 
;; The result returned by make-monitored is a third procedure, say mf, that keeps track
;; of the number of times it has been called by maintaining
;; an internal counter. If the input to mf is the special symbol
;; how-many-calls?, then mf returns the value of the counter.
;; If the input is the special symbol reset-count, then mf resets the counter to zero. 
;; For any other input, mf returns the
;; result of calling f on that input and increments the counter.
(declare make-monitored)
(defn square [x] (* x x))

(deftest make-monitored-test
  (testing "make-monitored square"
    (let [s (make-monitored square)]
      (is (= 0 (s :how-many-calls?)))
      (is (= 100 (s 10)))
      (is (= 1 (s :how-many-calls?)))
      (is (= 25 (s 5)))
      (is (= 2 (s :how-many-calls?)))
      (is (= 0 (s :reset-count)))
      (is (= 0 (s :how-many-calls?))))))

(defn make-monitored [f]
  (let [call-count (atom 0)]
    (fn [arg]
      (if (keyword? arg)
        (cond (= arg :how-many-calls?) @call-count
              (= arg :reset-count) (do
                                     (reset! call-count 0)
                                     @call-count)
              :else (throw (RuntimeException. (str "Unknown message to make-monitored: "
                                                   arg))))

        (do
          (swap! call-count inc)
          (f arg))))))

(run-tests)