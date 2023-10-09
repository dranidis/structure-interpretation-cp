(ns structure-interpretation-cp.1-test
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [structure-interpretation-cp.chapter-4.1 :refer [define-variable!
                                                             evaluate
                                                             extend-environment lambda-body lambda-parameters lambda? let->lambda-application let-body let-exps
                                                             let-vars let? set-variable-value! setup-environment the-empty-environment]]))

(deftest lambda-test
  (testing "lambda?"
    (is (lambda? (list :lambda (list :p1 :p2) :body))))
  (testing "lambda-parameters"
    (is (= (list :p1 :p2) (lambda-parameters (list :lambda (list :p1 :p2) :body)))))
  (testing "lambda-body"
    (is (= (list :body) (lambda-body (list :lambda (list :p1 :p2) (list :body)))))
    (is (= (list :body1 :body2) (lambda-body (list :lambda (list :p1 :p2) (list :body1 :body2)))))))

(deftest let-test
  (let [one-let (list :let (list
                            (list :var1 :exp1))
                      :body)
        two-let (list :let
                      (list
                       (list :var1 :exp1)
                       (list :var2 :exp2))
                      :body1
                      :body2)]
    (testing "let?"
      (is (let? (list :let (list :var1 :exp1)))))
    (testing "let-vars"
      (is (= (list :var1) (let-vars one-let)))
      (is (= (list :var1 :var2) (let-vars two-let))))
    (testing "let-exps"
      (is (= (list :exp1) (let-exps one-let)))
      (is (= (list :exp1 :exp2) (let-exps two-let))))
    (testing "let-body"
      (is (= (list :body) (let-body one-let)))
      (is (= (list :body1 :body2) (let-body two-let))))
    (testing "let->lambda-1"
      (is (= (list (list :lambda
                         (list :var1)
                         (list :body))
                   :exp1)
             (let->lambda-application one-let))))
    (testing "let->lambda-2"
      (is (= (list (list :lambda
                         (list :var1 :var2)
                         (list :body1 :body2))
                   :exp1 :exp2)
             (let->lambda-application two-let))))))

;; test for evaluate
(deftest evaluate-test
  (testing "evaluate self-evaluating"
    (is (= (evaluate 1 nil) 1))
    (is (= (evaluate "a string" nil) "a string")))
  (testing "evaluate variable"
    (let [env (->> the-empty-environment
                   (extend-environment {:y 2})
                   (extend-environment {:x 10}))]
      (is (= (evaluate :x env) 10))
      (is (= (evaluate :y env) 2))))
  (testing "evaluate quote"
    (is (= (evaluate '(:quote 12) nil) 12))))

(deftest set-variable-test
  (testing "setting variable"
    (let [env (->> the-empty-environment
                   (extend-environment {:y 2})
                   (extend-environment {:x 10}))
          _ (set-variable-value! :x 11 env)]
      (is (= (evaluate :x env) 11))
      (is (= (evaluate :y env) 2)))))

(deftest define-variable-test
  (testing "defining a variable changes the variable in the first frame if it already exists"
    (let [env (->> the-empty-environment
                   (extend-environment {:y 2})
                   (extend-environment {:x 10}))
          _ (define-variable! :x 11 env)]
      (is (= (evaluate :x env) 11))
      (is (= (evaluate :y env) 2))))
  (testing "defining a variable adds it to the first frame if it does not exist"
    (let [env (->> the-empty-environment
                   (extend-environment {:y 2})
                   (extend-environment {:x 10}))
          _ (define-variable! :y 11 env)]
      (is (= (evaluate :x env) 10))
      (is (= (evaluate :y env) 11)))))

(deftest evaluate-integration
  (testing "application of function"
    (let [env (setup-environment)
          fdef (list :define
                     (list :sq :x)
                     (list :* :x :x))
          fappl (list :sq 3)]
      (evaluate fdef env)
      (is (= 9 (evaluate fappl env)))))
  (testing "application of function with body with two statements"
    (let [env (setup-environment)
          fdef (list :define
                     (list :sq :x)
                     0
                     (list :* :x :x))
          fappl (list :sq 4)]
      (evaluate fdef env)
      (is (= 16 (evaluate fappl env))))))

(run-tests)