(ns structure-interpretation-cp.chapter-4.1
  (:require [clojure.pprint :as pprint]
            [clojure.test :refer [deftest is run-tests testing]]
            [structure-interpretation-cp.chapter-4.parse :as parse]))

;; implementation of frames as atoms of mappings
(defn make-frame [mapping-of-variables-to-values]
  (atom mapping-of-variables-to-values))

(defn get-value-from-frame [frame var]
  (get @frame var))

(defn frame-contains-var? [frame var]
  (contains? @frame var))

(defn add-binding-to-frame! [var val frame]
  (reset! frame (assoc @frame var val)))

(deftest frame-test
  (testing ""
    (let [a-frame (make-frame {:x 1})]
      (is (= 1 (get-value-from-frame a-frame :x)))
      (add-binding-to-frame! :y 2 a-frame)
      (is (= 2 (get-value-from-frame a-frame :y)))
      (add-binding-to-frame! :x 44 a-frame)
      (is (= 44 (get-value-from-frame a-frame :x))))))


;; an environment is a list of frames
(def the-empty-environment '())
(defn first-frame [env] (first env))
(defn enclosing-environment [env] (rest env))

(defn extend-environment [mapping-of-variables-to-values base-env]
  (cons (make-frame mapping-of-variables-to-values) base-env))

(declare print-env)

(defn lookup-variable-value [var env]
  (letfn [(env-loop [env]
            (letfn [(scan  [frame]
                      (let [val (get-value-from-frame frame var)]
                        (if (frame-contains-var? frame var)
                          val
                          (env-loop (enclosing-environment env)))))]
              (if (= env the-empty-environment)
                (throw (RuntimeException. (str "Unbound variable: " var)))
                (scan (first-frame env)))))]
    (try (env-loop env)
         (catch Exception e
           (println (.getMessage e) "Error in ENV: ")
           (print-env env)))))


;; 4.1.2 Representing Expressions

;; • The only self-evaluating items are numbers and strings:
(defn self-evaluating? [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

;; • Variables are represented by symbols:
(defn variable? [exp] (keyword? exp))

(defn tagged-list? [exp tag]
  (if (seq? exp)
    (= (first exp) tag)
    false))

;; • Quotations have the form (quote ⟨text-of-quotation⟩):
(defn quoted? [exp] (tagged-list? exp :quote))
(defn text-of-quotation [exp] (second exp))

;; • Assignments have the form (set! ⟨var⟩ ⟨value⟩):
(defn assignment? [exp] (tagged-list? exp :set!))
(defn assignment-variable [exp] (second exp))
(defn assignment-value [exp] (nth exp 2))

(deftest assignment-test
  (testing "assignment"
    (let [exp (list :set! :x 1)]
      (is (assignment? exp))
      (is (= :x (assignment-variable exp)))
      (is (= 1 (assignment-value exp))))))

(defn lambda? [exp] (tagged-list? exp :lambda))
(defn lambda-parameters [exp] (second exp))
(defn lambda-body [exp]
  (nth exp 2))
(defn make-lambda [parameters body]
  (list :lambda parameters body))



;; • Definitions have the form
;; (define ⟨var⟩ ⟨value⟩)
;; or the form
;; (define (⟨var⟩ ⟨parameter1⟩ . . . ⟨parametern⟩)
;;   ⟨body⟩)
;; The latter form (standard procedure definition) is syntactic sugar
;; for
;; (define ⟨var⟩
;;   (lambda (⟨parameter1⟩ . . . ⟨parametern⟩)
;;           ⟨body⟩))
(defn definition? [exp] (tagged-list? exp :define))
(defn definition-variable [exp]
  (if (keyword? (second exp))
    (second exp)
    (first (second exp))))
(defn definition-value [exp]
  (if (keyword? (second exp))
    (nth exp 2)
    (make-lambda (rest (second exp))
                 (rest (rest exp)))))

(deftest definition-test
  (testing "definition?"
    (is (definition? '(:define var))))
  (testing "definition-variable"
    (is (= :var1 (definition-variable (list :define :var1 :val))))
    (is (= :var1 (definition-variable (list :define (list :var1 :p1 :p2) :body)))))
  (testing "definition-value"
    (is (= 1 (definition-value '(:define :var1 1))))
    (is (= (list :lambda (list :p1 :p2) (list :body)) (definition-value (list :define (list :var1 :p1 :p2) :body))))))

;; • Conditionals begin with if and have a predicate, a consequent,
;; and an (optional) alternative. If the expression has no alternative
;; part, we provide false as the alternative

(defn if? [exp] (tagged-list? exp :if))
(defn if-predicate [exp] (second exp))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp]
  (if (= 3 (count exp))
    :false
    (nth exp 3)))

(deftest if-test
  (testing "if"
    (is (if? (list :if :pred :then)))
    (is (= :pred (if-predicate (list :if :pred :then))))
    (is (= :then (if-consequent (list :if :pred :then))))
    (is (= :false (if-alternative (list :if :pred :then))))
    (is (= :alt (if-alternative (list :if :pred :then :alt))))))

(defn and? [exp] (tagged-list? exp :and))

(defn make-procedure [parameters body env]
  (list :procedure parameters body env))

;; Exercise 4.6 
;; let expressions are derived expressions, because
;; (let ((⟨var1⟩ ⟨exp1⟩) . . . (⟨varn⟩ ⟨expn⟩))
;;   ⟨body⟩)
;; is equivalent to
;; ((lambda (⟨var1⟩ . . . ⟨varn⟩)
;;          ⟨body⟩)
;;  ⟨exp1⟩
;;  . . .
;;  ⟨expn⟩)

(defn let? [exp] (tagged-list? exp :let))



(defn compound-procedure? [p]
  (tagged-list? p :procedure))
(defn procedure-parameters [p]
  (second p))
(defn procedure-body [p]
  (nth p 2))
(defn procedure-env [p]
  (nth p 3))

(defn primitive-procedure? [proc]
  (tagged-list? proc :primitive))

(defn primitive-implementation [proc] (first (rest proc)))

(def apply-in-underlying-language clojure.core/apply)

(defn apply-primitive-procedure [proc args]
  (try (apply-in-underlying-language
        (primitive-implementation proc) args)
       (catch Exception e
         (str "apply-primitive-procedure"
              proc
              args
              (.getMessage e)))))


(defn set-variable-value! [var new-value env]
  (letfn [(env-loop [env]
            (letfn [(scan  [frame]
                      (let [old-value (get-value-from-frame frame var)]
                        (if (nil? old-value)
                          (env-loop (enclosing-environment env))
                          (add-binding-to-frame! var new-value frame))))]
              (if (= env the-empty-environment)
                (throw (Exception. (str "Unbound variable: " var)))
                (scan (first-frame env)))))]
    (env-loop env)))

(defn define-variable! [var val env]
  (add-binding-to-frame! var val (first-frame env)))

;; eval-sequence is used by apply to evaluate the sequence of expressions
;; in a procedure body and by eval to evaluate the sequence of expressions
;; in a begin expression. It takes as arguments a sequence of expressions
;; and an environment, and evaluates the expressions in the order in which
;; they occur. The value returned is the value of the final expression.
(declare evaluate)

(defn eval-if [exp env]
  (if (true? (evaluate (if-predicate exp) env))
    (evaluate (if-consequent exp) env)
    (evaluate (if-alternative exp) env)))

(defn and-conditions [exp]
  (rest exp))

(defn eval-and [conds env]
  (if (empty? conds)
    true
    (and (true? (evaluate (first conds) env))
         (eval-and (rest conds) env))))

;; sequences
(defn last-exp? [exps] (empty? (rest exps)))
(defn first-exp [exps] (first exps))
(defn rest-exps [exps] (rest exps))

(defn eval-sequence [exps env]
  (cond (last-exp? exps) (evaluate (first-exp exps) env)
        :else (do (evaluate (first-exp exps) env)
                  (eval-sequence (rest-exps exps) env))))

;; assignments and definitions
(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (evaluate (assignment-value exp) env)
                       env)
  :ok)

(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
    (evaluate (definition-value exp) env)
    env)
  :ok)

(defn create-mapping
  ([vars vals] (create-mapping vars vals {}))
  ([vars vals mapping]
   (if (empty? vars)
     mapping
     (create-mapping (rest vars) (rest vals) (assoc mapping (first vars) (first vals))))))

(create-mapping (list :x :y) (list 3 4))

(defn apply-2 [procedure arguments]
  (cond (primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
;; I have a problem with the
;; following
;; if the body is (list :+ :x :y)
;; the eval-squence returns the value of :y
        (eval-sequence
        ;; (evaluate
         (procedure-body procedure)
         (extend-environment
          (create-mapping (procedure-parameters procedure)
                          arguments)
          (procedure-env procedure)))
        :else
        (throw (RuntimeException. (str "Unknown procedure type: APPLY " procedure arguments)))))


;; When eval processes a procedure application, it uses list-of-values
;; to produce the list of arguments to which the procedure is to be applied.
;; list-of-values takes as an argument the operands of the combination. 
;; It evaluates each operand and returns a list of the corresponding
;; values:
;; (defn list-of-values [exps env]
;;   (if (no-operands? exps)
;;     '()
;;     (cons (evaluate (first-operand exps) env)
;;           (list-of-values (rest-operands exps) env))))

(defn list-of-values [exps env]
  (map #(evaluate % env) exps))

(def lov (list-of-values (list :x :y) (list (make-frame {:x 2, :y 3}))))
lov


(deftest list-of-values-test
  (testing "some values"
    (is (= (list 2 3) (list-of-values (list :x :y) (list (make-frame {:x 2, :y 3})))))))

(defn application? [exp] (coll? exp))
(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))

;; 4.1.1 The Core of the Evaluator
;; The evaluation process can be described as the interplay between two
;; procedures: eval and apply.
;; eval takes as arguments an expression and an environment. 
;; It classifies the expression and directs its evaluation. 
;; eval is structured as a case
;; analysis of the syntactic type of the expression to be evaluated. 
;; In order to keep the procedure general, we express the determination of the
;; type of an expression abstractly, 
;; making no commitment to any particular representation 
;; for the various types of expressions. Each type of
;; expression has a predicate that tests for it and an abstract means for
;; selecting its parts. This abstract syntax makes it easy to see how we can
;; change the syntax of the language by using the same evaluator, but with
;; a different collection of syntax procedures.

;; called eval in the book
;; p 496
(defn evaluate [exp env]
  ;; (println "EVAL: " exp)
  (cond
  ;;   Primitive expressions
  ;;   • For self-evaluating expressions, such as numbers, eval returns
  ;;   the expression itself.
    (self-evaluating? exp) exp
  ;;   • eval must look up variables in the environment to find their values.
    (variable? exp) (lookup-variable-value exp env)
    ;; • For quoted expressions, eval returns the expression that was quoted.
    (quoted? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (and? exp) (eval-and (and-conditions exp) env)
    (lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env)
    (application? exp) (apply-2 (evaluate (operator exp) env)
                                (list-of-values (operands exp) env))
    :else (throw (Exception. (str "Unknown expression type: " exp env)))))


(def primitives
  {:car (list :primitive first)
   :cdr (list :primitive #(if (empty? (rest %)) nil  (rest %)))
   :cons (list :primitive (fn [a b] (cons a (list b))))
   :null? (list :primitive nil?)
   :+ (list :primitive +)
   :- (list :primitive -)
   :* (list :primitive *)
   :/ (list :primitive /)
   :eq? (list :primitive =)
   :not (list :primitive not)
   :list (list :primitive list)})

(defn setup-environment []
  (let [initial-env (extend-environment primitives the-empty-environment)]
    (define-variable! :true true initial-env)
    (define-variable! :false false initial-env)
    initial-env))

(deftest setup-evaluate
  (testing "self"
    (is (= 1 (evaluate 1 (setup-environment)))))
  (testing "define variable"
    (let [env (setup-environment)]
      (evaluate (list :define :x 2) env)
      (is (= 2 (evaluate :x env))))))


(def the-global-environment (setup-environment))

(defn prompt-for-input [str]
  (println str))
(defn announce-output [str]
  (println str))

(defn user-print [object]
  (if (compound-procedure? object)
    (println (list :compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (println object)))

(def input-prompt ";;; M-Eval input:")
(def output-prompt ";;; M-Eval value:")

(declare print-env)

(defn print-frame-keyval [[key val]]
  (print key " --> ")
  (user-print val))

(defn print-frame [frame]
  (println "env")
  (doseq [keyval @frame] (print-frame-keyval keyval)))

(defn print-env
  ([env]
   (if (empty? env)
     :done
     (do
       (print-frame (first env))
       (print-env (rest env))))))

(defn read-lines-till-empty []
  (let [line (read-line)]
    (if (= line "")
      ""
      (str line (read-lines-till-empty)))))

(defn driver-loop []
  (print-env the-global-environment)
  (prompt-for-input input-prompt)
  (letfn [(input [] (let [line (read-line)]
                      (if (= line "")
                        ""
                        (str line (input)))))]
    (let [output (evaluate (parse/parse (input)) the-global-environment)]
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; (driver-loop)




;; (run-tests)


(comment
  (and)
  ;; (define x 1)
  ;; (define (add1 y) (+ x y))
  ;; (define (factorial x) (if (eq? x 1) 1 (* x (factorial (- x 1)))))
  ;; (factorial 1)
  ;; (add 3 5)
  ;; (if (eq? x 1) 5 10)
  the-empty-environment
  (first-frame the-global-environment)

  (def an-exp (list :define (list :sq :x) (list :* :x :x)))
  (definition? an-exp)
  (definition-variable an-exp)
  (definition-value an-exp)


  (define-variable! (definition-variable an-exp)
    (evaluate (definition-value an-exp) the-global-environment)
    the-global-environment)

  (add-binding-to-frame! :sq (evaluate (definition-value an-exp) the-global-environment) (first-frame the-global-environment))


  (def l-exp (list :lambda (list :x) (list :* :x :x)))
  l-exp
  (lambda? l-exp)
  (lambda-parameters l-exp)
  (lambda-body l-exp)

  (make-procedure (lambda-parameters l-exp)
                  (lambda-body l-exp)
                  the-global-environment)

  (evaluate (definition-value an-exp) the-global-environment)
  (evaluate an-exp the-global-environment)

  1)



;; (print-env the-global-environment)

;; (pprint/pprint the-global-environment)


