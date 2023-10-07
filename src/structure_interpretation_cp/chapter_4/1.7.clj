(ns structure-interpretation-cp.chapter-4.1.7
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [structure-interpretation-cp.chapter-4.1 :refer [application?
                                                             apply-primitive-procedure assignment-value assignment-variable assignment? compound-procedure?
                                                             create-mapping define-variable! definition-value definition-variable definition?
                                                             extend-environment if-alternative if-consequent if-predicate if? lambda-body
                                                             lambda-parameters lambda? lookup-variable-value make-procedure operands operator
                                                             primitive-procedure? procedure-body procedure-env procedure-parameters quoted?
                                                             self-evaluating? set-variable-value! setup-environment tagged-list? text-of-quotation
                                                             variable?]]))

;; 4.1.7 Separating Syntactic Analysis from Execution

(declare analyze)

;; Here is the simplest syntactic analysis procedure, 
;; which handles self-evaluating expressions. 
;; It returns an execution procedure that ignores
;; its environment argument and just returns the expression:
(defn- analyze-self-evaluating [exp]
  (fn [env] exp))

;; For a quoted expression, we can gain a litlle efficiency by extracting the
;; text of the quotation only once, in the analysis phase, rather than in the
;; execution phase.
(defn- analyze-quoted [exp]
  (let [qval (text-of-quotation exp)]
    (fn [env] qval)))

;; Looking up a variable value must still be done in the execution phase,
;; since this depends upon knowing the environment.
(defn- analyze-variable [exp]
  (fn [env] (lookup-variable-value exp env)))


;; analyze-assignment also must defer actually setting the variable until the execution, 
;; when the environment has been supplied. However,
;; the fact that the assignment-value expression can be analyzed (recursively) 
;; during analysis is a major gain in efficiency, because the
;; assignment-value expression will now be analyzed only once.The same
;; holds true for definitions.
(defn- analyze-assignment [exp]
  (let [var (assignment-variable exp)
        vproc (analyze (assignment-value exp))]
    (fn [env]
      (set-variable-value! var (vproc env) env)
      :ok)))

(defn- analyze-definition [exp]
  (let [var (definition-variable exp)
        vproc (analyze (definition-value exp))]
    (fn [env]
      (define-variable! var (vproc env) env)
      :ok)))

;; For if expressions, we extract and analyze the predicate, consequent,
;; and alternative at analysis time.
(defn- analyze-if [exp]
  (let [pproc (analyze (if-predicate exp))
        cproc (analyze (if-consequent exp))
        aproc (analyze (if-alternative exp))]
    (fn [env] (if (true? (pproc env))
                (cproc env)
                (aproc env)))))

;; Analyzing a lambda expression also achieves a major gain in efficiency:
;; We analyze the lambda body only once, even though procedures resulting 
;; from evaluation of the lambda may be applied many times.
;; p. 537 (565)
(defn- analyze-lambda [exp]
  (let [vars (lambda-parameters exp)
        ;; differs from book which uses analyze-sequence
        bproc (analyze (lambda-body exp))]
    (fn [env] (make-procedure vars bproc env))))

;; To analyze an application, we analyze the operator and operands and
;; construct an execution procedure that calls the operator execution 
;; procedure (to obtain the actual procedure to be applied) and the operand
;; execution procedures (to obtain the actual arguments). We then pass
;; these to execute-application, which is the analog of apply in Section
;; 4.1.1. execute-application differs from apply in that the procedure
;; body for a compound procedure has already been analyzed, so there is
;; no need to do further analysis. Instead, we just call the execution 
;; procedure for the body on the extended environment.
(defn- execute-application [proc args]
  (cond (primitive-procedure? proc)
        (apply-primitive-procedure proc args)
        (compound-procedure? proc)
        ((procedure-body proc)
         (extend-environment
          (create-mapping (procedure-parameters proc)
                          args)
          (procedure-env proc)))
        :else
        (throw (RuntimeException.
                (str "Unknown procedure type: EXECUTE-APPLICATRION " proc)))))

(defn- analyze-application [exp]
  (let [fproc (analyze (operator exp))
        aprocs (map analyze (operands exp))]
    (fn [env]
      (execute-application
       (fproc env)
       (map (fn [aproc] (aproc env))
            aprocs)))))



(defn analyze [exp]
  (cond (self-evaluating? exp) (analyze-self-evaluating exp)
        (quoted? exp) (analyze-quoted exp)
        (variable? exp) (analyze-variable exp)
        (assignment? exp) (analyze-assignment exp)
        (definition? exp) (analyze-definition exp)
        (if? exp) (analyze-if exp)
        (lambda? exp) (analyze-lambda exp)
        (application? exp) (analyze-application exp)
        :else (throw (RuntimeException. (str "Unknown expression type: ANALYZE" exp)))))

(defn evaluate [exp env] ((analyze exp) env))

(deftest analyze-evaluate
  (testing "self"
    (let [env (setup-environment)]
      (is (= 1 (evaluate 1 env)))))
  (testing "if"
    (let [env (setup-environment)]
      (is (= 1 (evaluate (list :if :true 1 0) env)))
      (is (= 0 (evaluate (list :if :false 1 0) env)))))
  (testing "define variable"
    (let [env (setup-environment)]
      (evaluate (list :define :x 2) env)
      (is (= 2 (evaluate :x env)))))
  (testing "define function and call"
    (let [env (setup-environment)]
      (evaluate (list :define
                      (list :sq :x)
                      (list :* :x :x)) env)
      (is (= 9 (evaluate (list :sq 3) env)))))
;;   (testing "define function with if and call"
;;     (let [env (setup-environment)]
;;       (evaluate (list :define
;;                       (list :fact :x)
;;                       (list :if (list :eq? :x 1)
;;                             1
;;                             (list :* (list :fact
;;                                            (list :- :x 1))))) env)
;;       (is (= 6 (evaluate (list :fact 3) env)))))
  )

(run-tests)

