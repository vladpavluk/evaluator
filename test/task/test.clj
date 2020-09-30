(ns task.test
  (:require [clojure.test :refer :all]
            [task.core :refer :all]))

(deftest optimize-test

  (testing "Symbols should not be optimized"
    (is (= (optimize 'random-symbol) 'random-symbol)))

  (testing "Optimizing single + form"
    (is (= (optimize '(+ 5 10)) '(+ 5 10)) "nothing to optimize")
    (is (= (optimize '(+ 5 10 0)) '(+ 5 10)) "remove extra zero value")
    (is (= (optimize '(+ 5 0 0)) 5) "one valuable operand should return itself"))

  (testing "Optimizing single - form"
    (is (= (optimize '(- 5 10)) '(- 5 10)) "nothing to optimize")
    (is (= (optimize '(- 5 10 0)) '(- 5 10)) "remove extra zero value")
    (is (= (optimize '(- 5 0 0)) 5) "one valuable operand should return itself")

    (is (= (optimize '(- 5 5 0)) 0) "two operands with same value should yield 0, if nothing else specified")
    (is (= (optimize '(- 5 5 3)) '(- 5 5 3)) "two operands with same value should yield 0, if nothing else specified"))

  (testing "Optimizing single * form"
    (is (= (optimize '(* 5 10)) '(* 5 10)) "nothing to optimize")
    (is (= (optimize '(* 5 0 2 0)) 0) "at least one 0 should yield 0")
    (is (= (optimize '(* 5 1 2 1)) '(* 5 2)) "unnecessary 1s should be removed"))

  (testing "Optimizing single * form"
    (is (= (optimize '(/ 5 10)) '(/ 5 10)) "nothing to optimize")
    (is (= (optimize '(/ 5 1 2 1)) '(/ 5 2)) "unnecessary 1s should be removed")
    (is (= (optimize '(/ 5 5)) 1) "two operands with same value should yield 1, if nothing else specified"))

  (testing "Optimizing nested forms"
    (is (= (optimize '(* 0 (any (nested (whatever form 2 1))))) 0))
    (is (= (optimize '(+ x (- y 0))) '(+ x y)))
    (is (= (optimize '(+ x (+ 2 0 0 (- 100 100) (* y 1)))) '(+ x (+ 2 y))))))


(deftest evaluate-test
  (testing "Evaluating simple forms"
    (is (= (evaluate {} '(+ 1 2 3)) 6))
    (is (= (evaluate {} '(* 1 2 3 2)) 12))
    (is (= (evaluate {:x 100} '(/ x 10 2)) 5))
    (is (thrown-with-msg? Exception #"Unknown variable"
                          (evaluate {:y 100} '(* x 5 1)))))

  (testing "Evaluating nested forms"
    (is (= (evaluate {} '(* 2 (+ 1 1))) 4))
    (is (= (evaluate {:a 10} '(* 2 (+ 1 (/ a 5)))) 6))
    (is (thrown-with-msg? Exception #"Unknown variable"
                          (evaluate {:y 100} '(* 2 (+ 1 (/ a 5)))))))

  (testing "Invalid forms"
    (is (thrown-with-msg? Exception #"Unknown form"
                          (evaluate {} '(* 2 ()))))
    (is (thrown-with-msg? Exception #"Unknown operand"
                          (evaluate {} '(* 2 (unknown 2 2)))))))

(deftest ->javascript-test
  (testing "Form without variables"
    (is (= (->javascript "example" '(+ 1 (* 2 2 (+ 3 3))))
           "function example() { return (1 + (2 * 2 * (3 + 3))); }")))
  (testing "Form with variables"
    (is (= (->javascript "example" '(+ 1 (* x 2 (+ y 3))))
           "function example(x,y) { return (1 + (x * 2 * (y + 3))); }")))
  (testing "Implicit Form Optimization"
    (is (= (->javascript "example" '(+ 1 (* x 2 (+ y 0))))
           "function example(x,y) { return (1 + (x * 2 * y)); }"))))