(ns praxis.rpn-test
    (:use clojure.test
          praxis.core
          praxis.prn))

(deftest rpn-test
  (is (= 10 (rpn [5 5 +])))
  (is (= 75 (rpn [10 20 + 5 - 3 *]))))
