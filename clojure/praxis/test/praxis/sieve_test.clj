(ns praxis.sieve-test
    (:use clojure.test
          praxis.sieve))

(deftest sieve-test
  (is (= '(2 3 5 7) (sieve 10)))
  (is (= '(2 3 5 7 11 13 17 19 23 29) (sieve 30))))
