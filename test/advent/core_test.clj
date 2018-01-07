(ns advent.core-test
  (:require [clojure.test :refer :all]
            [advent.core :refer :all]))


(deftest test-spiral-2
  (is (= (spiral 10) [2 1]))
  (is (= (spiral 11) [2 0]))
  (is (= (spiral 12) [2 1]))
  (is (= (spiral 13) [2 2]))
  (is (= (spiral 14) [2 1])))


(deftest test-passphrase
  (is (= (passphrase ["a b c d"
                      "a a c d"])
         1)))
