(ns advent.matcher
  (:require [clojure.test :refer [deftest is]]))


(deftest quick-check
  (is (= (matcher [2]
           1 "Ones"
           2 "Two"
           :else "Three")
         "Two")))


(defmacro matcher [val & conds]
  `(let [val# ~val]
     ~(let [conds-part (partition 2 conds)]
        (loop [conds conds-part]
         (if (< 0 (count conds))
           (let [[clause body] (first conds)
                 others        (rest conds)]
             `(if ~clause ~body
                  (recur ~others))))))))


(macroexpand-1
 (matcher 1
          1 (println "One")
          2 (println "Two")
          3 (println "Three")))
