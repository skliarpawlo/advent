(ns basics)

(+ 2 2)
(> 2 3)

(defn factorial [n]
  (if (= n 1) 1
      (* n (factorial (- n 1)))))


(def state (atom 0))
@state

(swap! state inc)
@state 