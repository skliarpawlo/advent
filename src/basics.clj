(ns basics)


(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))


(def state (atom {}))
(swap! state assoc :foo "baz")
@state