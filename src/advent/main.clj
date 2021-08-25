(ns advent.main)

(defn f 
  "Function Docs"
  [{name :name surname :surname}]
  (str name " " surname))

(f {:name "Pavlo" :surname "Skliar"})

(->> [1 2 3 4]
     (filter even?)
     (map #(inc %)))


(defn fun [x y z]
  (+ x y z))

(fun 1 2 3)