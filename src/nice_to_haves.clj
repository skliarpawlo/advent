(ns nice-to-haves)

;; pure-functional
(assoc {} :name "pavlo" :company "tubular")
(dissoc {:key 42} :key)

;; special symbols in method names
(defn uah->dollars [uah] (* uah 28))
(defn valid? [condition] (= condition true))
(defn mutate! [var val] (set! var val))