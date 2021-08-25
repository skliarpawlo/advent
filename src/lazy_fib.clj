(ns lazy-fib)


(defn lazy-fibonacci
  ([] (lazy-fibonacci 0 1))
  ([a b] (lazy-seq (cons b (lazy-fibonacci b (+ a b))))))

(take 10 (lazy-fibonacci))