(ns advent.random-problems)

(defn unpack
  ([seq]
   (unpack seq []))
  ([seq res]
   (let [elem  (first seq)
         other (rest seq)]
     (if (not (sequential? elem))
       (conj res seq)
       (let [res (unpack elem res)]
         (if (empty? other)
           res
           (unpack other res)))))))

;(unpack [["Do"] ["Nothing"]])

(defn inserter [pred val col]
  (if (empty? col) col
      (let [rest-col  (rest col)
            pairs     (map list col rest-col)
            traversed (for [[x y] pairs]
                        (if (pred x y)
                          [val y]
                          [y]))]
        (apply concat (concat [[(first col)]] traversed )))))


(defn calc [rom]
  (let [rom->dec (fn rom->dec [nums]
                   (if (empty? nums) 0
                      (let [cur  (first nums)
                            rst  (rest nums)
                            less (take-while #(> cur %) rst)
                            sum  (reduce - cur less)]
                        (+ sum (rom->dec (drop (inc (count less)) nums))))))
        r->d {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
        nums (reverse (map r->d rom))]
    (rom->dec nums)))


(defn triang
  ([a] (triang a 0 0))
  ([a x y]
   (let [cur (-> a (nth x) (nth y))]
     (if (= x (dec (count a)))
       cur
       (+ cur (min (triang a (inc x) y)
                   (triang a (inc x) (inc y))))))))


(defn subsets [k s]
  (cond
    (or (> k (count s)) (zero? k)) #{}
    (= k 1) (set (map #(set [%]) s))
    (= k (count s)) #{s}
    :else (let [cur (first s)
                rst (set (rest s))]
            (clojure.set/union
             (subsets k rst)
             (set
              (map #(conj % cur)
                   (subsets (dec k) rst)))))))


(defn is-prime? [n]
  (every? #(not= 0 (rem n %))
          (range 2 (inc (/ n 2)))))

(defn primes
  ([]
   (primes 2))
  ([n]
   (cons n (lazy-seq (filter #(not= 0 (rem % n))
                             (primes (inc n)))))))

(def all-primes (take 200 (primes)))


(defn my-eval [expr]
  (fn [vars]
    (cond (= '+ expr)    +
          (= '- expr)    -
          (= '* expr)    *
          (= '/ expr)    /
          (number? expr) expr
          (symbol? expr) (vars expr)
          (list? expr)   (let [ops (map #((my-eval %) vars) expr)]
                           (apply (first ops)
                                  (rest ops))))))


((my-eval '(+ a a (+ a b))) '{a 1 b 2})


(defmacro my-when [cond body]
  `(if ~cond ~body))


(defn parens-gen[n]
  (cond
    (= n 0) #{""}
    (odd? n) #{}
    :else (for [right-par (range 1 n 2)
                :let [left (parens-gen right-par)
                      right (parens-gen (dec (- n right-par)))]]
            )))
