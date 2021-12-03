(ns simplify)

(def latin (set (map char (concat (range 97 123)))))
(def digit #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(defn abs [n] (if (< n 0) (- n) n))

(defn read-sign [input-seq]
  (let [ch (first input-seq)]
    (cond
      (= ch \-) [\- (next input-seq)]
      (= ch \+) [\+ (next input-seq)]
      :else [\+ input-seq])))

(defn read-coef [input-seq]
  (let [coefs (take-while digit input-seq)]
    (if (> (count coefs) 0)
      [(Integer/parseInt (apply str coefs)) (drop (count coefs) input-seq)]
      [1 input-seq])))

(defn read-var [input-seq]
  (let [var (first input-seq)]
    (if (latin var)
      [var (next input-seq)]
      [\~ input-seq])))

(defn merge-expr [sign coef var]
  (let [coef (if (= sign \-) (- coef) coef)]
    {var coef}))

(defn add-expressions [expr1 expr2] (merge-with + expr1 expr2))
(defn multiply-by-coef [sign coef expr]
  (let [coef (if (= sign \-) (- coef) coef)]
    (zipmap (keys expr) (map #(* coef %) (vals expr)))))

(defn find-matching-paren [input-seq balance idx]
  (let [ch (first input-seq)]
    (cond (= ch \() (find-matching-paren (next input-seq) (inc balance) (inc idx))
          (= ch \)) (if (= balance 1)
                      idx
                      (find-matching-paren (next input-seq) (dec balance) (inc idx)))
          :else (find-matching-paren (next input-seq) balance (inc idx)))))

(declare parse-expressions)

(defn read-parens [input-seq]
  (let [ch (first input-seq)]
    (cond
      (= ch \()
      (let [matching-paren (find-matching-paren input-seq 0 0)]
        [(reduce add-expressions (parse-expressions
                                  (next (take matching-paren input-seq))))
         (drop (inc matching-paren) input-seq)])
      :else [:no-paren-expr input-seq])))


(defn parse-expressions [input-seq]
  (if (or (-> input-seq count zero?) (nil? input-seq))
    []
    (let [[sign after-sign] (read-sign input-seq)
          [coef after-coef] (read-coef after-sign)
          [var after-var] (read-var after-coef)
          [paren-expr after-paren-expr] (read-parens after-var)]
      (conj (parse-expressions after-paren-expr)
            (if (= paren-expr :no-paren-expr)
              (merge-expr sign coef var)
              (multiply-by-coef sign coef paren-expr))))))


(defn expression->str [expr]
  (let [filtered-keys (for [[key val] expr :when (not= val 0)] key)
        sorted-keys (sort filtered-keys)
        sorted-vals (for [key sorted-keys
                          :let [val (expr key)]]
                      (str (if (< 0 val) " + " " - ")
                           (if (or (= key \~) (not= (abs val) 1)) (abs val))
                           (if (= key \~) "" key)))
        str-repr (if (zero? (count sorted-vals)) ""
                     (clojure.string/join "" sorted-vals))
        clean-prefix (if (= \+ (second str-repr))
                       (clojure.string/join "" (drop 3 str-repr))
                       (str "- " (clojure.string/join "" (drop 3 str-repr))))]
    clean-prefix))


(defn simplify [line]
  (let [no-spaces (filter #(not= % \space) (seq line))
        expression (expression->str (reduce add-expressions (parse-expressions no-spaces)))
        expression (if (-> expression count zero?) "0" expression)]
    expression))


(defn main []
  (let [n (Integer/parseInt (read-line))]
    (doseq [i (range n)]
      (let [line (read-line)
            simplified-line (simplify line)]
        (println simplified-line)))))

; 2x - 4(4 + 4x + 3(18 - 2x))
; 2x - 4(  4 + 4x + 54 - 6x )
; 2x - 16 - 16x - 216 + 24x = 10x - 232