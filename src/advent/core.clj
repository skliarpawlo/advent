(ns advent.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :refer [difference]]
            [clojure.core.match :refer [match]]))

(defn captcha [s]
  (let [one-right (cons (last s) (butlast s))
        zipped (map list s one-right)]
    (reduce (fn [sum [l r]] (if (= l r) (+ sum l) sum)) 0 zipped)))

(defn captcha-half-way [s]
  (let [half (/ (count s) 2)
        half-right (concat (take half (drop half s)) (take half s))
        zipped (map list s half-right)]
    (reduce (fn [sum [l r]] (if (= l r) (+ sum l) sum)) 0 zipped)))

(defn sum-min-max [a]
  (let [mins (map #(apply min %) a)
        maxs (map #(apply max %) a)]
    (reduce (fn [sum [min max]]
              (+ sum (- max min)))
            0
            (map list mins maxs))))

(defn sum-divisible [a]
  (let [cross (for [[xi x] (map list (range) a),
                    [yi y] (map list (range) a)
                    :when (and (not= xi yi)
                               (zero? (mod x y)))] (quot x y))]
    (first cross)))

; (apply + (map sum-divisible rows))


(defn spiral [a]
  (let [n (loop [min-n 1]
            (if (<= a (* (+ min-n 2) (+ min-n 2)))
              min-n
              (recur (+ min-n 2))))
        x (+ 1 (quot (- n 1) 2))
        cur-n (+ n 2)
        cur-n-1 (dec cur-n)
        one-y (dec (quot cur-n-1 2))
        abs-y (mod (- (dec a) (* n n)) cur-n-1)
        abs (fn [n] (max n (- n)))
        y (abs (- abs-y one-y))]
    [x y]))

(def directions [[1 0]
                 [0 1]
                 [-1 0]
                 [0 -1]])

(defn spiral-next [[coords direction visited]]
  (let [look-left   (-> (inc direction)
                        (mod 4)
                        directions
                        (#(map + coords %)))
        next-direction (if (visited look-left)
                         direction
                         (-> (inc direction) (mod 4)))
        next-coords (map + coords (directions next-direction))
        dxy (for [dx (range -1 2)
                  dy (range -1 2) :when (not (and (zero? dx)
                                                  (zero? dy)))] [dx dy])
        neibour-sum (->> (repeat coords)
                         (map #(map + %1 %2) dxy)
                         (map visited)
                         (filter some?)
                         (apply +))]
    [next-coords next-direction (assoc visited coords neibour-sum)]))


(defn passphrase [passes]
  (let [splitted (map #(str/split % #" ") passes)
        sets (map set splitted)]
    (reduce (fn [sum [orig as-set]]
              (if (= (count orig) (count as-set)) (inc sum) sum))
            0
            (map list splitted sets))))


(defn passphrase-2 [passes]
  (let [splitted (map #(map set (str/split % #" ")) passes)
        sets (map set splitted)]
    (reduce (fn [sum [orig as-set]]
              (if (= (count orig) (count as-set)) (inc sum) sum))
            0
            (map list splitted sets))))



(defn cpu-maze
  ([a] (cpu-maze a 0 0))
  ([a x jumps-total]
   (let [jump (nth a x)
         jump-to (+ x jump)
         a (concat (take x a)
                   [(if (>= jump 3) (dec jump) (inc jump))]
                   (nthrest a (inc x)))
         jumps-total (inc jumps-total)]
     (if (and (>= jump-to 0)
              (< jump-to (count a)))
       (recur a jump-to jumps-total)
       jumps-total))))

(defn redist
  ([banks] (redist banks {banks 0} 0))
  ([banks visited res]
   (let [cur-max (apply max banks)
         banks-idx (map-indexed vector banks)
         max-idxs (filter #(= cur-max (second %)) banks-idx)
         max-idx (first (first max-idxs))
         new-banks (loop [cur-idx (mod (inc max-idx) (count banks))
                          max-rest cur-max
                          cur-banks (assoc banks max-idx 0)]
                     (if (zero? max-rest)
                       cur-banks
                       (recur (mod (inc cur-idx) (count banks))
                              (dec max-rest)
                              (assoc cur-banks
                                     cur-idx
                                     (inc (cur-banks cur-idx))))))]
     (if (visited new-banks)
       (- (inc res) (get visited new-banks))
       (recur new-banks (assoc visited banks res) (inc res))))))

(def inp [4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5])

(defn parse-edges [row]
  (when-let [[_ from to] (re-find #"([a-z]+) \(\d+\) -> ((?:[a-z]+(?:, )?)+)" row)]
    [from (str/split to #", ")]))

(defn bottom [edges]
  (let [edges (remove nil? edges)
        candidates (->> edges
                        (map first)
                        (set))
        not-candidates (->> edges
                            (mapcat second)
                            (set))
        result-set (difference candidates not-candidates)]
    (first result-set)))

(defn unbalanced
  ([edges] (unbalanced (into {} edges)
                       (bottom edges)))
  ([edges x]
   (let [children (get edges x)]
     (if (nil? children)
       [(second x) false]
       (let [child-unbalanced (map #(unbalanced edges %) children)
             unbalanced? (apply = (map first child-unbalanced))
             ]
         (if (not balanced?) []))))))

(defn parse-instruction [instr]
  (when-let [[_ reg op op-val if-reg if-op if-val]
             (re-find #"([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) ([!=<>]+) (-?[0-9]+)" instr)]
    [reg op op-val if-reg if-op if-val]))

(defn interp-instruction [regs [reg op op-val if-reg if-op if-val]]
  (let [reg-val (get regs reg 0)
        if-reg-val (get regs if-reg 0)
        if-op (if (= "!=" if-op) "not=" if-op)
        op ({"inc" "+", "dec" "-"} op)
        if-result (load-string (str "(" if-op " " if-reg-val " " if-val ")"))
        op-result (load-string (str "(" op " " reg-val " " op-val ")"))]
    (if if-result
      (assoc regs reg op-result)
      regs)))

(defn remove-grabage [stream]
  (reduce
   (fn [[output state] sym]
     (match [state sym]
       [{:balance _
         :ignore-next false
         :garbage _}
        \!] [output (assoc state :ignore-next true)]
       [{:balance     _
         :ignore-next true
         :garbage     _}
        _] [output (assoc state :ignore-next false)]
       [{:balance     _
         :ignore-next false
         :garbage     false}
        \<] [output (assoc state :garbage true)]
       [{:balance     _
         :ignore-next false
         :garbage     true}
        \>] [output (assoc state :garbage false)]
       [{:balance     _
         :ignore-next false
         :garbage     true}
        _] [output state]
       [{:balance     balance
         :ignore-next false
         :garbage     false}
        \{] [(+ output (inc balance)) (assoc state :balance (inc balance))]
       [{:balance     balance
         :ignore-next false
         :garbage     false}
        \}] [output (assoc state :balance (dec balance))]
       [a b] [output a]))
   [0 {:balance     0
       :ignore-next false
       :garbage     false}]
   stream))

(defn make-jump [[row pos skip] jump]
  (let [row-count (count row)
        cycle-row (cycle row)
        reversed-part (->> cycle-row
                           (drop pos)
                           (take jump)
                           (reverse))
        kept-part (->> cycle-row
                       (drop (+ pos jump))
                       (take (- row-count jump)))
        res-shifted (concat reversed-part kept-part)
        res (concat (drop (- row-count pos) res-shifted)
                    (take (- row-count pos) res-shifted))]
    [res
     (mod (+ pos jump skip) row-count)
     (mod (inc skip) row-count)]))

(defn reduce-jumps [jumps-input]
  (reduce make-jump [(range 256) 0 0] jumps-input))

;
;                n (0,1)
; nw (-0.5,0.5)  	   	    ne (0.5, 0.5)
;                x (0,0)
; sw (-0.5,-0.5)     		  se (0.5, -0.5)
;                s(0,-1)
;

(def steps {"s"  [0 -1]
            "sw" [-1/2 -1/2]
            "se" [1/2 -1/2]
            "n"  [0 1]
            "ne" [1/2 1/2]
            "nw" [-1/2 1/2]})

(defn traverse-path [pos edge]
  (map + pos (steps edge)))

(reduce (comp doall traverse-path) [0 0] ["ne" "ne" "s" "s"])

(def input-path
  (-> (slurp "resources/hex.txt")
      (str/trim)
      (str/split #",")))

(defn bfs [graph x]
  (loop [visited [x]]
    (let [from-visited (set (mapcat graph visited))
          extended-visited (clojure.set/union visited from-visited)]
      (if (= (count visited)
             (count extended-visited))
        visited
        (recur extended-visited)))))

(defn parse-village-row [row]
  (when-let [[_ from to] (re-find #"(\d+) <-> ((?:\d+(?:, )?)+)" row)]
    [from (str/split to #", ")]))

(def village-graph
  (-> (slurp "resources/village.txt")
      (str/split #"\n")
      (#(map parse-village-row %))
      (#(into {} %))))

(count (bfs village-graph "0"))

(defn failed-scanner? [layer time]
  (let [back-n-forth (mod time (* 2 (dec layer)))
        position (if (>= back-n-forth layer)
                   (- layer (- back-n-forth (dec layer)))
                   back-n-forth)]
    (zero? position)))

(defn calc-severity [layers]
  (let [layers-fails
        (for [time (->> layers (keys) (apply max) (inc) (range))
              :let [layer (get layers time 0)]]
          [time layer (failed-scanner? layer time)])
        severity (reduce (fn [sum [time layer failed?]]
                           (if failed?
                             (+ sum (* time layer))
                             sum))
                         0 layers-fails)]
    severity))
