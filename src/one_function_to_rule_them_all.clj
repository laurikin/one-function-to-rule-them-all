(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(concat-elements [])            ;=> ()
(concat-elements [[1 2]])       ;=> (1 2)
(concat-elements [[1 2] [3 4]]) ;=> (1 2 3 4)

(defn str-cat [a-seq]
  (let [reducer (fn
                  ([acc el] (str acc " " el))
                  ([] ""))]
    (reduce reducer a-seq)))

(str-cat ["I" "am" "Legend"])  ;=> "I am Legend"
(str-cat ["I" "am" "back"])    ;=> "I am back"
(str-cat ["more" " " "space"]) ;=> "more   space"
(str-cat [])                   ;=> ""

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (seq (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq)))))

(my-interpose 0 [1 2 3])               ;=> (1 0 2 0 3)
(my-interpose "," ["I" "me" "myself"]) ;=> ("I" "," "me" "," "myself")
(my-interpose :a [1])                  ;=> (1)
(my-interpose :a [])                   ;=> ()

(defn my-count [a-seq]
  (reduce
    (fn ([acc _] (+ acc 1)))
    0
    a-seq))

(my-count [])      ;=> 0
(my-count [1 2 3]) ;=> 3
(my-count [1])     ;=> 1

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(my-reverse [1 2 3]) ;=> (3 2 1)
(my-reverse [1 2])   ;=> (2 1)
(my-reverse [])      ;=> ()

(defn min-max-element [a-seq]
  (reduce
    (fn [[min-val max-val :as acc] el]
      (cond
        (> el max-val)
          (assoc acc 1 el)
        (< el min-val)
          (assoc acc 0 el)
        :else
          acc))
    [(first a-seq) (first a-seq)]
    a-seq))

(min-max-element [2 7 3 15 4]) ;=> [2 15]
(min-max-element [1 2 3 4])    ;=> [1 4]
(min-max-element [1])          ;=> [1 1]

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n '())
    (loop [ros sorted-seq
           prev nil
           acc []]
      (cond
        (empty? ros)
          (concat acc [n])
        (and (nil? prev) (<= n (first ros)))
          (concat acc [n] ros)
        (and (not (nil? prev)) (<= prev n (first ros)))
          (concat acc [n] ros)
        :else
          (recur (rest ros) (first ros) (conj acc (first ros)))))))

(insert [] 2)      ;=> (2)
(insert [1 3 4] 2) ;=> (1 2 3 4)
(insert [1] 2)     ;=> (1 2)

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(insertion-sort [2 5 3 1]) ;=> (1 2 3 5)
(insertion-sort [1 2])     ;=> (1 2)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(parity [:a :b :c])    ;=> #{:a :b :c}
(parity [:a :a :b :b]) ;=> #{}
(parity [1 2 3 1])     ;=> #{2 3}

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(minus 2)   ;=> -2
(minus 4 3) ;=> 1

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & rest]
    (reduce (fn [acc _] (+ acc 1)) 1 rest)))


(count-params)            ;=> 0
(count-params :a)         ;=> 1
(count-params :a 1 :b :c) ;=> 4

(defn my-*
  ([] 1)
  ([x] x)
  ([x & rest]
    (reduce * x rest)))

(my-*)           ;=> 1
(my-* 4 3)       ;=> 12
(my-* 1 2 3 4 5) ;=> 120

(defn old-pred-and [pred1 pred2]
  #(and (pred1 %) (pred2 %)))

(defn pred-and
  ([] (fn [_] true))
  ([pred] pred)
  ([pred1 pred2] (fn [item] (and (pred1 item) (pred2 item))))
  ([pred1 pred2 & rest]
    (fn [item]
      (reduce
        (fn [acc pred]
          (and acc (pred item)))
        (and (pred1 item) (pred2 item))
        rest))))

(filter (pred-and) [1 0 -2])                    ;=> (1 0 -2)
(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3]) ;=> (1 7)
(filter (pred-and number? integer? pos? even?)
        [1 4 0 -2 :a 7 "a" 2])                    ;=> (0 2)

(defn my-map [f a-seq]
  [:-])
