(ns looping-is-recursion)

(defn power [base exp]
  (let [power-help (fn [acc base exp]
                     (if (= 0 exp)
                       acc
                       (recur (* base acc) base (dec exp))))]
    (power-help 1 base exp)))


(defn last-element [a-seq]
  (let [last-element-help (fn [elem b-seq]
                     (if (empty? b-seq)
                       elem
                       (recur (first b-seq) (rest b-seq))))]
    (last-element-help (first a-seq) a-seq)))

(defn seq= [seq1 seq2]
  (let [seq-help (fn [seq-1 seq-2]
                     (if (and (empty? seq-1) (empty? seq-2))
                       true
                       (if (or (empty? seq-1) (empty? seq-2) (not= (first seq-1) (first seq-2)))
                         false
                         (recur (rest seq-1) (rest seq-2)))))]
    (seq-help seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         b-seq a-seq]
    (if (empty? b-seq)
      nil
      (if (pred (first b-seq))
        index
        (recur (inc index) (rest b-seq))))))


(defn avg [a-seq]
  (loop [sum 0
         index 0
         b-seq a-seq]
    (if (empty? b-seq)
      (if (= 0 index)
        0
        (/ sum index))
      (recur (+ sum (first b-seq)) (inc index) (rest b-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem)
                                 (disj a-set elem)
                                 (conj a-set elem)))]
  (loop [acc-seq '()
         b-seq a-seq]
    (if (empty? b-seq)
      acc-seq
      (recur (toggle (set acc-seq) (first b-seq)) (rest b-seq))))))

(defn fast-fibo [n]
  (loop [num 0
         fib-1 0
         fib-2 0
         fib 0]
    (let [fib-r (if (= 1 num) 1 fib)]
      (if (= num n)
        fib-r
        (recur (inc num) fib-r fib-1 (+ fib-r fib-1))))))

(defn cut-at-repetition [a-seq]
  (loop [part-1 []
         part-2 a-seq]
    (let [next-elem (first part-2)]
      (if (or (empty? part-2) (contains? (set part-1) next-elem))
        part-1
        (recur (conj part-1 next-elem) (rest part-2))))))

