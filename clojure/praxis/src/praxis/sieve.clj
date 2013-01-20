(ns praxis.sieve)

(defn sieve
  "Return the prime number lt or eq to n."
  ([n] (sieve (-> n Math/sqrt) 3 (range 3 (inc n) 2)))
  ([last-num current res]
     (if (< current last-num)
       (let [[sieved remaining] (partition-by #(< % (* current current)) res)
             filtered (remove #(zero? (mod % current)) remaining)
             new-res (concat sieved filtered)
             new-current (first (drop-while #(<= % current) new-res))]
         (recur
          last-num
          new-current
          new-res))
       (cons 2 res))))
