(ns praxis.josephus)

(defn josephus
  ([n m] (josephus (range 0 n) m 1 []))
  ([nums m ind res]
     (if (< (count nums) m)
       (concat res nums)
       (if (= m ind)
         (recur (rest nums) m 1 (conj res (first nums)))
         (recur (concat (rest nums) (list (first nums))) m (inc ind) res)))))
