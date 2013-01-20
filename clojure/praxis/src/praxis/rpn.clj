(ns praxis.rpn)

(defn rpn
  ([expr] (rpn expr []))
  ([expr stack]
     (if-not expr
       (peek stack)
       (let [f (first expr)]
         (if (number? f)
           (recur (next expr) (conj stack f))
           (recur (next expr) (conj (-> stack pop pop)
                                    ((eval f)
                                     (-> stack pop peek)
                                     (peek stack)))))))))

(defn str->expr
  [s]
  (map read-string (clojure.string/split s #"\s")))

(defn read-rpn-expr
  []
  (let [line (read-line)]
    (when (seq line)
      (println (rpn (str->expr line)))
      (recur))))
