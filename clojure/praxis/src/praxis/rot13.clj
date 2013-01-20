(ns praxis.rot13)

(defn rot13-encode
  [s]
  (->> s
       (map encode-char)
       (apply str)))

(defn rot13-decode
  [s]
  (->> s
       (map encode-char)
       (apply str)))

(defn- encode-char
  [c]
  (cond
   (Character/isLowerCase c) (char (+ (mod (+ 13 (- (int c) 97)) 26) 97))
   (Character/isUpperCase c) (char (+ (mod (+ 13 (- (int c) 65)) 26) 65))
   :else c))
