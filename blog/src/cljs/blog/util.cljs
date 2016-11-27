(ns blog.util)

(defn in? [col x]
  (some (partial = x) col))

(defn value-of [event]
  (-> event .-target .-value))
