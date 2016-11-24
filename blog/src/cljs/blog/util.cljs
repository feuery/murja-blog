(ns blog.util)

(defn value-of [event]
  (-> event .-target .-value))
