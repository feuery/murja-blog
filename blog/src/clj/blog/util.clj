(ns blog.util)

(defn change-key [map old-key new-key]
  (-> map
      (assoc new-key (get map old-key))
      (dissoc old-key)))
