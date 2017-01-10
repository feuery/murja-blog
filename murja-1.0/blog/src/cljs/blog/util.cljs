(ns blog.util)

(defn in? [col x]
  (some (partial = x) col))

(defn value-of [event]
  (-> event .-target .-value))

(defn ctrl-clicked? [e]
  (.-ctrlKey e))

(defn only-ctrl-clicked? [e]
  (println (if (or
                (= (.-which e) 17)
                ;; TODO React sees caps as caps even though I've set it as ctrl in gnome tweak tool. I should either paramtrize this or learn to hack this deeper in my linux environment
                (= (.-which e) 20))
             "Only ctrl clicked!"
             "No ctrl"))
  (or
   (= (.-which e) 17)
   ;; TODO React sees caps as caps even though I've set it as ctrl in gnome tweak tool. I should either paramtrize this or learn to hack this deeper in my linux environment
   (= (.-which e) 20)))
