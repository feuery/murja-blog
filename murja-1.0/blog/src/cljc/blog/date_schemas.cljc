(ns blog.date-schemas
  (:require [schema.core :as s]
            [clojure.string :as str]))

(def months [:january :february :march :april :may :june :july :august :september :october :november :december])
(def int->month (zipmap (map inc (range)) months))
(def month->int (zipmap months (map inc (range))))

(defn int->month-str [i]
  (-> (int->month i)
      name
      (str/replace #":" "")
      str/capitalize))

(s/defschema Month (apply s/enum months))

(s/defschema Timed-Title {:Title s/Str
                          :Id s/Num
                          :Year s/Num
                          :Month Month})
