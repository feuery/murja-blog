(ns blog.date-schemas
  (:require [schema.core :as s]))

(def months (mapv name [:january :february :march :april :may :june :july :august :september :october :november :december]))
(def int->month (zipmap (map inc (range)) months))

(s/defschema Month (apply s/enum months))

(s/defschema Timed-Title {:Title s/Str
                          :Year s/Num
                          :Month Month})
