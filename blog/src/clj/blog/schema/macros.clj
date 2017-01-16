(ns blog.schema.macros
  (:require [schema.core :as s]))

(defmacro allow-empty [forms]
  `(s/conditional empty? {}
                  :else ~forms))

