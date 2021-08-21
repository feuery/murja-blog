(ns murja.specs.timed-title
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as str]))

(def months [:january :february :march :april :may :june :july :august :september :october :november :december])
(def int->month (zipmap (map inc (range)) months))
(def month->int (zipmap months (map inc (range))))

(spec/def ::Title string?)
(spec/def ::Id number?)
(spec/def ::Year number?)
(spec/def ::Month number?)
(spec/def ::Tag string?)
(spec/def ::Tags (spec/* ::Tag))

(spec/def ::Timed-Title (spec/keys :req-un [::Title ::Id ::Year ::Month ::Tags]))
