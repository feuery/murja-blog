(ns murja.specs.user
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::username string?)
(spec/def ::nickname string?)
(spec/def ::img_location string?)

(spec/def ::User (spec/keys :req-un [::username ::nickname ::img_location]))
