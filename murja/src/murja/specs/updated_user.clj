(ns murja.specs.updated-user
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::nickname string?)
(spec/def ::img_location string?)
(spec/def ::password string?)

(spec/def ::updated-user (spec/keys :req-un [::nickname ::img_location ::password]))
