(ns murja.specs.login
    (:require [clojure.spec.alpha :as spec]))

(spec/def ::username string?)
(spec/def ::password string?)

(spec/def ::login (spec/keys :req-un [::username ::password]))
