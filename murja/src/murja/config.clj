(ns murja.config
  (:require [clojure.tools.reader :as r]
            [mount.core :refer [defstate]]))

(defstate config :start (or (-> "/etc/murja/config.edn"
                                slurp
                                r/read-string)
                            (throw (Exception. "Loading config failed!")))
  :stop nil)
