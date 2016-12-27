(ns blog.config
  (:require [clojure.tools.reader :as r]))

(def config (atom (or (-> "/etc/murja/config.edn"
                    slurp
                    r/read-string)
                (throw (Exception. "Loading config failed!")))))

(defn reload-config! []
  (reset! config 
          (-> "/etc/murja/config.edn"
              slurp
              r/read-string)))
