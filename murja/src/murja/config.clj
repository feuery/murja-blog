(ns murja.config
  (:require [clojure.tools.reader :as r]
            [clojure.string :as string]
            [mount.core :refer [defstate]]))

(defn windows? []
  (string/includes? (System/getProperty "os.name")
                    "Windows"))


(defstate config :start (or (-> (if-not (windows?)
                                  "/etc/murja/config.edn"
                                  ;; :shrug:, I have no idea where Windows' /etc is, and this server isn't expected to run on windows outside of my dev env
                                  "C:\\murja-config.edn")
                                slurp
                                r/read-string)
                            (throw (Exception. "Loading config failed!")))
  :stop nil)
