(defproject blog "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [ring-server "0.5.0"]
                 [reagent "0.6.0"]
                 [reagent-forms "0.5.25"]
                 [reagent-utils "0.2.0"]
                 [ring "1.8.2"]
                 [ring/ring-defaults "0.3.2"]
                 [compojure "1.6.2"]
                 [hiccup "1.0.5"]
                 [yogthos/config "1.1.7"]
                 [org.clojure/clojurescript "1.9.229"
                  :scope "provided"]
                 [secretary "1.2.3"]
                 [venantius/accountant "0.1.7"
                  :exclusions [org.clojure/tools.reader]]
                 [cljs-ajax "0.5.8"]
                 [clj-time "0.15.2"]
                 [com.andrewmcveigh/cljs-time "0.4.0"]

                 [org.postgresql/postgresql "42.2.18"]
                 [ragtime "0.8.0"]
                 [metosin/compojure-api "1.1.13"]
                 [prismatic/schema "1.1.12"]
                 [cheshire "5.10.0"]
                 [re-frame "0.8.0"]
                 [buddy "2.0.0"]
                 [org.clojars.freemarmoset/feedparser-clj "0.6.1"]
                 [clout "2.2.1"]]

  :plugins [[lein-environ "1.0.2"]
            [lein-ancient "0.6.15"]]

  :ring {:handler blog.handler/app
         :uberwar-name "blog.war"}

  :min-lein-version "2.5.0"

  :uberjar-name "blog.jar"

  :main blog.system

  :source-paths ["src/clj" "src/cljc"]
  :resource-paths ["resources"]






  :profiles {:dev {:repl-options {:init-ns user}

                   :dependencies [[ring/ring-mock "0.4.0"]
                                  [ring/ring-devel "1.8.2"]
                                  [prone "1.1.2"]
                                  [pjstadig/humane-test-output "0.10.0"]
                                  ]

                   :source-paths ["env/dev/clj"]

                   :env {:dev true}}

             :uberjar {:source-paths ["env/prod/clj"]

                       :env {:production true}}})
