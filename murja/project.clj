(defproject murja "2.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/feuery/murja-blog"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [metosin/muuntaja "0.6.7"]
                 [mount "0.1.16"]
                 [hiccup "1.0.5"]
                 [org.clojure/tools.namespace "1.1.0"]
                 [org.postgresql/postgresql "42.2.18"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [ring/ring-jetty-adapter "1.8.2"]
                 [commons-io/commons-io "2.8.0"]
                 [ragtime "0.8.0"]
                 [metosin/reitit "0.5.10"]
                 [com.layerware/hugsql-core "0.5.1"]
                 [com.layerware/hugsql-adapter-clojure-java-jdbc "0.5.1"]
                 [org.clojure/data.xml "0.0.8"]]
  :repl-options {:init-ns murja.reitit}
  :profiles {:dev {:dependencies [[org.clojure/tools.nrepl "0.2.13"]
                                  [cider/cider-nrepl "0.44.0"]

                                  [ring/ring-mock "0.4.0"]]
                   :plugins [[cider/cider-nrepl "0.26.0"]]}}
  :source-paths ["src" "src/sql"]
  
  :main murja.reitit
  :aot :all)
