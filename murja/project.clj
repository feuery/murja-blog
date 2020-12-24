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
                 [ragtime "0.8.0"]
                 [metosin/reitit "0.5.10"]]
  :repl-options {:init-ns murja.reitit}
  :profiles {:dev {:dependencies [[org.clojure/tools.nrepl "0.2.13"]
                                  [cider/cider-nrepl "0.25.2"]]
                   :plugins [[cider/cider-nrepl "0.25.2"]]}}
  :main murja.reitit
  :aot :all)
