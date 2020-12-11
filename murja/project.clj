(defproject murja "2.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/feuery/murja-blog"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [metosin/muuntaja "0.6.7"]
                 [mount "0.1.16"]
                 [org.clojure/tools.namespace "1.1.0"]
                 [http-kit "2.5.0"]
                 [metosin/reitit "0.5.10"]]
  :repl-options {:init-ns murja.reitit}
  :aot :all)
