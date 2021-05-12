(defproject euler "0.1.0-SNAPSHOT"
  :description "project euler in Clojure"
  :url "https://github.com/ahxxm/euler"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :repl-options {:init-ns euler.core}
  :aot :all
  :main euler.core)
