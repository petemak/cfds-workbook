(defproject ch1-statistics "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter/incanter-core "1.5.7"]
                 [incanter/incanter-excel "1.5.7"]
                 [incanter/incanter-charts "1.5.7"]]
  :main ^:skip-aot ch1-statistics.core
  :target-path "target/%s"
  :resource-paths ["data"]
  :jvm-opts ["-Xmx1024m"]
  :profiles {:uberjar {:aot :all}})
