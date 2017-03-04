(defproject neural-network "0.1.0-SNAPSHOT"
  :description "Deep learning homework"
  :url "https://github.com/cjqw/neural-network"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [net.mikera/core.matrix "0.57.0"]
                 [clatrix "0.5.0"]
                 [incanter "1.5.7"]]
  :main ^:skip-aot neural-network.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
