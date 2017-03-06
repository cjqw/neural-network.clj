(ns neural-network.mlqp
  (:use [neural-network.net]
        [neural-network.mathematic-function]
        [clojure.repl]
        [clojure.pprint]))

(def net [(build-layer 2 10 sigmoid :sqr true)
          (build-layer 10 1 purelin)])
