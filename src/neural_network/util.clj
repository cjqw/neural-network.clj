(ns neural-network.util)

(use 'clojure.core.matrix)

(def a
  [[1 2]
   [2 4]])

(def b
  [[3 5]
   [0 -1]])

(defn- test
  []
  (time (mmul a b))
  nil)
