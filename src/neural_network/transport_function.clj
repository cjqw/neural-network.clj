(ns neural-network.transport-function)

(def hardlim #(if (< % 0) 0 1))

(def hardlims #(if (< % 0) -1 1))

(def purelin (fn [x] x))
