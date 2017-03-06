(ns neural-network.util)

(def space-concat (fn [& s] (reduce #(str %1 " " %2) s)))
