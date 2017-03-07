(ns neural-network.util)

(def space-concat (fn [& s] (reduce #(str %1 " " %2) s)))

(def consv (comp (partial apply vector) cons))

(def compose-v (fn [s1 s2] (mapv (fn [x y] [x y]) s1 s2)))

(def repeat-v (fn [n f] (apply vector (take n (repeat f)))))

(def reverse-v (fn [s] (apply vector (reverse s))))
