(ns neural-network.mathematic-function)

(def hardlim #(if (< % 0) 0 1))

(def hardlims #(if (< % 0) -1 1))

(def purelin identity)

(def sigmoid #(/ 1 (inc (Math/exp (- 0 %)))))

(def square #(* % %))
