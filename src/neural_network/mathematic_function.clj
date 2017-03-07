(ns neural-network.mathematic-function)

(def hardlim #(if (< % 0) 0 1))

(def hardlims #(if (< % 0) -1 1))

(def purelin (fn [x] x))

(def sigmoid #(/ 1 (inc (Math/exp (- 0 %)))))

(def square #(* % %))

(defn- partial-sigmoid
  [x]
  (let [y (sigmoid x)]
    (* y (- 1 y))))

(def partial-function
  {purelin (fn [x] 1)
   sigmoid partial-sigmoid})
