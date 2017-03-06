(ns neural-network.net
  (:require [clojure.core.matrix.operators :as mat])
  (:use [clojure.core.matrix]))

(def consv (comp (partial apply vector) cons))

(def compose-v (fn [s1 s2] (mapv (fn [x y] [x y]) s1 s2)))

(def repeat-v (fn [n f] (apply vector (take n (repeat f)))))


(defn- sqr-mul
  [{u :u v :v} in]
  (mat/+ (mmul u (square in)) (mmul v in)))

(defn build-layer
  "Build the structure of a layer.

  Options:
  :sqr whether square the input in each layer (default false)
  :rand initialize the matrix with random values (default false)
        the ceil of random function is the value of rand option.
"
  [in out f & options]
  (let [opt (apply hash-map options)
        sqr (:sqr opt)
        rand (:rand opt)                ;TO BE ADDED
        b (repeat-v out 0)
        mat (repeat-v out (repeat-v in 0))]
    {:f f :b b :sqr sqr
     :mat (if sqr {:u mat :v mat} mat)}))

(defn pass-layer
  "Send a input to a layer and return the output.

  Options:
  :f Use specific function to calculate the result."
  [input {b :b w :mat fun :f sqr :sqr} & options]
  (let [opt (apply hash-map options)
        f (or (:f opt) fun)
        in (if sqr (sqr-mul w input) (mmul w input))]
    (emap f (mat/+ in b))))

(defn run-machine
  "Apply the net with the input and return the output.
  If the last layer have just one neuron, return a number.
  Otherwise return a vector.

  The net should be a sequence layers"
  [net input]
  (let [result (reduce (fn [in layer] (pass-layer in layer))
                       input net)]
    (if (= 1 (count result))
      (first result)
      result)))
