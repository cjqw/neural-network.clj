(ns neural-network.net
  (:require [clojure.core.matrix.operators :as mat]
            [clojure.core.matrix :as m])
  (:use [neural-network.util]))

(defn- sqr-mul
  [{u :u v :v} in]
  (mat/+ (m/mmul u (m/square in)) (m/mmul v in)))

(defn- randomize
  [m]
  (m/emap (fn [x] (- (rand 2) 1)) m))

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
    {:f f :b (randomize b) :sqr sqr
     :mat (if sqr {:u (randomize mat)
                   :v (randomize mat)}
              (randomize mat))}))

(defn pass-layer
  "Send a input to a layer and return the output.

  Options:
  :f Use specific function to calculate the result."
  [input {b :b w :mat fun :f sqr :sqr} & options]
  (let [opt (apply hash-map options)
        f (or (:f opt) fun)
        in (if sqr (sqr-mul w input) (m/mmul w input))]
    (m/emap f (mat/+ in b))))

(defn run-machine
  "Apply the net with the input and calculate the output.
  The result of each layer will be stored with :x key.
  The returned value is [output, new-net].
  The net should be a sequence layers"
  [net input]
  (let [initial [input,[]]]
    (reduce
     (fn [prev layer]
       (let [input (first prev)
             net (second prev)
             output (pass-layer input layer)
             new-layer (assoc layer :x output)]
         [output,(conj net new-layer)]))
     initial net
     )))
