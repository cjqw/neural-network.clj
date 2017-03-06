(ns neural-network.net
  (:require [clojure.core.matrix.operators :as mat])
  (:use [clojure.repl]
        [clojure.core.matrix]))

;; (def s [1])
;; (def f identity)
;; (def in 2)

;;; Test data above

(def consv (comp (partial apply vector) cons))

(def compose-v (fn [s1 s2] (mapv (fn [x y] [x y]) s1 s2)))

(def repeat-v (fn [n f] (apply vector (take n (repeat f)))))


(defn- sqr-mul
  [{u :u v :v} in]
  (mat/+ (mmul u (square in)) (mmul v in)))

(defn pass-layer
  "Send a input to a layer and return the output.

  Options:
  :f Use specific function to calculate the result."
  [input {b :b w :mat fun :f sqr :sqr} & options]
  (let [opt (apply hash-map options)
        f (or (:f opt) fun)
        in (if sqr (sqr-mul w input) (mmul w input))]
    (emap f (mat/+ in b))))

(defn build-layer
  "Build the structure of a layer.

  Options:
  :sqr whether square the input in each layer (default false)
"
  [in out f & options]
  (let [opt (apply hash-map options)
        sqr (:sqr opt)
        b (repeat-v out 0)
        mat (repeat-v out (repeat-v in 0))]
    {:f f :b b :sqr sqr
     :mat (if sqr {:u mat :v mat} mat)}))

;; (defn tune-machine
;;   "Need a bf function in machine.
;;   Tune the machine with given input and answer.

;;   The input of bp should be:
;;   [error layer]
;;   "
;;   [machine input answer]
;;   (let [e (- answer (run-machine machine input))
;;         net (:net machine)
;;         bp (:bp machine)
;;         new-net (reduce bp e (reverse net))]
;;     (assoc machine :net new-net)))

;; (defn run-machine
;;   "Apply the net with the input and return the output.
;;   If the last layer have just one neuron, return a number.
;;   Otherwise return a vector."
;;   [machine input]
;;   (let [net (:net machine)
;;         sqr (:sqr machine)
;;         f (:forward machine)
;;         result (reduce (fn [in layer] (pass-layer in layer f :sqr sqr))
;;                        input net)]
;;     (if (= 1 (count result))
;;       (first result)
;;       result)))

;; (defn build-machine
;;   "Build the structure of neural network

;;   Options:
;;   :bp-function backpropagation function
;;   :net origin weight net (default zero matrix)
;;   :sqr whether square the input in each layer (default false)
;;   "
;;   [s f in & options]
;;   (let [opt (apply hash-map options)
;;         bp (:bp-function opt)
;;         sqr (:sqr opt)
;;         net (or (:net opt) (mapv #(build-layer % :sqr sqr)
;;                                  (compose-v (pop (consv in s)) s)))]
;;     {:forward f :bp bp :sqr sqr :net net}))
