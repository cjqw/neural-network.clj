(ns neural-network.mlqp
  (:require [clojure.core.matrix.operators :as mat]
            [clojure.core.matrix :as m]
            [neural-network.mathematic-function :as fun])
  (:use [neural-network.net]
        [neural-network.parser]
        [neural-network.chart]
        [neural-network.util]
        [clojure.repl]
        [clojure.pprint]))

;;; Define alpha

(def alpha 0.0010)

;; Get input data

(def input-file "data-set/two-spirals.data")

(def data (shuffle (parse-two-spirals-data (slurp input-file))))

(def draw-raw-data #(save-chart (view-chart (build-data-chart data)) :file-name "two-spirals.png"))

;; Define the network

(def net [(build-layer 2 10 fun/sigmoid :sqr true)
          (build-layer 10 1 fun/purelin :sqr true)])

;; Backprop

(defn- calc-F
  [f x]
  (let [F (m/identity-matrix (count x))]
    (m/emap (fun/partial-function f) F)))

(defn- seq-matrix
  [x]
  (let [len (count x)
        mat (repeat-v len 0)]
    (mapv (fn [index] (m/mset mat index (nth x index))) (range len))))

(defn- calc-J
  [u v F x]
  (mat/+ (m/mmul F v)
         (mat/* 2
                (m/mmul (seq-matrix x) (m/mmul F u)))))

(defn- calc-s
  [s-seq {x :x f :f {u :u v :v} :mat}]
  (let [s (first s-seq)
        F-k (calc-F f x)
        J-k (calc-J u v F-k x)
        s-k (m/mmul s J-k)]
    (conj s-seq s-k)))

(defn- update-layer
  [layer s x]
  (let [{{u :u v :v} :mat b :b} layer
        db (mat/* alpha s)
        dv (m/mmul (m/transpose [s]) x)
        du (m/mmul (m/transpose [s]) (m/square x))]
    (-> layer
        (assoc :mat {:u (mat/- u (mat/* alpha du))
                     :v (mat/- v (mat/* alpha dv))})
        (assoc :b (mat/- b (mat/* alpha db))))))

(defn train-machine
  "Apply the net with a data and adapt the weight."
  [net {x-0 :pos t :value}]
  (let [output (run-machine net x-0)
        result (first output)
        new-net (second output)
        error (mat/- t result)
        F-m (calc-F (:f (last new-net)) result)
        s-m (m/mmul (mat/* -2 error) F-m)
        s-seq (apply vector
                     (reduce calc-s (list s-m) (reverse new-net)))]
    (mapv update-layer net (rest s-seq)
          (vector [x-0] (pop (mapv :x new-net))))))

(defn- train-data-1
  [net times]
  (loop [now net
         t times]
    (if (zero? t)
      now
      (recur (train-machine now (first data))
             (dec t)))))

(defn pass-data-set
  [net data times]
  (loop [now net
         t times]
    (if (zero? t)
      now
      (recur (reduce train-machine now data)
             (dec t)))))

;;; Train the data
(defn- map-value [x] (* 6.0 (/ (- x 50) 50)))

(defn- format-data
  [net]
  (->> (range 10000)
       (mapv (fn [x] [(mod x 100) (quot x 100)]))
       (mapv (fn [[x y]] [(map-value x) (map-value y)]))
       (mapv (fn [x] {:pos x :value (first (first (run-machine net x)))}))
       (mapv (fn [x] (assoc x :value (fun/hardlim (:value x)))))))

(defn- print-result
  [net name]
  (let [data (format-data net)]
    (save-chart (-> (build-data-chart data :title (str "Result -0." name))
                    view-chart)
                :file-name (str "result-" name ".png"))))

(defn try-alpha
  [name]
  (-> (pass-data-set net data 1000)
      (print-result name)))
