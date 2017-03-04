(ns neural-network.transport-function
  (:require [clojure.math.numeric-tower :as math]))

;; (expt 2 200)
;; => 1606938044258990275541962092341162602522202993782792835301376

(defn hardlim
  [x]
  (if (< x 0)
    0
    1))

(defn hardlims
  [x]
  (if (< x 0)
    -1
    1))

(def purelin (fn [x] x))

(defn satlin
  [x]
  (cond
    (< x 0) 0
    (< 1 x) 1
    :else x))

(defn satlins
  [x]
  (cond
    (< x -1) -1
    (< 1 x) 1
    :else x))

(def logsig (fn [x] x))
