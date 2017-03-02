(ns neural-network.chart
  (:require [incanter.core :as in]
            [clojure.repl :as repl]
            )
  (:use [incanter stats charts]))

;;; use xy-plot to display result chart
;;; the 3 seqs are for testing
(def er1 (take 100 (repeat 1)))
(def er2 (take 100 (repeat 0.1)))
(def er3 (take 100 (repeat 0.3)))

(defn build-result-chart
  "Initialize an result chart.

  options:
  :title (default \"Training Result\") main title
  :x (default \"Times\")
  :y (default \"Error Rate\")
  :label (default x expression)

  examples:

  (def line1 (take 100 (repeat 0.5)))
  (def ch (build-result-chart line1 :label \"line-1\"))
  (view-chart ch)
  "
  [seq & options]
  (let [opts (apply hash-map options)
        y-label (or (:y opts) "Error Rate")
        x-label (or (:x opts) "Times")
        series-label (:label opts)
        title (or (:title opts) "Training Result")
        n (count seq)]
    (xy-plot (range n) seq
             :title title
             :x-label x-label
             :y-label y-label
             :series-label series-label
             :legend true)))

(defn add-line-to-result-chart
  "Add a new line to an result chart.

  options:
  :label (default x expression)

  examples:
  (def line1 (take 100 (repeat 0.5)))
  (def line2 (take 100 (repeat 0.1)))
  (def ch (build-result-chart line1 :label \"line-1\"))
  (view-chart ch)
  (add-line-to-result-chart ch line2 :label \"line-2\")
  (view-chart ch)
  "
  [ch seq & options]
  (let [opts (apply hash-map options)
        series-label (:label opts)
        n (count seq)]
    (add-lines ch (range n) seq
               :series-label series-label)))


(defn view-chart
  "Print the chart."
  [chart]
  (in/view chart))

(defn save-chart
  "Save the chart in the imgs folder as a png file.
  And return the path where you can get the png file.

  options:
  :width (defalut 500) chart width
  :height (default 400) chart height
  :file-name (default (str chart)) name of png file
  :path (default in img//file-name) the folder to store the file

  example:
  (def line1 (take 100 (repeat 0.5)))
  (def line2 (take 100 (repeat 0.1)))
  (def ch (build-result-chart line1 :label \"line-1\"))
  (view-chart ch)
  (save-chart ch :file-name \"chart-1\")
  (save-chart ch :file-name \"chart-1\" :path \"img2/\")
  "
  [chart & options]
  (let [opts (apply hash-map options)
        width (:width opts)
        height (:height opts)
        file-name (or (:file-name opts) (str chart))
        path (or (:path opts) "imgs//") ]
    (.mkdir (java.io.File. path))
    (in/save chart (str path file-name)
             :width width
             :height height)
    (str path file-name)))
