(ns neural-network.chart
  (:require [incanter.core :as in])
  (:use [incanter stats charts]))


;;; Data Chart
;;; Use scatter-plot to show the origin data.

(defn build-data-chart
  "Initialize an data chart.

  options:
  :title (default \"Origin Data\") main title
  :x (default \"x\")
  :y (default \"y\")
  :label (default x expression)
  :legend (default false)
  :default-group (default nil)

  Input format:
  Input seq should be a vector of items.
  Each item should in the format of:
  {:pos [x-card y-card]
  :value class-label}
  "
  [seq & options]
  (let [opts (apply hash-map options)
        y-label (or (:y opts) "y")
        x-label (or (:x opts) "x")
        series-label (:label opts)
        title (or (:title opts) "Origin Data")
        legend (:legend opts)
        default-group (:defaut-group opts)
        group (mapv #(or (:value %) default-group) seq)
        x (mapv #(first (:pos %)) seq)
        y (mapv #(second (:pos %)) seq)
        ]
    (scatter-plot  x y
                   :group-by group
                   :title title
                   :x-label x-label
                   :y-label y-label
                   :series-label series-label
                   :legend legend)))

(defn add-straight-line-to-data-chart
  "Add a straight line to an data chart.

  Options:
  :label (default x expression)
  :step-size (default (/ (- max-range min-range) 500))

  Format:
  The line is lambda * [x y]^T = c
  "
  [ch lambda c min-range max-range & options]
  (let [opts (apply hash-map options)
        series-label (:label opts)
        step-size (:step-size opts)
        a (first lambda)
        b (second lambda)
        f (if (zero? b)
            (fn [x] c)
            #(/ (- c (* a %)) b))]
    (add-function ch f min-range max-range
                  :series-label series-label
                  :step-size step-size)))

  ;;; Result Chart
  ;;; Use xy-plot to show how the error rate goes.
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
  (in/view chart)
  chart)

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
        file-name (or (:file-name opts) (str chart ".png"))
        path (or (:path opts) "imgs//") ]
    (.mkdir (java.io.File. path))
    (in/save chart (str path file-name)
             :width width
             :height height)
    (str path file-name)))
