(ns neural-network.hw-1
  (:require [neural-network.chart :as ch]
            [clojure.repl :as r]
            [incanter.core :as in])
  (:use [incanter stats charts]
        [neural-network.mathematic-function]))

(defn- point
  [x y t]
  {:pos [x y]
   :value t})

(def p1 (point 1 -1 1))
(def p2 (point -1 -1 1))
(def p3 (point 0 0 0))
(def p4 (point 1 0 0))
(def p5 (point -2 0 nil))
(def p6 (point 1 1 nil))
(def p7 (point 0 1  nil))
(def p8 (point -1 -2 nil))

(defn solve1-1
  []
  (let [ch-1 (ch/build-data-chart [p1 p2 p3 p4])
        ans1-1 (ch/add-straight-line-to-data-chart ch-1 [0 -1] 0.5 -2 2)]
    (ch/view-chart ans1-1)
    (ch/save-chart ans1-1 :file-name "ans1-1.png")
   ))

(defn solve1-4
  []
  (let [ch-1 (ch/build-data-chart
              [p1 p2 p3 p4 p5 p6 p7 p8] :default-group 2)]
    (-> ch-1
        (ch/add-straight-line-to-data-chart [0 1] 0 0 1)
        (ch/add-straight-line-to-data-chart [0 1] -1 -1 1)
        (ch/add-straight-line-to-data-chart [1 -2] 1 -2 2)
        (ch/add-straight-line-to-data-chart [1 1] 0 -2 2)
        (add-text 0.5 1.5 "S1")
        (add-text 0 -1.5 "S2")
        ch/view-chart
        (ch/save-chart :file-name "ans1-4.png"))
    ))

(defn- pass-data
  [{times :times w :w b :b flag :flag} {pos :pos value :value}]
  (let [a (reduce + b (map * w pos))
        res (hardlim a)
        e (- value res)]
    (println "No." times ": w = " w "b = " b " data = " pos "t = " value)
    {:times (inc times)
     :w (mapv + w (mapv (partial * e) pos))
     :b (+ b e)
     :flag (and flag (zero? e))}))

(defn- train-perception
  [para data-set]
  (let [net (assoc para :flag true)
        result (reduce pass-data net data-set)
        flag (:flag result)]
    (if flag
      result
      (recur result data-set))))

(defn solve1-5
  []
  (let [result (train-perception
                {:times 0 :w [0 0] :b 0 :flag true}
                [p1 p2 p3 p4])]
    (-> (ch/build-data-chart [p1 p2 p3 p4]
                             :title "Perception Machine")
        (ch/add-straight-line-to-data-chart
         (:w result)
         (- 0 (:b result))
         -2 2)
        (ch/save-chart :file-name "ans1-5.png"))))

(defn- calc-lms
  [{w :w b :b} {p :pos}]
  (reduce + b (mapv * w p)))

(defn- delta
  [para data]
  (let [e (- (:value data) (calc-lms para data) )
        k (* 2 (:alpha para) e)]
    {:b k
     :w (mapv (partial * k) (:pos data))}
    ))

(defn- train-lms
  [para data]
  (let [d (delta para data)]
    (-> para
        (assoc :b (+ (:b d) (:b para)))
        (assoc :w (mapv + (:w d) (:w para)))
        )))

(def avg-map (fn [s f] (/ (reduce + (mapv f s)) (count s))))

(defn update-e
  [para data-set]
  (let [e (:e para)
        f #(fun/square (- (:value %) (calc-lms para %)))
        new-e (Math/log (avg-map data-set f))]
    (assoc para :e (conj e new-e))
    ))

(defn run-lms
  [para data-set]
  (loop [p para
         t 5000]
    (if (zero? t)
      p
      (recur (update-e (reduce train-lms p data-set) data-set)
             (- t 1))
      )))

(defn lms-train
  [alpha & options]
  (let [opt (apply hash-map options)
        para {:w [1 1] :b 1 :alpha alpha :e []}
        data-set (or (:data-set opt) [p1 p2 p3 p4])]
    (println data-set)
    (run-lms para data-set))
)

(defn run-lms-and-get-e
  [& options]
  (let [opt (apply hash-map options)
        alpha (or (:alpha opt) 0.01)
        file-name (or (:file-name opt) "tmp")
        title (:title opt)
        data-chart (ch/build-data-chart [p1 p2 p3 p4]
                                        :title title)
        result (lms-train alpha)
        ans-chart (ch/add-straight-line-to-data-chart
                   data-chart
                   (:w result)
                   (- 0 (:b result))
                   -2 2)]
    (ch/save-chart ans-chart :file-name file-name)
    (:e result)
   ))

(defn solve2
  []
  (-> (ch/build-result-chart (run-lms-and-get-e :alpha 0.01
                                                :title "LMS -0.01"
                                                :file-name "ans2-1.png")
                             :x "Times"
                             :y "log(E)"
                             :label "alpha = 0.01")
      (ch/add-line-to-result-chart
       (run-lms-and-get-e :alpha 0.1
                          :title "LMS -0.1" :file-name "ans2-2.png")
       :label "alpha = 0.1")
      (ch/add-line-to-result-chart
       (run-lms-and-get-e :alpha 0.5
                          :title "LMS -0.5" :file-name "ans2-3.png")
       :label "alpha = 0.5")
      ch/view-chart
      (ch/save-chart :file-name "ans2.png")
      ))


(defn draw-addition
  []
    (let [alpha 0.01
          file-name "ans2-addition.png"
          title "LMS -addition"
          translate (fn [x] (assoc x :value (* 2 (- (:value x) 0.5))))
          data-set (mapv translate [p1 p2 p3 p4])
          data-chart (ch/build-data-chart
                      data-set
                      :title title)
          result (lms-train alpha :data-set data-set)
          ans-chart (ch/add-straight-line-to-data-chart
                     data-chart
                     (:w result)
                     (- 0 (:b result))
                     -2 2)]
    (ch/save-chart ans-chart :file-name file-name)
   )
)
