(ns neural-network.parser
  (:require [instaparse.core :as insta])
  (:use [neural-network.util]
        [clojure.repl]))

(defn parse-two-spirals-data
  [s]
  (->> (str "[" s "]")
       read-string
       (partition 3)
       (mapv (fn [s] {:pos [(first s) (second s)]
                     :value (last s)}))))
