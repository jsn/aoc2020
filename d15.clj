(ns d15
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn one [s tgt]
  (let [in (u/string->vector s)
        xs (pop in)
        x (peek in)]
  (loop [nums (zipmap xs (range 1 1e6))
         x x
         i (count in)]
    (if (= i tgt) x
      (let [x' (- i (nums x i))
            nums' (assoc nums x i)
            i' (inc i)]
        (recur nums' x' i'))))))

(deftest t-1
  (is (= [0 3 3 1 0 4 0] (map #(one "0,3,6" %) (range 4 11)))))

(defn -main [& args]
  (let [input (slurp "d15.in")]
    (println "1." (one input 2020))
    (println "2." (one input 30000000))))
