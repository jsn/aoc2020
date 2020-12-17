(ns d17bis
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [clojure.math.combinatorics :as combo]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
".#.
..#
###")

(defn- pic->world [pic & pad]
  (let [a (str/split-lines pic)
        w (count (first a))
        h (count a)]
    (as-> (for [y (range h) x (range w)] (apply conj [x y] pad)) $
        (zipmap $ (mapcat seq a))
        (keep (fn [[point tile]] (when (= tile \#) point)) $)
        (set $))))

(defn- neighbs [p]
  (->> (repeat (count p) [-1 0 1])
       (apply combo/cartesian-product)
       (keep #(when-not (every? zero? %) (mapv + p %)))))

(deftest t-neighs
  (is (= 26 (count (neighbs [0 0 0]))))
  (is (= 80 (count (neighbs [0 0 0 0])))))

(defn- step [world]
  (->> world
       (mapcat neighbs)
       frequencies
       (keep (fn [[p n]] (when (or (and (world p) (#{2 3} n)) (= 3 n)) p)))
       set))

(defn- run [world n] (-> (iterate step world) (nth n) count))

(defn one [s] (run (pic->world s 0) 6))

(deftest t-1
  (is (= 112 (one test1))))

(defn two [s] (run (pic->world s 0 0) 6))

(deftest t-2
  (is (= 848 (two test1))))

(defn -main [& args]
  (let [input (slurp "d17.in")]
    (println "1." (one input))
    (println "2." (two input))))
