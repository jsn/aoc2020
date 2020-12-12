(ns d12
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"F10
N3
F7
R90
F11")

(defn- parse1 [s]
  (if-let [[_ d arg] (re-matches #"^([NSEWFLR])(\d+)$" s)]
    [(keyword d) (Integer/parseInt arg)]
    (throw (ex-info "can't parse" {:s s}))))

(defn- parse [s]
  (->> s str/split-lines (map parse1)))

(defn- v+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn- v*k [[x y] k] [(* k x) (* k y)])

(defn- turn [{:keys [dir pos] :as ctx} angle]
  (have! zero? (rem angle 90))
  (let [angle (mod (quot angle 90) 4)
        [x y] dir]
    (case (long angle)
      ; 0 ctx
      1 (assoc ctx :dir [y (- x)])
      2 (assoc ctx :dir [(- x) (- y)])
      3 (assoc ctx :dir [(- y) x])
      (throw (ex-info "bad angle" {:angle angle})))))

(defn- move [ctx dir n] (update ctx :pos v+ (v*k dir n)))

(defn- run1 [{:keys [dir pos] :as ctx} [op arg :as cmd]]
  (case op
    :N (move ctx [0 1] arg)
    :S (move ctx [0 -1] arg)
    :E (move ctx [1 0] arg)
    :W (move ctx [-1 0] arg)
    :F (move ctx dir arg)
    :R (turn ctx arg)
    :L (turn ctx (- 360 arg))
    (throw (ex-info "bad op" {:cmd cmd :ctx ctx}))))

(defn one [s]
  (->> s parse (reduce run1 {:dir [1 0] :pos [0 0]}) :pos
       (map #(Math/abs ^long %)) (apply +)))

(deftest t1
  (is (= 25 (one test1))))

(defn- move-wp [ctx dir n] (update ctx :dir v+ (v*k dir n)))

(defn- run2 [{:keys [dir pos] :as ctx} [op arg :as cmd]]
  (case op
    :N (move-wp ctx [0 1] arg)
    :S (move-wp ctx [0 -1] arg)
    :E (move-wp ctx [1 0] arg)
    :W (move-wp ctx [-1 0] arg)
    :F (move ctx dir arg)
    :R (turn ctx arg)
    :L (turn ctx (- 360 arg))
    (throw (ex-info "bad op" {:cmd cmd :ctx ctx}))))

(defn two [s]
  (->> s parse (reduce run2 {:dir [10 1] :pos [0 0]}) :pos
       (map #(Math/abs ^long %)) (apply +)))

(deftest t2
  (is (= 286 (two test1))))

(defn -main [& args]
  (let [input (slurp "d12.in")]
    (println "1." (one input))
    (println "2." (two input))))
