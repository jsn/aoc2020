(ns d17
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
".#.
..#
###")

(defn- pic->world [pic]
  (let [a (str/split-lines pic)
        w (count (first a))
        h (count a)
        cells (zipmap (for [y (range h) x (range w)] [x y 0]) (mapcat seq a))]
    {:p1 [0 0 0] :p2 [(dec w) (dec h) 0] :cells cells}))

(defn- neighbs [[x y z]]
  (let [r [-1 0 1]]
    (for [dx r dy r dz r :when (not (= 0 dx dy dz))]
      [(+ x dx) (+ y dy) (+ z dz)])))

(deftest t-neighbs
  (is (= 26 (count (neighbs [0 0 0])))))

(defn- step [{:keys [p1 p2 cells] :as world}]
  (let [p1 (mapv dec p1)
        p2 (mapv inc p2)
        cells
        (into {}
              (for [x (range (p1 0) (inc (p2 0)))
                    y (range (p1 1) (inc (p2 1)))
                    z (range (p1 2) (inc (p2 2)))
                    :let [p [x y z]
                          curr (cells p \.)
                          nbs (->> p neighbs
                                   (map #(cells %))
                                   (filter #{\#})
                                   count)]]
                [p
                 (case curr
                   \. (if (= 3 nbs) \# \.)
                   \# (if (#{2 3} nbs) \# \.)
                   (throw (ex-info "bad tile" {:c curr :p p})))]))]
    {:p1 p1 :p2 p2 :cells cells}))

(defn one [s n]
  (let [steps (iterate step (pic->world s))]
    (-> steps (nth n) :cells vals frequencies (get \#))))

(deftest t-1
  (is (= 112 (one test1 6))))

(defn- pic->world2 [pic]
  (let [a (str/split-lines pic)
        w (count (first a))
        h (count a)
        cells (zipmap (for [y (range h) x (range w)] [x y 0 0]) (mapcat seq a))]
    {:p1 [0 0 0 0] :p2 [(dec w) (dec h) 0 0] :cells cells}))

(defn- neighbs2 [[x y z w]]
  (let [r [-1 0 1]]
    (for [dx r dy r dz r dw r :when (not (= 0 dx dy dz dw))]
      [(+ x dx) (+ y dy) (+ z dz) (+ w dw)])))

(deftest t-neighbs2
  (is (= 80 (count (neighbs2 [0 0 0 0])))))

(defn- step2 [{:keys [p1 p2 cells] :as world}]
  (let [p1 (mapv dec p1)
        p2 (mapv inc p2)
        cells
        (into {}
              (for [x (range (p1 0) (inc (p2 0)))
                    y (range (p1 1) (inc (p2 1)))
                    z (range (p1 2) (inc (p2 2)))
                    w (range (p1 3) (inc (p2 3)))
                    :let [p [x y z w]
                          curr (cells p \.)
                          nbs (->> p neighbs2
                                   (map #(cells %))
                                   (filter #{\#})
                                   count)]]
                [p
                 (case curr
                   \. (if (= 3 nbs) \# \.)
                   \# (if (#{2 3} nbs) \# \.)
                   (throw (ex-info "bad tile" {:c curr :p p})))]))]
    {:p1 p1 :p2 p2 :cells cells}))

(defn two [s n]
  (let [steps (iterate step2 (pic->world2 s))]
    (-> steps (nth n) :cells vals frequencies (get \#))))

(deftest t-2
  #_(is (= 848 (two test1 6)))
  (is (= 320 (two test1 3))))

(defn -main [& args]
  (let [input (slurp "d17.in")]
    (println "1." (one input 6))
    (println "2." (two input 6))))
