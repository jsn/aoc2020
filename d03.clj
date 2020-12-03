(ns d03
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [util])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s
"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defn pic->world [pic]
  (let [a (str/split-lines pic)
        w (count (first a))
        h (count a)
        cells (zipmap (for [y (range h) x (range w)] [x y]) (mapcat seq a))]
    {:w w :h h :cells cells}))

(defn- v* [v k] (mapv #(* k %) v))

(defn- run [pic step]
  (let [{:keys [w h cells]} (pic->world pic)
        spots (take-while #(< (last %) h) (map v* (repeat step) (range)))
        wrapped (map (fn [[x y]] [(rem x w) y]) spots)
        objects (map cells wrapped)]
    (count (filter #{\#} objects))))

(defn- run-one [pic] (run pic [3 1]))

(defn one [] (-> "d03.in" slurp run-one))

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn- run-two [pic] (apply * (map #(run pic %) slopes)))

(defn two [] (-> "d03.in" slurp run-two))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing 1
    (is (= (run-one test1-s) 7)))
  (testing 2
    (is (= (run-two test1-s) 336))))
