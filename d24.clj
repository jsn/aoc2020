(ns d24
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(def dir-re #"(?:e|se|sw|w|nw|ne)")

(defn- parse [s]
  (->> s str/split-lines (map #(map keyword (re-seq dir-re %)))))

(def DIRS
  {:e  [-1 0]
   :se [-1/2 -1]
   :sw [1/2 -1]
   :w  [1 0]
   :nw [1/2 1]
   :ne [-1/2 1]})

(defn- move1 [p dir] (mapv + p (have! (DIRS dir))))

(defn- dirs->pos [dirs] (reduce move1 [0 0] dirs))

(defn- input->blacks [s]
  (->> s parse (map dirs->pos) frequencies (remove (comp even? val)) keys set))

(defn one [s]
  (count (input->blacks s)))

(deftest t-one
  (is (= 10 (one test1))))

(defn- neighbs [p] (map #(move1 p %) (keys DIRS)))

(defn- step [blacks]
  (let [neis (->> blacks (mapcat neighbs) frequencies)
        diff (->> (into blacks (keys neis))
                  (map #(let [cnt (neis % 0)]
                           [%
                            (if (blacks %)
                              (if (or (zero? cnt) (> cnt 2)) :w :b)
                              (if (= 2 cnt) :b :w))]))
                  (group-by second))
        whites+ (->> diff :w (map first) set)
        blacks+ (->> diff :b (map first) set)]
    (-> blacks (set/difference whites+) (set/union blacks+))))

(deftest t-step
  (is (= (->> test1 input->blacks (iterate step) (map count) (take 11))
         [10 15 12 25 14 23 28 41 37 49 37])))

(defn two [s]
  (->> s input->blacks (iterate step) (drop 100) first count))

(deftest t-2
  (is (= 2208 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) "d24.in"))]
    (println "1." (one input))
    (println "2." (two input))))
