(ns d22
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(defn- parse-player [s]
  (->> s str/split-lines rest (mapv #(Long/parseLong %))))

(defn- parse [s]
  (->> (str/split s #"\n\n") (map parse-player)))

(defn one [s]
  (let [[p1 p2] (parse s)]
    (loop [[c1 & c1s :as p1] p1
           [c2 & c2s :as p2] p2]
      (cond
        (not (and c1 c2))
        (->> (if c1 p1 p2) reverse (map * (rest (range))) (apply +))

        (< c1 c2) (recur (vec c1s) (conj (vec c2s) c2 c1))
        (> c1 c2) (recur (conj (vec c1s) c1 c2) (vec c2s))
        :else (throw (ex-info "oops" {:p1 p1 :p2 p2}))))))

(deftest t-1
  (is (= 306 (one test1))))

(defn- play2 [p1 p2 gen]
  (loop [[c1 & c1s :as p1] p1
         [c2 & c2s :as p2] p2
         seen #{}]
    (let [seen' (conj seen [p1 p2])]
      (cond
        (seen [p1 p2]) [1 p1]
        (not (and c1 c2)) (if c1 [1 p1] [2 p2])

        (and (<= c1 (count c1s)) (<= c2 (count c2s)))
        (let [[winner _]
              (play2 (vec (take c1 c1s)) (vec (take c2 c2s)) (inc gen))]
          (if (= 1 winner)
            (recur (conj (vec c1s) c1 c2) (vec c2s) seen')
            (recur (vec c1s) (conj (vec c2s) c2 c1) seen')))

        (< c1 c2) (recur (vec c1s) (conj (vec c2s) c2 c1) seen')
        (> c1 c2) (recur (conj (vec c1s) c1 c2) (vec c2s) seen')
        :else (throw (ex-info "oops" {:p1 p1 :p2 p2}))))))

(defn two [s]
  (let [[p1 p2] (parse s)
        [_ deck] (play2 p1 p2 1)]
    (->> deck reverse (map * (rest (range))) (apply +))))

(deftest t-2
  (is (= 291 (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) "d22.in"))]
    (println "1." (one input))
    (println "2." (two input))))
