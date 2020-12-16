(ns d16
  (:refer-clojure :exclude [==])
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
")

(defn- parse-rule [s]
  (if-let [[_ k & ds]
           (re-matches #"([^:]+):\s+(\d+)-(\d+)\s+or\s+(\d+)-(\d+)$" s)]
    (let [ds (->> ds
                  (map #(Long/parseLong %))
                  (partition 2)
                  (map (fn [[a b]] (set (range a (inc b)))))
                  (apply set/union))]
      [k ds])
    (throw (ex-info "bad rule" {:s s}))))

(defn- parse-rules [s]
  (->> s str/split-lines (map parse-rule)))

(defn- parse-tickets [s]
  (->> s str/split-lines rest (map u/string->vector)))

(defn one [s]
  (let [[rules my-t ts] (-> s (str/split #"\n\n"))
        rules (parse-rules rules)
        ts (parse-tickets ts)
        all-rules (apply set/union (map last rules))]
    (->> ts (apply concat) (remove all-rules) (apply +))))

(deftest t-1
  (is (= 71 (one test1)))) 

(defn- tpose [tickets]
  (mapv (fn [i] (mapv #(% i) tickets)) (range (count (first tickets)))))

(defn- infer-fields [rules tcols]
  (for [[r-name r-set] rules
        col (range (count tcols))
        :let [t-col (tcols col)]
        :when (every? r-set t-col)]
    [r-name col]))

(defn two [s]
  (let [[rules my-t ts] (-> s (str/split #"\n\n"))
        rules (parse-rules rules)
        all-rules (apply set/union (map last rules))
        my-t (last (parse-tickets my-t))
        ts (filter #(every? all-rules %) (parse-tickets ts))
        matches
        (reduce
          (fn [h [r-name r-col]] (update h r-name #(conj (or % #{}) r-col)))
          {} (infer-fields rules (tpose ts)))

        matches (vec (sort-by #(count (val %)) matches))

        vars (map l/lvar matches)
        idxs (l/run 1 [q]
                    (l/== q vars)
                    (fd/distinct vars)
                    (l/everyg
                      (fn [[v [_ vs]]] (fd/in v (apply fd/domain vs)))
                      (zipmap vars matches)))]
    (->> idxs
         first
         (zipmap (map first matches))
         (filter #(= 0 (str/index-of (key %) "departure")))
         vals
         (map #(my-t %))
         (apply *))))

(deftest t-2
  (is (= (two test1))))

(defn -main [& args]
  (let [input (slurp "d16.in")]
    (println "1." (one input))
    (println "2." (two input))))
