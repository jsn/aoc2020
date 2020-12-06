(ns d06
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]
            [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s
"abc

a
b
c

ab
ac

a
a
a
a

b")

(defn- count-one [s op]
  (->> s str/split-lines (map set) (apply op) count))

(defn- count-all [s op]
  (->> (str/split s #"\n\n") (map #(count-one % op)) (apply +)))

(defn- run-one [s] (count-all s set/union))
(defn- run-one [s] (count-all s set/intersection))

(defn one [] (-> "d06.in" slurp run-one))
(defn two [] (-> "d06.in" slurp run-two))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 100 100))))
