(ns d02
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [util])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s
"1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(defn- parse-line [s]
  (let [m (re-matches #"^(\d+)-(\d+)\s+([a-z]):\s+(\S+)$" s)]
    [(Integer/parseInt (m 1)) (Integer/parseInt (m 2)) (get (m 3) 0) (m 4)]))

(defn- line-valid? [[n1 n2 ch s]] (<= n1 (count (filter #{ch} s)) n2))

(defn- run-one [input]
  (->> input str/split-lines (map parse-line) (filter line-valid?) count))

(defn one [] (-> "d02.in" slurp run-one))

(defn- line-valid2? [[n1 n2 ch s]]
  (->> [n1 n2] (map dec) (map #(get s %)) (filter #{ch}) count (= 1)))

(defn- run-two [input]
  (->> input str/split-lines (map parse-line) (filter line-valid2?) count))

(defn two [] (-> "d02.in" slurp run-two))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "one"
    (is (= 2 (run-one test1-s))))
  (testing "two"
    (is (= 1 (run-two test1-s)))))
