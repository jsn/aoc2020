(ns d01
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [util])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s
"1721
979
366
299
675
1456")

(defn- run-one [s]
  (let [input (util/string->vector s)
        len (count input)]
    (for [i (range (dec len))
          j (range (inc i) len)
          :let [x (input i) y (input j) xy (+ x y)]
          :when (= 2020 xy)]
      (* x y))))

(defn- run-two [s]
  (let [input (util/string->vector s)
        len (count input)]
    (for [i (range (- len 2))
          j (range (inc i) (dec len))
          k (range (inc j) len)
          :let [x (input i) y (input j) z (input k) xyz (+ x y z)]
          :when (= 2020 xyz)]
      (* x y z))))

(defn one [] (-> "d01.in" slurp run-one))

(defn two [] (-> "d01.in" slurp run-two))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "one"
    (is (= (run-one test1-s) [514579])))
  (testing "two"
    (is (= (run-two test1-s) [241861950]))))
