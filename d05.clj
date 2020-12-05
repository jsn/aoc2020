(ns d05
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [util])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s "FBFBBFFRLR")

(defn- parse-part [s dict]
  (Integer/parseInt (->> s (replace dict) (apply str)) 2))

(defn- bpass->int [bp]
  (let [row (-> bp (subs 0 7) (parse-part {\F \0 \B \1}))
        seat (-> bp (subs 7 10) (parse-part {\R \1 \L \0}))]
    (+ (* row 8) seat)))

(def input-sids (->> "d05.in" slurp str/split-lines (map bpass->int)))

(defn one [] (apply max input-sids))

(defn two []
  (let [bset (set input-sids)]
    (filter #(and (not (bset %)) (bset (inc %))) (map inc input-sids))))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing 1
    (is (= (bpass->int test1-s) 357))
    (is (= (bpass->int "BFFFBBFRRR") 567))
    (is (= (bpass->int "FFFBBBFRRR") 119))
    (is (= (bpass->int "BBFFBBFRLL") 820))))
