(ns d10
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-1 "16 10 15 5 1 11 7 19 6 12 4")
(def test1-2
  "28 33 18 42 31 14 46 20 48 47 24 23 49 45 19
  38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3")

(defn- wrap [lst]
  (let [lst (->> lst sort (cons 0) vec)]
    (conj lst (+ 3 (peek lst)))))

(defn- diffs [lst]
  (->> lst wrap (partition 2 1) (map #(apply - %))))

(defn one [s]
  (let [freqs (->> s u/string->vector diffs frequencies)]
    (* (freqs -1) (freqs -3))))

(deftest t1
  (is (= 35 (one test1-1)))
  (is (= 220 (one test1-2))))

(defn- count-variants1 [^long n1s]
  (case n1s
    1 1 ; 1
    2 2 ; 11 2
    3 4 ; 111 21 12 3
    4 7 ; 1111 211 22 121 112 31 13
    (throw (ex-info "oops" {:n n1s}))))

(defn two [s]
  (->> s
       u/string->vector
       diffs
       (partition-by identity)
       (keep #(when (= -1 (first %)) (count %)))
       (map count-variants1)
       (apply *)))

(deftest t2
  (is (= (two test1-1) 8))
  (is (= (two test1-2) 19208)))

(defn -main [& args]
  (println "1." (-> "d10.in" slurp one))
  (println "2." (-> "d10.in" slurp two)))
