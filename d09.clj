(ns d09
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.math.combinatorics :as combo]
            [taoensso.truss :refer [have!]]
            [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
  "35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576")

(defn- valid? [p x]
  (seq
    (for [[a b] (combo/combinations p 2)
          :when (and (not= a b) (= x (+ a b)))]
      [a b])))

(defn one [s len]
  (first
    (let [in (u/string->vector s)]
      (for [i (range len (count in))
            :let [x (in i)
                  p (subvec in (- i len) i)]
            :when (not (valid? p x))]
        x))))

(defn- find-range [v x]
  (loop [i 0
         j 2]
    (if (> j (count v)) nil
      (let [sv (subvec v i j)
            sum (apply + sv)]
        (cond
          (= sum x) sv
          (> sum x) (recur (inc i) (max (+ 2 i) j))
          (< sum x) (recur i (inc j)))))))

(defn two [s len]
  (let [v (u/string->vector s)
        rv (one s len)
        rng (find-range v rv)]
    (+ (apply min rng)(apply max rng))))

(defn -main [& args]
  (println "1." (one (slurp "d09.in") 25))
  (println "2." (two (slurp "d09.in") 25)))

(deftest everything
  (testing 1
    (is (= [[15 25]] (valid? [35 20 15 25 47] 40)))
    (is (= (one test1 5) 127)))
  (testing 2
    (is (= (two test1 5) 62))))
