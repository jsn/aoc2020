(ns d13
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"939
7,13,x,x,59,x,31,19")

(defn one [s]
  (let [[now buses] (str/split-lines s)
        now (Integer/parseInt now)
        buses (map #(Integer/parseInt %) (str/split buses #"[x,]+"))
        bus (first (sort-by #(mod (- now) %) buses))]
    (* bus (mod (- now) bus))))

(deftest t1
  (is (= 295 (one test1))))

(defn- reduce-bus [[t & _] [_ xs period]]
  [(+ 1 t (/ (count xs) 2)) (Integer/parseInt period)])

(defn- solve2 [[r1 p1] [r2 p2]]
  (let [p (* p1 p2)
        r (->> p2
               range
               (map #(+ r1 (* p1 %)))
               (filter #(= (- p2 (rem r2 p2)) (rem % p2)))
               first)]
    [r p]))

(defn two [s]
  (->> s
       str/split-lines
       last
       (re-seq #"((?:x,)*)(\d+)")
       (reductions reduce-bus [-1])
       rest
       (reduce solve2)
       first))

(deftest t2
  (is (= 1068781 (two test1))))

(defn -main [& args]
  (let [input (slurp "d13.in")]
    (println "1." (one input))
    (println "2." (two input))))
