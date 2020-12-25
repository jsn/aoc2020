(ns d25
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"5764801
17807724")

(defn- encrypt [sn ls]
  (loop [ls ls
         v 1]
    (if (zero? ls) v
      (recur (dec ls) (long (rem (* v sn) 20201227))))))

(defn- force-enc [sn lim match?]
  (loop [i 0
         v 1]
    (if (>= i lim) nil
      (if-let [mv (match? v)] [mv i]
        (recur (inc i) (long (rem (* v sn) 20201227)))))))

(deftest t-enc
  (is (= 5764801 (encrypt 7 8)))
  (is (= 17807724 (encrypt 7 11)))
  (is (= (force-enc 7 200 #{5764801 17807724}))))

(defn one [s]
  (let [[cp dp] (->> s str/split-lines (map #(Long/parseLong %)))
        [p ls] (force-enc 7 2000000 (set [cp dp]))
        co-p (if (= p cp) dp cp)]
    (encrypt co-p ls)))

(deftest t-1
  (is (= 14897079 (one test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) "d25.in"))]
    (println "1." (one input))))
