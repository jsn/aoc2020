(ns d21
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(defn- parse1 [s]
  (let [[_ ings alls]
        (have! (re-matches #"((?:\w+\s+)+)\(contains\s+([^\)]+)\)" s))]
    (mapv #(set (str/split % #"[,\s]+")) [ings alls])))

(defn- infer-alls [foods]
  (loop [done {}
         queue #{}]
    (if-let [all (->> foods
                      (mapcat peek)
                      (remove done)
                      (remove queue)
                      frequencies
                      (sort-by val)
                      last
                      first)]
      (let [[ing & more] (->> foods
                              (keep (fn [[ings as]] (when (as all) ings)))
                              (apply set/intersection)
                              (remove (set (vals done))))]
        (if-not more
          (recur (assoc done all ing) queue)
          (recur done (conj queue all))))
      (if (empty? queue) done
        (recur done #{})))))

(defn one [s]
  (let [foods (->> s str/split-lines (map parse1))
        alls (->> foods infer-alls vals set)]
    (->> foods (mapcat first) (remove alls) count)))

(deftest t-1
  (is (= 5 (one test1))))

(defn two [s]
  (let [foods (->> s str/split-lines (map parse1))
        alls (infer-alls foods)]
    (->> alls keys sort (map alls) (str/join ","))))

(deftest t-2
  (is (= "mxmxvkd,sqjhc,fvjkl" (two test1))))

(defn -main [& args]
  (let [input (slurp (or (first args) "d21.in"))]
    (println "1." (one input))
    (println "2." (two input))))
