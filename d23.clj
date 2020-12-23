(ns d23
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1 "389125467")

(defn- turn [v]
  (let [top (apply max v)
        cur (v 0)
        pick (subvec v 1 4)
        tail (subvec v 4)
        dst (->> (range (- cur 2) (- cur top 1) -1)
                 (map #(inc (mod % top)))
                 (remove (set pick))
                 first)
        i (inc (.indexOf ^clojure.lang.PersistentVector tail dst))]
    (vec (concat (subvec tail 0 i) pick (subvec tail i) [cur]))))

(defn one [s n]
  (let [v (mapv #(Long/parseLong (str %)) (str/trim s))
        v' (first (drop n (iterate turn v)))
        i (.indexOf ^clojure.lang.PersistentVector v' 1)]
    (apply str (concat (subvec v' (inc i)) (subvec v' 0 i)))))

(deftest t-1
  (is (= "92658374" (one test1 10))))

(defn- make-state [s n]
  (let [v (map #(Long/parseLong (str %)) (str/trim s))
        v (vec (concat v (range 10 n)))
        arr (long-array (inc (count v)))]
    (doseq [i (range (count v)) :let [j (mod (inc i) (count v))]]
      (aset-long arr (v i) (v j)))
    [(first v) arr]))

(defn two [s n moves]
  (let [[head ^longs arr] (make-state s (inc n))
        top (dec (count arr))]
    (loop [head head
           moves moves]
      (let [n1 (aget arr head)
            n2 (aget arr n1)
            n3 (aget arr n2)
            dst (->> (range (- head 2) (- head 6) -1)
                     (map #(inc (mod % top)))
                     (remove #{n1 n2 n3})
                     first)
            n3-> (aget arr n3)
            dst-> (aget arr dst)]
        (aset arr dst n1)
        (aset arr n3 dst->)
        (aset arr head n3->)

        (if (= 1 moves)
          (let [n1 (aget arr 1)
                n2 (aget arr n1)]
            (* n1 n2))
          (recur n3-> (dec moves)))))))

(deftest t-2
  #_(is (= 149245887792 (two test1 1000000 10000000))))

(defn -main [& args]
  (let [input (slurp (or (first args) "d23.in"))]
    (println "1." (one input 100))
    (println "2." (two input 1000000 10000000))))
