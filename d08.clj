(ns d08
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn- parse-inst [s]
  (let [[_ op arg] (have! (re-matches #"^([a-z]\w{2})\s+([-+]?\d+)$" s))]
    [(keyword op) (Integer/parseInt arg)]))

(defn- run [insts]
  (loop [pc 0
         acc 0
         seen #{}]
    (cond
      (seen pc) [:loop acc]
      (= pc (count insts)) [:exit acc]

      :else
      (let [[op ^long arg] (insts pc)
            seen (conj seen pc)]
        (case op
          :nop (recur (inc pc) acc seen)
          :acc (recur (inc pc) (+ acc arg) seen)
          :jmp (recur (+ pc arg) acc seen)
          (throw (ex-info "bad op" {:op op :arg arg :pc pc})))))))

(defn one [s]
  (->> s str/split-lines (mapv parse-inst) run))

(defn two [s]
  (let [insts (mapv parse-inst (str/split-lines s))]
    (for [i (range (count insts))
          :let [op' ({:jmp :nop :nop :jmp} (get-in insts [i 0]))]
          :when op'
          :let [insts (assoc-in insts [i 0] op')
                [stop rv] (run insts)]
          :when (= stop :exit)]
      rv)))

(defn -main [& args]
  (println "1." (->> "d08.in" slurp one))
  (println "2." (->> "d08.in" slurp two)))

(deftest everything
  (testing 1
    (is (= (one test1-s) [:loop 5])))
  (testing 2
    (is (= (two test1-s) [8]))))
