(ns d18
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- parse1 [s] (read-string (str \( s \))))

(def ^:private OPS {'* * '+ +})

(defn- evaluate [e]
  (match e
    (a :guard int?) a
    ([a] :seq) (evaluate a)

    ([a (op :guard OPS) b & tail] :seq)
    (evaluate (cons ((OPS op) (evaluate a) (evaluate b)) tail))))

(deftest t-evail
  (is (= (evaluate 1)))
  (is (= (evaluate '(1))))
  (is (= (evaluate '(2 * 2))))
  (is (= (evaluate (parse1 "2 * 3 + (4 * 5)")) 26))
  (is (= (evaluate (parse1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")) 437))
  (is (= (evaluate (parse1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))
         12240)))

(defn one [s] (->> s str/split-lines (map (comp evaluate parse1)) (apply +)))

(defn- evaluate2[e]
  (match e
    (a :guard int?) a
    ([a] :seq) (evaluate2 a)

    ([a '+ b & tail] :seq)
    (evaluate2 (cons (+ (evaluate2 a) (evaluate2 b)) tail))

    ([a '* b '+ c & tail] :seq)
    (evaluate2 (apply list a '* (+ (evaluate2 b) (evaluate2 c)) tail))

    ([a '* b & tail] :seq)
    (evaluate2 (cons (* (evaluate2 a) (evaluate2 b)) tail))))

(deftest t-eval2
  (is (= (evaluate2 (parse1 "1 + 2 * 3 + 4 * 5 + 6")) 231))
  (is (= (evaluate2 (parse1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))
         23340)))

(defn two [s] (->> s str/split-lines (map (comp evaluate2 parse1)) (apply +)))

(defn -main [& args]
  (let [input (slurp "d18.in")]
    (println "1." (one input))
    (println "2." (two input))))
