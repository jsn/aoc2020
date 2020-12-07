(ns d07
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s
"light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn- parse-tail [s]
  (if (re-matches #"^no other bags$" s) {}
    (have! not-empty
           (into {}
                 (map (fn [[_ n color]] [color (Integer/parseInt n)])
                      (re-seq #"(\d+) (\S+ \S+) bag(?:s)?" s))))))

(defn- parse-line [s]
  (if-let [[_ src tail]
           (re-matches #"(\S+ \S+) bags contain (\S.*)\.$" s)]
    [src (parse-tail tail)]
    (throw (ex-info "bad format" {:line s}))))

(defn- parse [s]
  (->> s str/split-lines (map parse-line) (into {})))

(defn can-contain1 [rules spec]
  (map first (filter (fn [[k v]] (v spec)) rules)))

(defn- can-contain [rules spec]
  (loop [cands [spec]
         rv #{}]
    (let [cands' (remove rv (mapcat #(can-contain1 rules %) cands))
          rv' (apply conj rv cands')]
      (if (seq cands')
        (recur cands' rv')
        rv))))

(defn one [s]
  (let [rules (parse s)]
    (count (can-contain rules "shiny gold"))))

(defn- must-contain [rules spec n]
  (let [rule (rules spec)]
    (+ n (* n (apply + (map (fn [[spec' n']] (must-contain rules spec' n'))
                            rule))))))

(defn two [s]
  (let [rules (parse s)]
    (dec (must-contain rules "shiny gold" 1))))

(defn -main [& args]
  (println "1." (-> "d07.in" slurp one))
  (println "2." (-> "d07.in" slurp two)))
