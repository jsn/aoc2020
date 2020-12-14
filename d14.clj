(ns d14
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(defn- parse-mask [s x] (Long/parseLong (str/replace s \X x) 2))

(defn- parse-cmd [s]
  (condp re-matches s
    #"^mask\s+=\s+([X01]{36})$"
    :>> (fn [[_ mask]] [:mask (parse-mask mask \1) (parse-mask mask \0)])

    #"^mem\[(\d+)\]\s+=\s+(\d+)$"
    :>> (fn [[_ addr v]] [:mem (Long/parseLong addr) (Long/parseLong v)])

    (throw (ex-info "bad command" {:cmd s}))))

(defn- reduce-cmd [{:keys [mem mask] :as ctx} cmd]
  (match cmd
    [:mask m-and m-or] (assoc ctx :mask [m-or m-and])
    [:mem idx v] (let [[m-or m-and] mask
                       v' (-> v (bit-or m-or) (bit-and m-and))]
                   (update ctx :mem assoc idx v'))))

(defn one [s]
  (->> s
       str/split-lines
       (map parse-cmd)
       (reduce reduce-cmd {:mem {} :mask [0 0]})
       :mem
       vals
       (apply +)))

(deftest t1
  (is (= 165 (one test1))))

(def test2
"mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(defn- reduce-pf [vs [^char c n]]
  (case c
    \0 vs
    \1 (mapv #(+ % (bit-shift-left 1 n)) vs)
    \X (into vs (mapv #(+ % (bit-shift-left 1 n)) vs))
    (throw (ex-info "bad char" {:c c}))))

(defn- parse-float [s]
  (reduce reduce-pf [0] (map vector (reverse s) (range))))

(deftest t-pf
  (is (= [2r010010 2r010011 2r110010 2r110011] (parse-float "X1001X"))))

(defn- parse-cmd2 [s]
  (condp re-matches s
    #"^mask\s+=\s+([X01]{36})$"
    :>> (fn [[_ mask]] [:mask (parse-float mask) (parse-mask mask \0)])

    #"^mem\[(\d+)\]\s+=\s+(\d+)$"
    :>> (fn [[_ addr v]] [:mem (Long/parseLong addr) (Long/parseLong v)])

    (throw (ex-info "bad command" {:cmd s}))))

(defn- reduce-cmd2 [{:keys [mem mask] :as ctx} cmd]
  (match cmd
    [:mask m-floats m-or] (assoc ctx :mask [m-floats m-or])

    [:mem idx v]
    (let [[m-floats m-or] mask
          m-float (peek m-floats)
          idxs (map #(-> idx (bit-and-not m-float) (bit-or % m-or)) m-floats)]
      (update ctx :mem merge (zipmap idxs (repeat v))))))

(defn two [s]
  (->> s
       str/split-lines
       (map parse-cmd2)
       (reduce reduce-cmd2 {:mem {} :mask [[0] 0]})
       :mem
       vals
       (apply +)))

(deftest t2
  (is (= 208 (two test2))))

(defn -main [& args]
  (let [input (slurp "d14.in")]
    (println "1." (one input))
    (println "2." (two input))))
