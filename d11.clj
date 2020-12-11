(ns d11
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn- pic->world [pic]
  (let [a (str/split-lines pic)
        w (count (first a))
        h (count a)
        cells (zipmap (for [y (range h) x (range w)] [x y]) (mapcat seq a))]
    {:w w :h h :cells cells}))

(defn- neighbors [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not (= 0 dx dy))] [(+ x dx) (+ y dy)]))

(defn- region-occupied [w pt]
  (count (filter #(= \# %) (map (:cells w) (neighbors pt)))))

(defn turn [{:keys [w h cells] :as world}]
  (assoc world :cells
         (into {}
               (for [x (range w)
                     y (range h)
                     :let [cur (cells [x y])
                           occ (region-occupied world [x y])]]
                 [[x y]
                  (cond
                    (and (= cur \L) (zero? occ)) \#
                    (and (= cur \#) (>= occ 4)) \L
                    :else cur)]))))

(defn one [s]
  (let [world (pic->world s)]
    (loop [world world
           i 0]
      (let [world' (turn world)]
        (if (= world' world)
          (-> world' :cells vals frequencies (get \#))
          (recur world' (inc i)))))))

(deftest t1
  (is (= (neighbors [1 1]) ))
  (is (= 37 (one test1))))

(defn- trace-dir1 [cells state start [dx dy :as dir]]
  (loop [state state
         start start
         done {}]
    (if (empty? start) [state done]
      (let [[[[x y] v] & start'] start
            p [(+ x dx) (+ y dy)]]
        (if-let [v' (cells p)]
          (let [v' (if (= \. v') v v')]
            (recur (update state p assoc dir v) start' (assoc done p v')))
          (recur state start' done))))))

(defn- trace-dir [cells state start dir]
  (loop [state state
         start start]
    (let [[state' start'] (trace-dir1 cells state start dir)]
      (if (empty? start') state
        (recur state' start')))))

(defn- trace [{:keys [w h cells] :as world}]
  (let [state (zipmap (keys cells) (repeat {}))
        start (into {} (filter (fn [[[x y] v]]
                                 (or (= x 0) (= y 0)
                                          (= x (dec w))  (= y (dec h))))
                               cells))
        dirs (neighbors [0 0])]
    (reduce #(trace-dir cells %1 start %2) state dirs)))

(defn- run1 [{:keys [w h cells] :as world}]
  (let [tr (trace world)]
    (assoc world :cells
           (into {}
                 (for [x (range w)
                       y (range h)
                       :let [cur (cells [x y])
                             occ (->> [x y] tr vals (filter #(= \# %)) count)]]
                   [[x y]
                    (cond
                      (and (= cur \L) (zero? occ)) \#
                      (and (= cur \#) (>= occ 5)) \L
                      :else cur)])))))

(defn- world->pic [{:keys [w h cells]}]
  (->> (for [y (range h) x (range w)] (get cells [x y] \space))
       (partition w)
       (map #(apply str %))
       (str/join \newline)))

(defn two [s]
  (let [world (pic->world s)]
    (loop [world world
           i 0]
      (let [world' (run1 world)]
        (if (= world' world)
          (-> world' :cells vals frequencies (get \#))
          (recur world' (inc i)))))))

(deftest t2
  (is (= (two test1) 26)))

(defn -main [& args]
  (let [input (slurp "d11.in")]
    (println "1." (one input))
    (println "2." (two input))))
