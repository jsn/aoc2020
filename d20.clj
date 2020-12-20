(ns d20
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- pixels->borders [pixels]
  [(first pixels)
   (->> pixels (map last) (apply str))
   (str/reverse (last pixels))
   (->> pixels (map first) reverse (apply str))])

(defn- parse-tile [s]
  (let [[h & pixels] (str/split-lines s)
        [_ id] (have! (re-matches #"Tile (\d+):" h))]
    {:id (Long/parseLong id) :pixels pixels :bs (pixels->borders pixels)}))

(defn- is-outer? [outer bs] (->> bs (filter outer) count (= 2)))

(defn one [fname]
  (let [tiles (map parse-tile (-> fname slurp (str/split #"\n\n")))
        bs-coll (mapcat #(concat (:bs %) (map str/reverse (:bs %))) tiles)
        outer (->> bs-coll frequencies
                   (keep (fn [[k v]] (when (= 1 v) k))) set)]
    (->> tiles (keep #(when (is-outer? outer (:bs %)) (:id %))) (apply *))))

(deftest t-1
  (is (= 20899048083289 (one "d20-t1.in"))))

(defn- flip-tile [{:keys [pixels] :as tile}]
  (let [pixels (map str/reverse pixels)]
    (assoc tile :pixels pixels :bs (pixels->borders pixels))))

(defn- rotate-tile [{:keys [pixels] :as tile}]
  (let [rp (reverse pixels)
        pixels (for [i (range (count pixels))] (apply str (map #(get % i) rp)))]
    (assoc tile :pixels pixels :bs (pixels->borders pixels))))

(deftest t-xforms
  (let [tiles (map parse-tile (-> "d20-t1.in" slurp (str/split #"\n\n")))
        tile (first tiles)]
    (is (= tile (-> tile rotate-tile rotate-tile rotate-tile rotate-tile)))
    (is (= tile (-> tile flip-tile flip-tile)))))

(defn- tile->rmap [{:keys [id bs] :as tile}]
  (zipmap (concat bs (map str/reverse bs)) (repeat {id tile})))

(defn- tile-variants [tile]
  (concat (take 4 (iterate rotate-tile tile))
          (take 4 (iterate rotate-tile (flip-tile tile)))))

(defn- fix-corner [c outer?]
  (->> c tile-variants (filter #(every? outer? (map (:bs %) [0 3]))) first))

(defn- join [tile side rmap]
  (let [coside (rem (+ 2 side) 4)
        cotile (->> (get-in tile [:bs side])
                    rmap
                    vals
                    (remove #(= (:id %) (:id tile)))
                    (have! #(= 1 (count %)))
                    first)]
    (->> cotile
         tile-variants
         (filter #(= (get-in % [:bs coside])
                     (str/reverse (get-in tile [:bs side]))))
         first)))

(defn- reconstruct [corner rmap width]
  (loop [done []
         curr [corner]]
    (cond
      (empty? curr) (if (= width (count done)) done
                      (recur done [(join (first (peek done)) 2 rmap)]))
      (= width (count curr)) (recur (conj done curr) [])
      :else (recur done (conj curr (join (peek curr) 1 rmap))))))

(defn- reconstruct-input [fname]
  (let [tiles (map parse-tile (-> fname slurp (str/split #"\n\n")))
        width (long (Math/sqrt (count tiles)))
        rmap (apply merge-with merge (map tile->rmap tiles))
        outer? (set (keep (fn [[k v]] (when (= 1 (count v)) k)) rmap))
        corners (filter #(->> % :bs (filter outer?) count (= 2)) tiles)
        c1 (fix-corner (flip-tile (first corners)) outer?)]
    (reconstruct c1 rmap width)))

(deftest t-reconstruct
  (is (= (map #(map :id %) (reconstruct-input "d20-t1.in"))
         [[1951    2311    3079]
          [2729    1427    2473]
          [2971    1489    1171]])))

(defn- tile->inner [tile]
  (->> tile :pixels rest butlast (map #(subs % 1 9))))

(defn- world->tile [world]
  (let [pixels
        (mapcat #(apply map (comp str/join vector) (map tile->inner %)) world)]
    {:id :world :pixels pixels :bs (pixels->borders pixels)}))

(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(defn- monster-re [width]
  (let [m-lines (map #(str/replace % " " ".") monster)
        m-width (count (first monster))
        sep-re (str
                 (apply str "(?s)" (repeat (inc (- width m-width)) \.))
                 "(?-s)")]
    (str/join sep-re m-lines)))

(defn- count-monsters [ws m-re]
  (loop [cnt 0
         skip 0]
    (let [re (re-pattern (str "(?s).{" skip "}(.*?)(?-s)" m-re "(?s).*"))]
      (if-let [[_ skip1 & _] (re-matches re ws)]
        (recur (inc cnt) (+ skip (count skip1) (count (monster 0))))
        (when-not (zero? cnt) cnt)))))

(defn two [fname]
  (let [world (world->tile (reconstruct-input fname))
        width (count (:pixels world))
        m-re (monster-re width)
        n-monsters (->> world
                        tile-variants
                        (map #(str/join "\n" (:pixels %)))
                        (keep #(count-monsters % m-re))
                        first)
        m-hashes (-> monster str/join frequencies (get \#))
        w-hashes (-> world :pixels str/join frequencies (get \#))]
    (- w-hashes (* n-monsters m-hashes))))

(deftest t-two
  (is (= (two "d20-t1.in") 273)))

(defn -main [& args]
  (let [input "d20.in"]
    (println "1." (one input))
    (println "2." (two input))))
