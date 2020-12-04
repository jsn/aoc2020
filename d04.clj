(ns d04
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]
            [util])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1-s
"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defn- parse-one [s]
  (into {} (map #(str/split % #":") (str/split s #"\s+"))))

(defn- parse [s] (map parse-one (str/split s #"\n\n")))

(defn- valid1? [rec]
  (set/subset? fields (set (keys rec))))

(defn- run-one [s] (->> s parse (filter valid1?) count))

(defn one [] (-> "d04.in" slurp run-one))

(defn- check-pair [[k v]]
  (condp = k
    "byr" (when-let [y (Integer/parseInt (re-matches #"^\d{4}$" v))]
            (<= 1920 y 2002))
    "iyr" (when-let [y (Integer/parseInt (re-matches #"^\d{4}$" v))]
            (<= 2010 y 2020))
    "eyr" (when-let [y (Integer/parseInt (re-matches #"^\d{4}$" v))]
            (<= 2020 y 2030))
    "hgt" (when-let [[_ x u] (re-matches #"^(\d+)(cm|in)$" v)]
            (let [[l h] ({"cm" [150 193] "in" [59 76]} u)]
              (<= l (Integer/parseInt x) h)))
    "hcl" (re-matches #"^#[0-9a-f]{6}$" v)
    "ecl" (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v)
    "pid" (re-matches #"^\d{9}$" v)
    true))

(defn- valid2? [rec]
  (and (valid1? rec)
       (every? check-pair (select-keys rec fields))))

(defn- run-two [s] (->> s parse (filter valid2?) count))

(def test2-s1
"eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def test2-s2
"pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(defn two [] (-> "d04.in" slurp run-two))

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 4 (count (parse test1-s)))))
  (testing 1
    (is (= 2 (run-one test1-s))))
  (testing 2
    (is (= 0 (run-two test2-s1)))
    (is (= 4 (run-two test2-s2)))))

