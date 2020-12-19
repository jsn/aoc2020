(ns d19
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.truss :refer [have!]]
            [util :as u :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def test1
"0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")

(def test1-1
"0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\"

aabb")

(defn- parse-rule [s]
  (let [[id v] (str/split s #":\s+")
        id (Long/parseLong id)]
    (if-let [[_ sv] (re-matches #"\"([a-z]+)\"" v)]
      [id sv]
      (let [disjs (str/split v #"\s+\|\s+")]
        [id
         (map
           (fn [x] (mapv #(Long/parseLong %) (str/split x #"\s+")))
           disjs)]))))

(defn- parse [s]
  (let [[rules msgs] (str/split s #"\n\n")]
    [(->> rules str/split-lines (map parse-rule) (into {}))
     (str/split-lines msgs)]))

(defn- rule->regexp [rules id]
  (let [r (have! (rules id))]
    (if (string? r) r
      (let [disjs (map
                    (fn [ids]
                      (str
                        "(?:"
                        (str/join (mapv #(rule->regexp rules %) ids))
                        \))) r)]
        (str "(?:" (str/join "|" disjs) \) )))))

(defn one [s]
  (let [[rules msgs] (parse s)
        rule (re-pattern (rule->regexp rules 0))]
    (->> msgs (filter #(re-matches rule %)) count)))

(deftest t-1
  (is (= (one test1-1)))
  (is (= 2 (one test1))))

(def test2
"42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(defn- count-patterns [r s] (count (re-seq (re-pattern r) s)))

(defn two [s]
  (let [[rules msgs] (parse s)

        _ (have! (= '([8 11]) (rules 0)))

        r42 (rule->regexp rules 42)
        r31 (rule->regexp rules 31)

        r0 (re-pattern (str "^(" r42 "+)(" r31 "+)$"))

        f #(when-let [[_ s42 s31] (re-matches r0 %)]
             (> (count-patterns r42 s42) (count-patterns r31 s31)))]
    (->> msgs (filter f) count)))

(deftest t-2
  (is (= 3 (one test2)))
  (is (= 12 (two test2))))

(defn -main [& args]
  (let [input (slurp "d19.in")]
    (println "1." (one input))
    (println "2." (two input))))
