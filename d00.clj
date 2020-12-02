(ns d00
  (:require [clojure.test :refer :all]
            [util])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn one []
  "not implemented.")

(defn two []
  "not implemented")

(defn -main [& args]
  (println "1." (one))
  (println "2." (two)))

(deftest everything
  (testing "read-input"
    (is (= 100 100))))
