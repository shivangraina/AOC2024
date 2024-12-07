(ns advent-of-code.core
  (:gen-class)
  (:require [clojure.string :as str]))

;; DAY-1 and Day-2

(def ^:private input-dir "/Users/shivang.raina/workspace/advent_of_code/resources/input/")

(defn- read-input
  "Read input file for a given problem ID"
  [prob-id]
  (slurp (str input-dir prob-id ".txt")))

(defn- parse-input
  "Parse input string into a sequence of integer lists"
  [input-str]
  (->> input-str
       (str/split-lines)
       (map #(->> (str/split % #"\s+")
                  (map (comp int str/trim))))))

(defn- is-row-safe?
  "Check if a row is safe based on specific constraints"
  [row]
  (let [pairs (partition 2 1 row)]
    (and (or (every? #(apply < %) pairs)
             (every? #(apply > %) pairs))
         (every? #(< (abs (apply -  %)) 3) pairs))))

(defn- check-combinations
  "Check if any row combination is safe"
  [row]
  (->> (range (count row))
       (map #(concat (subvec (vec row) 0 %)
                     (subvec (vec row) (inc %))))
       (some is-row-safe?)))

(defn problem-1-a
  []
  (let [input (parse-input (read-input "1"))
        [keys vals] (mapv #(sort (keys (frequencies %))) input)]
    (reduce + (map #(abs (- %1 %2)) keys vals))))

(defn problem-1-b
  []
  (let [input (parse-input (read-input "1"))
        [keys vals] (mapv #(sort (keys (frequencies %))) input)]
    (+ (reduce + (map #(abs (- %1 %2)) keys vals))
       (apply + (map * keys (frequencies vals))))))

(defn problem-2-a
  []
  (->> (read-input "2")
       parse-input
       (filter is-row-safe?)
       count))

(defn problem-2-b
  []
  (->> (read-input "2")
       parse-input
       (filter check-combinations)
       count))

(defn calculate-sum
  [in]
  (apply + (map (fn [s]
                  (let [[a b] (re-seq #"\d+" s)]
                    (*  (Integer/parseInt a) (Integer/parseInt b))))
                (re-seq #"mul\(\d+,\d+\)" in))))

(defn problem-3-a
  []
  (calculate-sum (read-input 3)))

(defn problem-3-b
  []
  (let [in (read-input 3)
        whole-sum (calculate-sum in)
        corrupted-list  (map second (re-seq #"(?s)don't\(\)(.+?)(?:do\(\)|$)" in))
        corrupted-list-sum (apply + (map calculate-sum corrupted-list))]
    (- whole-sum corrupted-list-sum)))










