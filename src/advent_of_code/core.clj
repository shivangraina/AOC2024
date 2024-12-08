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





(re-seq #"(?s)don't\(\)(.+?)(?:do\(\)|$)" "don't() 12 33 \n")


(re-seq #".ar" "ar")


(comment (apply map vector  [[ 1 2 3 4 ]
                     [ 5 6 7 8 ]
                     [ 9 10 11 12]]))





(def m [[1 5 9]
        [2 6 10]
        [3 7 11]
        [4 8 12]])


(let [r (count m)
      c (count (first m))
      total-d (dec (+ r c))]

  (map #(loop [row %
          col 0
          d []
          res []]

     (if (or (< col 0)
             (> col (dec c))
             (> row (dec r))
             (< row 0))
       (conj res d)
       (let [value (nth (nth m row ) col)]
         (recur (inc row) (inc col) (conj d value) res)))) (range total-d)))







(def aa (read-input 4))

(def mm (map vec (str/split-lines aa)))

(defn extract-right-diagonal [m row]
  (loop [row row, col 0, out []]
    (if (< row 0)
      (filter #(not (false? %)) out)
      (let [value (nth (nth m row []) col false)]
        (recur (dec row) (inc col) (conj out value))))))



(defn extract-left-diagonal [rows row]
  (loop [row row, col (dec (count (first rows))), out []]
    (if (< col 0)
      (filter #(not (false? %)) out)
      (let [value (nth (nth rows row []) col false)]
        (recur (dec row) (dec col) (conj out value))))))


(def d (let [r (count mm)
       c (count (first mm))
       total-d (dec (+ r c))
             elements (map #(str/join "" %)
                           (concat (filter #(<= 4 (count %))(map #(extract-right-diagonal mm %) (range total-d)))
                                   (filter #(<= 4 (count %))(map #(extract-left-diagonal mm %) (range total-d)))))
             _ (def ee elements)]
         (+ (count (remove nil? (mapcat #(re-seq #"XMAS" %) elements)))
            (count (remove nil? (mapcat #(re-seq #"SAMX" %) elements))))))



(def h (+ (count  (mapcat #(re-seq #"SAMX" %)  (str/split-lines aa)))
    (count (mapcat #(re-seq #"XMAS" %)  (str/split-lines aa)))))



(def v (+ (count (mapcat #(re-seq #"SAMX" %) (map #(str/join "" %)(apply map vector mm))))
    (count (mapcat #(re-seq #"XMAS" %) (map #(str/join "" %)(apply map vector mm))))))


(+ d h v)


