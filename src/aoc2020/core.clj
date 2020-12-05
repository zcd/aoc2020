(ns aoc2020.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as tower]
            [clojure.string :as string]))

(defn resource [name]
  (slurp (str "/Users/zcd/stuff/aoc2020/resources/"
              name)))

(defn- n-sum-filter [n sum nums]
  (filter (fn [tuple]
            (= sum (reduce + tuple)))
          (combo/combinations nums n)))

(defn- lines [input]
  (string/split input #"\n"))

(defn- lines-to-ints [input]
  (->> input
     (lines)
     (map #(Integer/parseInt %))))

(defn solution-1a [input]
  (->> input
     (lines-to-ints)
     (n-sum-filter 2 2020)
     (map #(reduce * %))
     (first)))

(defn solution-1b [input]
  (->> input
     (lines-to-ints)
     (n-sum-filter 3 2020)
     (map #(reduce * %))
     (first)))

(defstruct pair
  :left
  :right)

(defstruct pw-spec
  :letter
  :pair)

(defn- parse-pw-spec [raw-spec]
  (let [[raw-pair [letter]] (string/split raw-spec #" ")
        [left right] (string/split raw-pair #"-")
        pair (struct pair
                     (Integer/parseInt left)
                     (Integer/parseInt right))]
    (struct pw-spec
            letter
            pair)))

(defn solution-2a [input]
  (letfn [(satisfies-spec [spec password]
            (let [{letter :letter
                   {lb :left
                    ub :right} :pair} spec
                  ct (count (filter #(= letter %)
                                    password))]
              (and (<= lb ct)
                 (<= ct ub))))
          (validate-line [line]
            (let [[raw-spec password] (string/split line #": ")]
              (satisfies-spec (parse-pw-spec raw-spec)
                              password)))]
    (->> input
       (lines)
       (filter validate-line)
       (count))))

(defn solution-2b [input]
  (letfn [(xor [a b]
            (or (and a (not b))
               (and (not a) b)))
          (satisfies-spec [spec password]
            (let [{letter :letter
                   {candidate-0 :left
                    candidate-1 :right} :pair} spec
                  passlen (count password)]
              (xor (= (nth password (dec candidate-0))
                      letter)
                   (= (nth password (dec candidate-1))
                      letter))))
          (validate-line [line]
            (let [[raw-spec password] (string/split line #": ")]
              (satisfies-spec (parse-pw-spec raw-spec)
                              password)))]
    (->> input
       (lines)
       (filter validate-line)
       (count))))

(defstruct grid
  :dims ; (l x w)
  :data)

(defn- parse-grid [input]
  (let [raw (lines input)]
    (struct grid
            (struct pair
                    (count raw)
                    (if (empty? raw)
                      0
                      (count (first raw))))
            raw)))

(defn- read-grid
  [{x :left
    y :right}
   {{length :left
     width :right} :dims
    data :data}]
  (let [rectified-x (mod x width)
        rectified-y (mod y length)]
    (nth (nth data
              rectified-y)
         rectified-x)))

(defn- gen-problem-2a-path [parsed-grid]
  (let [{{length :left
          width :right} :dims
         data :data} parsed-grid]
    (map (fn [y]
           (struct pair (* 3 y) y))
         (range length))))

(defn solution-3a [input]
  (let [parsed-grid (parse-grid input)]
    (->> parsed-grid
       (gen-problem-2a-path)
       (map #(read-grid % parsed-grid))
       (filter #(= \# %))
       (count))))

(defn solution-3b [input]
  (let [parsed-grid (parse-grid input)
        {{length :left
          width :right} :dims
         data :data} parsed-grid
        slopes [(struct pair 1 1)
                (struct pair 3 1)
                (struct pair 5 1)
                (struct pair 7 1)
                (struct pair 1 2)]]
    (letfn [(gen-path-for-slope [{right :left
                                  down :right}]
              (->> (range length)
                 (map (fn [y]
                        (struct pair
                                (* right y)
                                (* down y))))
                 (filter (fn [{x :left y :right}]
                           (< y length)))))
            (count-path-trees [path]
              (->> path
                 (map #(read-grid % parsed-grid))
                 (filter #(= \# %))
                 (count)))]
      (->> slopes
         (map gen-path-for-slope)
         (map count-path-trees)
         (reduce *)))))

(defn- blocks [input]
  (string/split input #"\n\n"))

(defstruct passport-spec
  :required-fields
  :optional-fields
  :validators)

(defn- parse-kv
  [lines]
  (into {} (map #(string/split % #":") lines)))

(defn- valid-passport?
  [{req :required-fields
    opt :optional-fields
    checks :validators}
   kvs]
  (let [ks (keys kvs)]
    (and (= req
          (clojure.set/intersection req
                                    (into #{} ks)))
       (every? (fn [[k v]]
                 ((get checks k
                       (fn [anything]
                         true))
                  v))
               kvs))))

(defn solution-4a [input]
  (->> input (blocks)
     (map (fn [block]
            (string/split block #"(\n| )")))
     (map parse-kv)
     (filter (partial valid-passport?
                      (struct passport-spec
                              #{"byr"  ;(Birth Year)
                                "iyr"  ;(Issue Year)
                                "eyr"  ;(Expiration Year)
                                "hgt"  ;(Height)
                                "hcl"  ;(Hair Color)
                                "ecl"  ;(Eye Color)
                                "pid"  ;(Passport ID)
                                }
                              #{"cid"  ;(Country ID)
                                }
                              {})))
     (count)))

(defn- is-numeric? [s]
  (every? #(Character/isDigit %) s))

(defn- between? [x a b]
  (and (<= a x)
     (<= x b)))

(defn- drop-suffix [s suffix]
  (subs s 0 (- (count s) (count suffix))))

(defn solution-4b [input]
  (->> input (blocks)
     (map (fn [block]
            (string/split block #"(\n| )")))
     (map parse-kv)
     (filter (partial valid-passport?
                      (struct passport-spec
                              #{"byr"   ;(Birth Year)
                                "iyr"   ;(Issue Year)
                                "eyr"   ;(Expiration Year)
                                "hgt"   ;(Height)
                                "hcl"   ;(Hair Color)
                                "ecl"   ;(Eye Color)
                                "pid"   ;(Passport ID)
                                }
                              #{"cid"   ;(Country ID)
                                }
                              {"byr" (fn [yr]
                                       (and (is-numeric? yr)
                                          (= 4
                                             (count yr))
                                          (between? (Integer/parseInt yr)
                                                    1920 2002)))
                               "iyr" (fn [yr]
                                       (and (is-numeric? yr)
                                          (= 4
                                             (count yr))
                                          (between? (Integer/parseInt yr)
                                                    2010 2020)))
                               "eyr" (fn [yr]
                                       (and (is-numeric? yr)
                                          (= 4
                                             (count yr))
                                          (between? (Integer/parseInt yr)
                                                    2020 2030)))
                               "hgt" (fn [s]
                                       (cond (string/ends-with? s "in")
                                             (let [in-s (drop-suffix s "in")]
                                               (and (is-numeric? in-s)
                                                  (between? (Integer/parseInt in-s)
                                                            59 76)))
                                             
                                             (string/ends-with? s "cm")
                                             (let [cm-s (drop-suffix s "in")]
                                               (and (is-numeric? cm-s)
                                                  (between? (Integer/parseInt cm-s)
                                                            150 193)))
                                             :else false))
                               "hcl" (fn [hcl]
                                       (and (string/starts-with? hcl "#")
                                          (every? (fn [c]
                                                    (contains? (into #{} "0123456789abcdef")
                                                               c))
                                                  (rest hcl))))
                               "ecl" (fn [ecl]
                                       (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
                                                  ecl))
                               "pid" (fn [pid]
                                       (and (= 9 (count pid ))
                                          (is-numeric? pid)))
                               })))
     (count)))

(defn- bsearch [lb ub directions]
  (let [distance (inc (- ub lb))
        direction (first directions)]
    (cond (= distance 0) lb
          (empty? directions) lb
          (= direction :down) (bsearch lb (- ub (/ distance 2))
                                       (rest directions))
          (= direction :up) (bsearch (+ lb (/ distance 2)) ub
                                     (rest directions)))))

(defn- seat-id [raw-directions]
  (letfn [(row [directions]
            (bsearch 0 127
                     (map (fn [c]
                            (case c
                              \B :up
                              \F :down))
                          directions)))
          (col [directions]
            (bsearch 0 7
                     (map (fn [c]
                            (case c
                              \R :up
                              \L :down))
                          directions)))]
    (let [row-dirs (subs raw-directions 0 7)
          col-dirs (subs raw-directions 7)]
      (+ (col col-dirs)
         (* 8 (row row-dirs))))))

(defn solution-5a [input]
  (->> (lines input)
     (map seat-id)
     (reduce max)))

(defn solution-5b [input]
  (let [ids (->> (lines input)
               (map seat-id)
               (sort))
        [[prev __]] (filter (fn [[a b]]
                              (= (- b a) 2))
                            (map vector ids (rest ids)))]
    (inc prev)))
