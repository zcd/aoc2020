(ns aoc2020.core-test
  (:require [clojure.test :refer :all]
            [aoc2020.core :refer :all]))

;; https://adventofcode.com/2020/day/1
(def sample-01 (aoc2020.core/resource "sample-01"))
(deftest day1-test
  (testing "day 1"
    (is (= 514579
           (aoc2020.core/solution-1a sample-01)))
    (is (= 241861950
           (aoc2020.core/solution-1b sample-01)))))

;; https://adventofcode.com/2020/day/2
(def sample-02 (aoc2020.core/resource "sample-02"))
(deftest day2-test
  (testing "day 2"
    (is (= 2
           (aoc2020.core/solution-2a sample-02)))
    (is (= 1
           (aoc2020.core/solution-2b sample-02)))))

;; https://adventofcode.com/2020/day/3
(def sample-03 (aoc2020.core/resource "sample-03"))
(deftest day3-test
  (testing "day 3"
    (is (= 7
           (aoc2020.core/solution-3a sample-03)))
    (is (= 336
           (aoc2020.core/solution-3b sample-03)))))

;; https://adventofcode.com/2020/day/4
(def sample-04 (aoc2020.core/resource "sample-04"))
(deftest day4-test
  (testing "day 4"
    (is (= 2
           (aoc2020.core/solution-4a sample-04)))
    (is (= 4
           (aoc2020.core/solution-4b (aoc2020.core/resource "sample-04-valid"))))
    (is (= 0
           (aoc2020.core/solution-4b (aoc2020.core/resource "sample-04-invalid"))))))
