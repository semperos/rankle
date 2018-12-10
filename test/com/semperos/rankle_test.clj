(ns com.semperos.rankle-test
  (:refer-clojure :exclude [+ - * / < <= > >= count])
  (:require [clojure.test :refer :all]
            [com.semperos.rankle :refer :all]))

(def ^:private one? (partial = 1))

(deftest test-rank
  (let [table (in [2 3])]
    (is (= [[0 1 2]
            [3 4 5]]
           table))
    (is (= [0 1 2]
           (first table)))
    (is (= [0 3]
           ((rank first 1) table))
        "First of each 1-rank cell (sublist)")
    (is (= [[3 4 5]]
           (next table)))
    (is (= [[1 2]
            [4 5]]
           ((rank next 1) table))
        "Next of each 1-rank cell (sublist)")))

(deftest test->
  (is (one? (> 3 2)))
  (is (= []
         (> 4 [])))
  (is (= [0 1 0]
         (> 4 [5 2 7])))
  (is (= [0 1 1]
         (< [2 6 4] [1 8 2] [0 9 2])))
  (is (= [0 0 0]
         (> [2 6 4] [1 8 2] [7 9 2]))))

(deftest test-in
  (is (= [0 1 2 3 4]
         (in 5)))
  (is (= [[0 1 2]
          [3 4 5]]
         (in [2 3])))
  (is (= 1
         (in "abc" \b)))
  (is (= [1]
         (in "abc" "b")))
  (is (= [0 2]
         (in "abc" "ac")))
  (let [s    "abracadabra"
        idxs (in s [[\a \b] [\c \d]])]
    (is (= [[0 1]
            [4 6]]
           idxs))
    (is (= '((\a \b) (\c \d))
           (from idxs s)))))

(deftest test-from
  (is (= \b
         (from 1 "abc")))
  (is (= [\A \a]
         (from [65 97] alphabet)))
  (is (= [1 6 11]
         ((rank from 1) 1 (in [3 5])))))

(deftest test-over
  (is (= 10
         ((over +) (in 5)))))

(deftest test-prefix
  (is (= [1 3 6]
         ((prefix (over +)) [1 2 3]))))

(deftest test-unicode
  (is (= \π
         (unicode 0x03c0)))
  (is (= '(\♠ \♡ \♢ \♣)
         (unicode (+ 0x2660 (in 4))))))

(deftest test-?
  (is (every? (partial > 10) (? (range 10))))
  (is (every? (partial > 10) (? 10 10)))
  (is (= 10 (count (? 10 10)))))

(deftest test-deal-deck
  (let [s (unicode (+ 0x2660 (in 4)))
        d "A23456789TJQK"
        deck (partition-all 2 (ravel ((over (rank ravel 0)) d s)))]
    (is (= '((\A \♠) (\A \♡) (\A \♢) (\A \♣)
             (\2 \♠) (\2 \♡) (\2 \♢) (\2 \♣)
             (\3 \♠) (\3 \♡) (\3 \♢) (\3 \♣)
             (\4 \♠) (\4 \♡) (\4 \♢) (\4 \♣)
             (\5 \♠) (\5 \♡) (\5 \♢) (\5 \♣)
             (\6 \♠) (\6 \♡) (\6 \♢) (\6 \♣)
             (\7 \♠) (\7 \♡) (\7 \♢) (\7 \♣)
             (\8 \♠) (\8 \♡) (\8 \♢) (\8 \♣)
             (\9 \♠) (\9 \♡) (\9 \♢) (\9 \♣)
             (\T \♠) (\T \♡) (\T \♢) (\T \♣)
             (\J \♠) (\J \♡) (\J \♢) (\J \♣)
             (\Q \♠) (\Q \♡) (\Q \♢) (\Q \♣)
             (\K \♠) (\K \♡) (\K \♢) (\K \♣))
           deck))
    (is (every? (set deck) (from (? 5 52) deck)))))
