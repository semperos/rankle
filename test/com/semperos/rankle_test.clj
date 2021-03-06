(ns com.semperos.rankle-test
  (:refer-clojure :exclude [= + - * / < <= > >= count not not= drop take])
  (:require [clojure.test :refer :all]
            [com.semperos.rankle :refer :all]))

;; TODO Test helpers that rely on Iverson 0/1 booleans

(deftest test-rank
  (let [table (in [2 3])]
    (is (c= [[0 1 2]
            [3 4 5]]
           table))
    (is (c= [0 1 2]
           (first table)))
    (is (c= [0 3]
           ((rank first 1) table))
        "First of each 1-rank cell (sublist)")
    (is (c= [[3 4 5]]
           (next table)))
    (is (c= [[1 2]
            [4 5]]
           ((rank next 1) table))
        "Next of each 1-rank cell (sublist)")))

(deftest test-fork
  ;; Cases from examples on http://jsoftware.com/help/learning/09.htm
  (let [mean (fork / (over +) count)]
    (is (c= 50
           (mean (in 101)))))
  (let [give-or-take (fork ravel + -)]
    (is (c= [12 8]
           (give-or-take 10 2)))))

(deftest test->
  (is (one? (> 3 2)))
  (is (c= []
         (> 4 [])))
  (is (c= [0 1 0]
         (> 4 [5 2 7])))
  (is (c= [0 1 1]
         (< [2 6 4] [1 8 2] [0 9 2])))
  (is (c= [0 0 0]
         (> [2 6 4] [1 8 2] [7 9 2]))))

(deftest test-in
  (is (c= [0 1 2 3 4]
         (in 5)))
  (is (c= [[0 1 2]
          [3 4 5]]
         (in [2 3])))
  (is (c= 1
         (in "abc" \b)))
  (is (c= [1]
         (in "abc" "b")))
  (is (c= [0 2]
          (in "abc" "ac")))
  ;; TODO Unbreak this
  (let [s    "abracadabra"
        idxs (in s [[\a \b] [\c \d]])]
    (is (c= [[0 1]
            [4 6]]
           idxs))
    #_(is (c= '((\a \b) (\c \d))
           (from idxs s)))))

(deftest test-indices
  (is (c= [2 4 5]
         (indices [0 0 1 0 1 1]))))

(deftest test-intervals
  (is (c= ["ab" "cde"]
         (intervals [1 0 1 0 0] "abcde"))))

(deftest test-from
  (is (c= \b
         (from 1 "abc")))
  (is (c= [\A \a]
         (from [65 97] alphabet)))
  (is (c= [1 6 11]
         ((rank from 1) 1 (in [3 5])))))

(deftest test-not=
  (is (c= [1 1 1 0 1]
          (not= 3 (in 5))))
  (is (c= [0 0 0 0 0]
          ((reflex not=) (in 5))))
  (is (c= [1 1 0 1 1]
            (not= (in 5) (rot (in 5))))))

(deftest test-nub-sieve
  (is (c= [1 1 1 0 1 0 1 0 0]
         (nub-sieve [1 2 3 2 4 3 5 2 1]))))

(deftest test-over
  (is (c= 10
         ((over +) (in 5))))
  (is (c= 11
         ((over +) :init 1 (in 5)))
      "The function returned from `over` can take :init + init value.")
  (is (c= 12
         ((over + 2) (in 5)))
      "The `over` function accepts an init value for the reduction."))

(deftest test-prefix
  ;; TODO Consider fill/ragged
  (is (c= '((1) (1 2) (1 2 3))
         ((prefix identity) [1 2 3])))
  (is (c= [1 3 6]
         ((prefix (over +)) [1 2 3]))))

(deftest test-rot
  (is (c= [2 1 0] (rot (in 3))))
  (let [coll '(a b c d)]
    (are [n result] (c= result (rot n coll))
      0 coll
      1 '(b c d a)
      2 '(c d a b)
      3 '(d a b c)
      4 '(a b c d)
      -1 '(d a b c)
      -2 '(c d a b)
      -3 '(b c d a)
      -4 '(a b c d))))

(deftest test-tally
  (is (c= 3 (tally (in 3))))
  (is (c= 2 (tally (in [2 3]))))
  (is (c= [3 3] ((rank tally 1) (in [2 3]))))
  (is (c= [[4 4 4] [4 4 4]] ((rank tally 1) (in [2 3 4])))))

(deftest test-unicode
  (is (c= \π
         (unicode 0x03c0)))
  (is (c= '(\♠ \♡ \♢ \♣)
         (unicode (+ 0x2660 (in 4))))))

(deftest test-=
  (is (one? (= 3 3)))
  (is (zero? (= 3 4)))
  (is (c= [1 0 1]
          (= 3 [3 4 3])))
  (is (c= [[0 1 0]
           [1 0 1]]
          (= 3 [[4 3 4]
                [3 4 3]])))
  (is (c= [1 0 1]
          (= [5 4 5] [5 6 5]))))

(deftest test-?
  (is (every? (partial > 10) (? (range 10))))
  (is (every? (partial > 10) (? 10 10)))
  (is (c= 10 (tally (? 10 10)))))

(deftest test-deal-deck
  (let [s (unicode (+ 0x2660 (in 4)))
        d "A23456789TJQK"
        deck (partition-all 2 (ravel ((over (rank ravel 0)) d s)))]
    (is (c= '((\A \♠) (\A \♡) (\A \♢) (\A \♣)
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
