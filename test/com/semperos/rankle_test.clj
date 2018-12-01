(ns com.semperos.rankle-test
  (:refer-clojure :exclude [+ - * / count])
  (:require [clojure.test :refer :all]
            [com.semperos.rankle :refer :all]))

(deftest rank-test
  (is (= ##Inf (rank #'count))))

(deftest test-table-and-column-map
  (let [data (mapv (fn [idx]
                     {:foo    (* (inc idx) 15)
                      :barcel (nth ["alpha" "beta" "gamma" "delta" "epsilon"] idx)
                      :baz    (mapv #(* (inc idx) 2 %) [1 2 3])})
                   (range 5))
        cm   (to-column-map data)
        t    (to-table cm)]
    (is (= t (flip cm)))
    (is (= cm (flip t)))
    (testing "Single row or column selector"
      (is (= [15 30 45 60 75]
             (t [nil :foo])))
      (is (= [15 30 45 60 75]
             (cm [:foo nil])))
      (is (= (column-map [:foo :barcel :baz] [[15] ["alpha"] [[2 4 6]]])
             (t [0 nil])))
      (is (= (column-map [:foo :barcel :baz] [[15] ["alpha"] [[2 4 6]]])
             (cm [nil 0]))))
    (testing "Row and column selectors"
      (is (= 15
             (t [0 :foo])))
      (is (= 60
             (t [3 :foo])))
      (is (= 15
             (cm [:foo 0])))
      (is (= 60
             (cm [:foo 3])))
      (testing "with nested tuples"
        (testing "of one type"
          (is (= (column-map [:foo     :barcel            :baz]
                             [[15 45]  ["alpha" "gamma"]  [[2 4 6] [6 12 18]]])
                 (t [[0 2] nil])))
          (is (= (column-map {:foo    [15 45]
                              :barcel ["alpha" "gamma"]
                              :baz    [[2 4 6] [6 12 18]]})
                 (t [[0 2] nil])))
          (is (= (column-map {:foo [30 60]
                              :barcel ["beta" "delta"]
                              :baz [[4 8 12] [8 16 24]]})
                 (t [[1 3] nil])))
          (is (= [[15 30 45 60 75] ["alpha" "beta" "gamma" "delta" "epsilon"]]
                 (t [nil [:foo :barcel]])))
          (is (= (column-map {:foo [15 60]
                              :barcel ["alpha" "delta"]
                              :baz [[2 4 6] [8 16 24]]})
                 (cm [nil [0 3]])))
          (is (= (column-map {:foo [30 45]
                              :barcel ["beta" "gamma"]
                              :baz [[4 8 12] [6 12 18]]})
                 (cm [nil [1 2]])))
          (is (= [[15 30 45 60 75]
                  [[2 4 6] [4 8 12] [6 12 18] [8 16 24] [10 20 30]]]
                 (cm [[:foo :baz] nil])))
          (testing "with non-nested other"
            (is (= (column-map [:foo]
                               [[15 45]])
                   (t [[0 2] :foo])))
            (is (= [45 "gamma"]
                   (cm [[:foo :barcel] 2]))))
          (testing "with nested other"
            (is (= (column-map [:foo :barcel]
                               [[15 45] ["alpha" "gamma"]])
                   (t [[0 2] [:foo :barcel]])))
            (is (= '((15 "alpha") (60 "delta"))
                   (cm [[:foo :barcel] [0 3]])))))))))

(deftest ranked-count-test
  (testing "data rank 2"
    (is (= 3
           ((rank #'count 2) [[0 1 2 3]
                                 [4 5 6 7]
                                 [8 9 10 11]])))
    (is (= [4 4 4]
           ((rank #'count 1) [[0 1 2 3]
                                 [4 5 6 7]
                                 [8 9 10 11]])))
    (is (= [[1 1 1 1]
            [1 1 1 1]
            [1 1 1 1]]
           ((rank #'count 0) [[0 1 2 3]
                                 [4 5 6 7]
                                 [8 9 10 11]]))))
  (testing "data rank 3"
    (is (= 2
           ((rank #'count 3) [[[0 1 2 3]
                                  [4 5 6 7]
                                  [8 9 10 11]]
                                 [[12 13 14 15]
                                  [16 17 18 19]
                                  [20 21 22 23]]])))
    (is (= [3 3]
           ((rank #'count 2) [[[0 1 2 3]
                                  [4 5 6 7]
                                  [8 9 10 11]]
                                 [[12 13 14 15]
                                  [16 17 18 19]
                                  [20 21 22 23]]])))
    (is (= [[4 4 4]
            [4 4 4]]
           ((rank #'count 1) [[[0 1 2 3]
                                  [4 5 6 7]
                                  [8 9 10 11]]
                                 [[12 13 14 15]
                                  [16 17 18 19]
                                  [20 21 22 23]]])))
    ;; TODO
    #_(is (= [[[1 1 1 1]
             [1 1 1 1]
             [1 1 1 1]
             [1 1 1 1]]
            [[1 1 1 1]
             [1 1 1 1]
             [1 1 1 1]
             [1 1 1 1]]]
           ((rank #'count 0) [[[0 1 2 3]
                                  [4 5 6 7]
                                  [8 9 10 11]]
                                 [[12 13 14 15]
                                  [16 17 18 19]
                                  [20 21 22 23]]])))))
