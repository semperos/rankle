(ns com.semperos.rankle.util-test
  (:require [clojure.test :refer :all]
            [com.semperos.rankle.util :as util]))

(def ^:dynamic *processed*)
(def ^:dynamic *iteration*)

(deftest test-cond-table
  (testing "Default cond-table `and`"
    (let [success?      (fn [] (= *processed* ::success))
          failure?      (fn [] (= *processed* ::failure))
          final?        (fn [] (= *iteration* ::final))
          intermediate? (fn [] (= *iteration* ::retry))
          ok            (constantly ::ok)
          log           (constantly ::log)
          test          (fn [[p i] expected]
                          (binding [*processed* p *iteration* i]
                            (is (= expected
                                   (util/cond-table
                                    :| __________   (final?)  (intermediate?)
                                    :| (success?)     (ok)      ::not-bad
                                    :| (failure?)      -1         (log)))
                                (str "If processed is " p
                                     " and iteration is " i
                                     " then the result should be " expected))))]
      (test [::failure ::retry] ::log)
      (test [::failure ::final] -1)
      (test [::success ::retry] ::not-bad)
      (test [::success ::final] ::ok)))
  (testing "Custom op, xor function table"
    (let [xor (fn [x y]
                (let [x⊻y (fn [& args] (= [x y] args))]
                  (util/cond-table
                   :| x⊻y  0  1
                   :|   0  0  1
                   :|   1  1  0)))
          test (fn [[x y] expected]
                 (let [xor-result (xor x y)
                       bit-xor-result (bit-xor x y)]
                   (is (= expected xor-result bit-xor-result)
                       (str "Expected " expected
                            " but xor gave " xor-result
                            " and bit-xor gave " bit-xor-result))))]
      (test [0 0] 0)
      (test [1 1] 0)
      (test [0 1] 1)
      (test [1 0] 1))))

(deftest test-validate-cond-table
  (is (thrown-with-msg?
       IllegalArgumentException
       #"Each row in cond-table must begin with the keyword :\|"
       (util/validate-cond-table
        [      :a :b
         :| :c :d :e])))
  (is (thrown-with-msg?
       IllegalArgumentException
       #"Every row after the first in cond-table must start with a predicate and include an expression for each cell in the table."
       (util/validate-cond-table
        [:|    :a :b
         :| :c :d :e
         :|    :g :h])))
  (is (thrown-with-msg?
       IllegalArgumentException
       #"Every row after the first in cond-table must start with a predicate and include an expression for each cell in the table."
       (util/validate-cond-table
        [:|    :a :b
         :| :c    :e
         :| :f :g :h]))))
