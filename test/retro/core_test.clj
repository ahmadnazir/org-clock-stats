(ns retro.core-test
  (:require [clojure.test :refer :all]
            [retro.core :refer :all]
            [clj-time.core :as t]
            ))

(deftest test-to-date-time
  (testing "String to date time conversion test"
    (is (= (to-date-time "20170112")
           (t/date-time 2017 1 12)
           ))))

(deftest test-total-clocked-minutes
  (testing "Get the total clocked minutes from a journal file containing a log book"
    (is (=
         (total-clocked-minutes ".. *Total time* ... *43:23*")
         2603
         ))))

(deftest test-duration-to-minutes
  (testing "Convert a string representing a duration into minutes"
    (is (=
         (duration-to-minutes "01:13")
         73
         ))))

