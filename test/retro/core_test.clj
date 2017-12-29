(ns retro.core-test
  (:require [clojure.test :refer :all]
            [retro.core :refer :all]
            [clj-time.core :as t]
            ))

(deftest test-to-date-time
  (testing "String to date time conversion test"
    (is (= (toDateTime "20170112")
           (t/date-time 2017 1 12)
           ))))

(deftest test-to-date-duration-map
  (testing "String to Date Duration Map test"
    (is (=
           (to-date-duration-map "20170112:01:13")
           {:date (t/date-time 2017 1 12), :duration 73}
           ))))

(deftest test-to-date-duration-maps
  (testing "String to Date Duration Map test"
    (is (=
         (to-date-duration-maps "20170112:01:13")
         [{:date (t/date-time 2017 1 12), :duration 73}]
         ))))


(deftest test-duration-to-minutes
  (testing "Convert a string representing a duration into minutes"
    (is (=
         (duration-to-minutes "01:13")
         73
         ))))

