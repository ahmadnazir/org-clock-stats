(ns org-clock-stats.core-test
  (:require [clojure.test :refer :all]
            [org-clock-stats.core :refer :all]
            [clj-time.core :as t]
            ))

(deftest test-to-date-time
  (testing "String to date time conversion test"
    (is (= (to-date-time "20170112")
           (t/date-time 2017 1 12)
           ))))

;; (deftest test-total-clocked-minutes
;;   (testing "Get the total clocked minutes from a journal file containing a log book"
;;     (is (=
;;          (sum-total-clocked-minutes ".. *Total time* ... *43:23*")
;;          2603
;;          ))))

;; (deftest test-check-in-time
;;   (testing "Get the total clocked minutes from a journal file containing a log book"
;;     (is (=
;;          (check-in-time "** 09:23 retro")
;;          563
;;          ))))

(deftest test-find-support-time
  (testing "Get the total clocked minutes from a journal file containing a log book"
    (is (=
         (find-support-time " | \\_  09:41 Support                         |        | 1:17 | ")
         77
         ))))

(deftest test-duration-to-minutes
  (testing "Convert a string representing a duration into minutes"
    (is (=
         (duration-to-minutes "01:13")
         73
         ))))

(deftest test-vacation-days
  (testing "Convert a string representing a duration into minutes"
    (is (=
         (vacation-days {
                         :single #{ "20170101" "20170102"}
                         :range #{
                                  { :start "20170227" :end "20170301"}
                                  { :start "20170327" :end "20170401"}
                                  }
                         })
         (set (map to-date-time ["20170101", "20170102", "20170227", "20170228", "20170301", "20170327", "20170328", "20170329", "20170330", "20170331", "20170401"])
          )
         ))))
