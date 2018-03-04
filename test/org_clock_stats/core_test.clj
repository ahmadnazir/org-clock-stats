(ns org-clock-stats.core-test
  (:require [clojure.test :refer :all]
            [org-clock-stats.core :refer :all]
            [clj-time.core :as t]
            ))

(deftest test-find-work-time-support
  (testing "Get the total clocked minutes using a title in a log book"
    (let [content " | \\_  09:41 Support                        |        | 1:17 | "]
      (is (=
           (find-work-time :title-starts-with "support" content)
           77
           )))
    ))

(deftest test-find-work-time-not-found
  (testing "Get the total clocked minutes from a journal file containing a log book"
    (let [content " | \\_  09:41 Support                         |        | 1:17 | "]
      (is (thrown? NullPointerException
           (find-work-time :title-starts-with "not-found" content)
           )))
    ))

(deftest test-find-work-time-title-contains-1
  (testing "Get the total clocked minutes using a title in a log book"
    (let [content " | \\_  09:41 Working on my XYZ project       |        | 1:18 | "]
      (is (=
           (find-work-time :title-contains "XYZ" content)
           78
           )))
    ))

(deftest test-find-work-time-title-contains-2
  (testing "Get the total clocked minutes using a title in a log book"
    (let [content " | \\_  12:44 org-clock-stats |        | 1:58 | "]
      (is (=
           (find-work-time :title-contains "clock" content)
           118
           )))
    ))

