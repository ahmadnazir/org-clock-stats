(ns org-clock-stats.test.convert
  (:require [clojure.test :refer :all]
            [org-clock-stats.convert :as convert]
            [clj-time.core :as t]
            ))

(deftest test-to-date-time
  (testing "String to date time conversion test"
    (is (= (convert/to-date-time "20170112")
           (t/date-time 2017 1 12)
           ))))
