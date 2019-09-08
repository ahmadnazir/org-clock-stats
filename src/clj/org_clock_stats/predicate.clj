(ns org-clock-stats.predicate
  (:require [org-clock-stats.convert :as convert]
            [clj-time.predicates :as pr]
            [clj-time.core :as t]
            )
  )

;; on strings

(defn date-time?
  "String to date time"
  [s]
  (try
    (convert/to-date-time s)
    (catch Exception e nil)
    )
  )

;; on journal files

(defn journal-file?
  "Check if the file belongs to a date time that satisfies the predicate"
  [predicate file]
  (->> file
       (.toPath)
       (.getFileName)
       (str)
       (convert/to-date-time)
       (predicate)
       )
  )

;; on date time (should be in clj-time)

(defn q1? [date-time]
  (#{1 2 3} (t/month date-time)))

(defn q2? [date-time]
  (#{4 5 6} (t/month date-time)))

(defn q3? [date-time]
  (#{7 8 9} (t/month date-time)))

(defn q4? [date-time]
  (#{10 11 12} (t/month date-time)))

