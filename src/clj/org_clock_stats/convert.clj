(ns org-clock-stats.convert
  (:require
   [clojure.string :as string]
   [clj-time.format :as f]
   )
  )


;; conversion from strings

(defn to-date-time
  "String to date time"
  [s]
  (f/parse (f/formatter "yyyyMMdd") s)
  )

(defn duration-to-minutes
  "String in the format hh:mm to minutes conversion"
  [s]
  (let [duration (string/split s #":")
        hours (Integer. (first duration))
        minutes (Integer. (second duration))]
    (+ (* hours 60) minutes)
    )
  )

(defn minutes-to-hours
  "Minutes to a string representing the work days"
  [minutes]
  (int (/ minutes 60))
  )

(defn hours-to-work-days
  [hours]
  (->> 5
       (/ hours)
       Math/ceil
       int
       ))

(defn minutes-to-work-days
  [mintes]
  (->> mintes
       minutes-to-hours
       hours-to-work-days
       ))
