(ns retro.core
  (:use [clojure.string])
  (require [clj-time.core :as t])
  )

(defn to-date-time
  "String to date time"
  [s]
  (t/date-time (Integer. (subs s 0 4)) (Integer. (subs s 4 6)) (Integer. (subs s 6 8)))
  )

(defn duration-to-minutes
  "String in the format hh:mm to minutes conversion"
  [s]
   (let [duration (split s #":")
         hours (Integer. (first duration))
         minutes (Integer. (second duration))]
     (+ (* hours 60) minutes)
     )
  )


(defn to-date-duration-map
  "Converts a string to a date duration map"
  [s]
  {:date (to-date-time (subs s 0 8))
   :duration (duration-to-minutes (subs s 9))})

(defn to-date-duration-maps
  "Converts a string to a date duration map"
  [s]
  (->>
   (split s #"\n")
   (filter #(not= %1 ""))
   (map (partial to-date-duration-map))
   (vec)
   ))

(defn total-time
  "Calculate the total time for the date duration maps"
  [collection]
  (->>
   (map :duration collection)
   (reduce +)
   )
  )

;; (defn format-minutes
;;   "Convert the minutes to a readable format"
;;   [mins]
;;   "test"
;;   )

;; (format-minutes "123")
;; (total-time (to-date-duration-maps collection))

;; (foo "test")
;; (def collection "20170111:3:37
;;   20170112:3:12
;;   20170113:3:06
;;   20170117:6:20
;;   ")
;; (def element "20170112:3:12")

;; (->>
;;  (split collection #"\n")
;;  (filter #(not= %1 ""))
;;  (map (partial to-date-duration-map))
;;  (map :duration)
;;  (reduce +)
;;  )

