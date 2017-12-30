(ns retro.core
  (:use [clojure.string])
  (require [clj-time.core :as t])
  (require [clj-time.format :as f])
  (require [clj-time.predicates :as pr])
  )


;; predicates

(defn date-time?
  "String to date time"
  [s]
  (try
    (to-date-time s)
    (catch Exception e nil)
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
   (let [duration (split s #":")
         hours (Integer. (first duration))
         minutes (Integer. (second duration))]
     (+ (* hours 60) minutes)
     )
  )

;; Filename tests

(defn test-filename
   "Check if the file has a name that matches a journal file pattern"
   [predicate file]
   (->> file
        (.toPath)
        (.getFileName)
        (str)
        (predicate)
        )
   )

;; Get files

(defn journal-files-in-dir
  "Get all the journal files"
  [dir]
  (->> dir
       (clojure.java.io/file)
       (.listFiles)
       (filter #(.isFile %))
       (filter (partial test-filename date-time?))
       )
  )

;; Total clocked minutes

(defn total-clocked-minutes
  "Total clocked time from string"
  [content]
  (let [[match duration] (re-find #"\*Total time\*.*\*(\d+:\d+)\*" content)]
    (duration-to-minutes duration)
    ))

(defn total-clocked-minutes-file
  "Get the total clocked time for the file"
  [file]
  {:file file
   :minutes (try
              (total-clocked-minutes (slurp file))
              (catch Exception e
                0))}
  )

(defn total-clocked-minutes-dir
  "Calculate the total time clocked in from logbook in the journal files"
  [dir]
  (->>
   dir
   (journal-files-in-dir)
   (map total-clocked-minutes-file)
   (map :minutes)
   (vec)
   (reduce +)
   )
  )


;; Testing...

;; (journal-files-in-dir "/home/mandark/Documents/journal" )
;; (total-clocked-minutes-dir "/home/mandark/Documents/journal" )


;; Old tests

;; (->>
;;  "/home/mandark/Documents/journal"
;;  (journal-files-in-dir)
;;  (last)
;;  (total-clocked-minutes-file)
;;  )

;; Check the journal filenames

;; (defn is-journal
;;   "Check if string might be a journal file name"
;;   [filename]
;;   (to-date-time filename)
;;   )

;; (defn is-weekday
;;   "Check if the file corresponds to a weekday"
;;   [filename]
;;   (->> filename
;;        (to-date-time)
;;        (pr/weekday?)
;;        )
;;   )
