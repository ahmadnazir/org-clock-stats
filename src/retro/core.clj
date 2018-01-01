(ns retro.core
  (:use [clojure.string])
  (require [clj-time.core :as t])
  (require [clj-time.format :as f])
  (require [clj-time.predicates :as pr])
  )


;; utilities

(defn filename
  "Get the file name for a file"
  [file]
  (->> file
       (.toPath)
       (.getFileName)
       (str)
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

(defn minutes-to-duration
  "Minutes to a string representing the duration"
  [minutes]
  (let [h (int (/ minutes 60))
        m (mod minutes 60)
        days (if (> h 23) (int (/ h 24)))
        ]
    (if days (str days " day" (if (> days 1) "s") ", " (str (mod h 24) ":" m))
        (str h ":" m))
    )
  )

(defn minutes-to-workdays
  "Minutes to a string representing the work days"
  [minutes]
  (let [h (int (/ minutes 60))
        m (mod minutes 60)
        workdays (if (> h 6) (int (/ h 6)))
        ]
     (if workdays (str workdays " workday" (if (> workdays 1) "s") ", " (str (mod h 6) ":" m))
         (str h ":" m))
    )
  )

;; predicates
;; ----------

;; on strings

(defn date-time?
  "String to date time"
  [s]
  (try
    (to-date-time s)
    (catch Exception e nil)
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

;; Filename tests

(defn test-filename
   "Check if the file has a name that matches a journal file pattern"
   [predicate file]
   (->> file
        (filename)
        (predicate)
        )
   )

;; Get files

(defn journal-files-in-dir
  "Get all the journal files"
  [filter-fn dir]
  (->> dir
       (clojure.java.io/file)
       (.listFiles)
       (filter #(.isFile %))
       (filter (partial test-filename date-time?))
       (filter filter-fn)
       )
  )

(def dir "/home/mandark/Documents/journal")

;; (journal-files-in-dir dir (comp not nil?))

;; Total clocked minutes
;;
;; TODO The following two functions can be refactored to use multimethods
(defn find-in-file
  "Get the total clocked time for the file"
  [find-fn file]
  {:file file
   :match (try
              (find-fn (slurp file))
              (catch Exception e
                (print (str "Ignoring file: " file " since it doesn't contain the values. Error:" (.getMessage e)))))
   }
  )

(defn find-in-files
  "Run the passed function for every journal file passed"
  [find-fn files]
  (->>
   files
   (map (partial find-in-file find-fn))
   )
  )

;; Find functions
;; These functions go through the content and find specific information

(defn total-clocked-minutes
  "Total clocked time from string"
  [content]
  (let [[match duration] (re-find #"\*Total time\*.*\*(\d+:\d+)\*" content)]
    (duration-to-minutes duration)
    ))

(defn check-in-time
  "Total clocked time from string"
  [content]
  (let [[match duration] (re-find #"\*\* (\d+:\d+) " content)]
    (duration-to-minutes duration)
    ))


;; find total clocked minutes

(defn average
  "Average for a vector of numbers"
  [numbers]
  (/ (reduce + numbers) (count numbers)))


;; ---------------
;; Clocked minutes
;; ---------------

(defn total-clocked-minutes-files
  "Find total clocked minutes per journal file found in the directory"
  [files]
  (->>
   files
   (find-in-files total-clocked-minutes)
   )
  )

(defn sum-total-clocked-minutes-files
  "Sum the total clocked minutes for file passed"
  [files]
  (->>
   files
   (total-clocked-minutes-files)
   (filter (fn [x] ((comp not nil?) (:match x))))
   (map :match)
   (vec)
   (reduce +))
  )

(defn sum-total-clocked-minutes-dir
  "Sum the total clocked minutes for files present in directory"
  [filter-fn dir]
  (->>
   dir
   (journal-files-in-dir (fn [file]
                           (->> file
                                (filename)
                                (to-date-time)
                                (filter-fn)
                                )))
   (sum-total-clocked-minutes-files)
   (minutes-to-workdays))
  )

;; -------------
;; Check in time
;; -------------

(defn average-checked-in-time-files
  "Average checked in time for journal files relating to the weekdays in the directory"
  [files]
  (->> files
       (find-in-files check-in-time)
       (map :match)
       (vec)
       (filter (comp not nil?))
       (average)
       (float)
       (int)
       )
  )

(defn average-checked-in-time-dir
  "Average checked in time for journal files relating to the files in the directory"
  [filter-fn dir]
  (->>
   dir
   (journal-files-in-dir (fn [file]
                           (->> file
                                (filename)
                                (to-date-time)
                                (filter-fn)
                                )))
   (average-checked-in-time-files)
   (minutes-to-duration))
)

;; (stats "/home/mandark/Documents/journal")

(defn stats
  "Get all the stats"
  [dir]
  {
   :clocked-in-time {
                     :total (sum-total-clocked-minutes-dir (comp not nil?) dir)
                     :workdays {
                                :year (sum-total-clocked-minutes-dir pr/weekday? dir)
                                :q1 (sum-total-clocked-minutes-dir (every-pred pr/weekday? q1?) dir)
                                :q2 (sum-total-clocked-minutes-dir (every-pred pr/weekday? q2?) dir)
                                :q3 (sum-total-clocked-minutes-dir (every-pred pr/weekday? q3?) dir)
                                :q4 (sum-total-clocked-minutes-dir (every-pred pr/weekday? q4?) dir)
                                }
                     :weekends {
                                :year (sum-total-clocked-minutes-dir pr/weekend? dir)
                                :q1 (sum-total-clocked-minutes-dir (every-pred pr/weekend? q1?) dir)
                                :q2 (sum-total-clocked-minutes-dir (every-pred pr/weekend? q2?) dir)
                                :q3 (sum-total-clocked-minutes-dir (every-pred pr/weekend? q3?) dir)
                                :q4 (sum-total-clocked-minutes-dir (every-pred pr/weekend? q4?) dir)
                                }
                     }
   :check-in-time {
                   :workdays {
                              :year (average-checked-in-time-dir pr/weekday? dir)
                              :q1 (average-checked-in-time-dir (every-pred pr/weekday? q1?) dir)
                              :q2 (average-checked-in-time-dir (every-pred pr/weekday? q2?) dir)
                              :q3 (average-checked-in-time-dir (every-pred pr/weekday? q3?) dir)
                              :q4 (average-checked-in-time-dir (every-pred pr/weekday? q4?) dir)
                              }
                   :weekends {
                              :year (average-checked-in-time-dir pr/weekend? dir)
                              :q1 (average-checked-in-time-dir (every-pred pr/weekend? q1?) dir)
                              :q2 (average-checked-in-time-dir (every-pred pr/weekend? q2?) dir)
                              :q3 (average-checked-in-time-dir (every-pred pr/weekend? q3?) dir)
                              :q4 (average-checked-in-time-dir (every-pred pr/weekend? q4?) dir)
                              }
                   }
   }
  )

(def dir  "/home/mandark/Documents/journal")

(clojure.pprint/pprint
  (stats dir)
  )

(clojure.pprint/ )

(spit "/tmp/stats.txt"
      (stats dir)
      )


;; TODO: also use 'my' working days
;;
;; Working days                            : 252
;; voluntary and mandatory vacation I took : 22 + 3
;; My working days                         : 227
