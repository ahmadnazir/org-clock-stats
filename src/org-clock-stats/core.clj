(ns org-clock-stats.core
  (:use [clojure.string])
  (require [clj-time.core :as t])
  (require [clj-time.format :as f])
  (require [clj-time.predicates :as pr])
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

;; on journal files

(defn journal-file-predicate
  "Check if the file belongs to a date time that satisfies the predicate"
  [predicate file]
  (->> file
       (.toPath)
       (.getFileName)
       (str)
       (to-date-time)
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

;; Filename tests


;; Get files
;;
;; TODO refactor the following so that 3 separate functions aren't needed

(defn filename
  "Get the file name for a file"
  [file]
  (->> file
       (.toPath)
       (.getFileName)
       (str)
       )
  )
(defn test-filename
  "Check if the file has a name that matches a journal file pattern"
  [predicate file]
  (->> file
       (filename)
       (predicate)
       )
  )
(defn journal-files
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

;; (journal-files dir (comp not nil?))

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
                (print (str "\nIgnoring file: " file " since it doesn't contain the values. Error:" (.getMessage e)))))
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

(defn find-total-clocked-minutes
  "Total clocked time from string"
  [content]
  (let [[match duration] (re-find #"\*Total time\*.*\*(\d+:\d+)\*" content)]
    (duration-to-minutes duration)
    ))

(defn find-check-in-time
  "Total clocked time from string"
  [content]
  (let [[match duration] (re-find #"\*\* (\d+:\d+) " content)]
    (duration-to-minutes duration)
    ))

(defn find-support-time
  "Total clocked time from string"
  [content]
  (let [[match start duration] (re-find #" ..  (\d+:\d+) [Ss]upport.*(\d+:\d+)" content)]
    (duration-to-minutes duration)
    ))


;; ---------------
;; Clocked minutes
;; ---------------

(defn total-clocked-minutes
  "Find total clocked minutes per journal file"
  [files]
  (->>
   files
   (find-in-files find-total-clocked-minutes)
   )
  )
(defn sum-total-clocked-minutes
  "Sum the total clocked minutes for files passed"
  [files]
  (->>
   files
   (total-clocked-minutes)
   (filter (fn [x] ((comp not nil?) (:match x))))
   (map :match)
   (vec)
   (reduce +)
   (minutes-to-workdays)
   )
  )

;; ------------
;; Support time
;; ------------

(defn support-time
  "Find the duration spend on support per journal file"
  [files]
  (->>
   files
   (find-in-files find-support-time)
   )
  )
(defn sum-support-time
  "Sum the total clocked minutes for files passed"
  [files]
  (->>
   files
   (support-time)
   (filter (fn [x] ((comp not nil?) (:match x))))
   (map :match)
   (vec)
   (reduce +)
   (minutes-to-workdays)
   )
  )

;; -------------
;; Check in time
;; -------------

(defn average-checked-in-time
  "Average checked in time for journal files relating to the weekdays in the directory"
  [files]
  (->> files
       (find-in-files find-check-in-time)
       (map :match)
       (vec)
       (filter (comp not nil?))
       ((fn [minutes] (/ (apply + minutes) (count minutes)))) ;; average
       (float)
       (int)
       (minutes-to-duration)
       )
  )

(def predicates {
            :weekday    (partial journal-file-predicate pr/weekday?)
            :q1-weekday (partial journal-file-predicate (every-pred pr/weekday? q1?))
            :q2-weekday (partial journal-file-predicate (every-pred pr/weekday? q2?))
            :q3-weekday (partial journal-file-predicate (every-pred pr/weekday? q3?))
            :q4-weekday (partial journal-file-predicate (every-pred pr/weekday? q4?))
            :weekend    (partial journal-file-predicate pr/weekend?)
            :q1-weekend (partial journal-file-predicate (every-pred pr/weekend? q1?))
            :q2-weekend (partial journal-file-predicate (every-pred pr/weekend? q2?))
            :q3-weekend (partial journal-file-predicate (every-pred pr/weekend? q3?))
            :q4-weekend (partial journal-file-predicate (every-pred pr/weekend? q4?))
            }
  )

(defn stats
  "Get all the stats"
  [predicates files]
  {
   :clocked-in-time {
                     :total (->> files
                                 (filter (comp not nil?))
                                 (sum-total-clocked-minutes))
                     :workday {
                               :year (->> files
                                          (filter (predicates :weekday))
                                          (sum-total-clocked-minutes))
                               :q1 (->> files
                                        (filter (predicates :q1-weekday))
                                        (sum-total-clocked-minutes))
                               :q2 (->> files
                                        (filter (predicates :q2-weekday))
                                        (sum-total-clocked-minutes))
                               :q3 (->> files
                                        (filter (predicates :q3-weekday))
                                        (sum-total-clocked-minutes))
                               :q4 (->> files
                                        (filter (predicates :q4-weekday))
                                        (sum-total-clocked-minutes))
                               }
                     :weekend {
                               :year (->> files
                                          (filter (predicates :weekend))
                                          (sum-total-clocked-minutes))
                               :q1 (->> files
                                        (filter (predicates :q1-weekend))
                                        (sum-total-clocked-minutes))
                               :q2 (->> files
                                        (filter (predicates :q2-weekend))
                                        (sum-total-clocked-minutes))
                               :q3 (->> files
                                        (filter (predicates :q3-weekend))
                                        (sum-total-clocked-minutes))
                               :q4 (->> files
                                        (filter (predicates :q4-weekend))
                                        (sum-total-clocked-minutes))
                               }
   :check-in-time {
                   :workday {
                             :year (->> files
                                        (filter (predicates :weekday))
                                        (average-checked-in-time))
                             :q1 (->> files
                                      (filter (predicates :q1-weekday))
                                      (average-checked-in-time))
                             :q2 (->> files
                                      (filter (predicates :q2-weekday))
                                      (average-checked-in-time))
                             :q3 (->> files
                                      (filter (predicates :q3-weekday))
                                      (average-checked-in-time))
                             :q4 (->> files
                                      (filter (predicates :q4-weekday))
                                      (average-checked-in-time))
                             }
                   :weekend {
                             :year (->> files
                                        (filter (predicates :weekend))
                                        (average-checked-in-time))
                             :q1 (->> files
                                      (filter (predicates :q1-weekend))
                                      (average-checked-in-time))
                             :q2 (->> files
                                      (filter (predicates :q2-weekend))
                                      (average-checked-in-time))
                             :q3 (->> files
                                      (filter (predicates :q3-weekend))
                                      (average-checked-in-time))
                             :q4 (->> files
                                      (filter (predicates :q4-weekend))
                                      (average-checked-in-time))
                             }
                   }

   :support-time {
                  :workday {
                            :year (->> files
                                       (filter (predicates :weekday))
                                       (sum-support-time))
                            :q1 (->> files
                                     (filter (predicates :q1-weekday))
                                     (sum-support-time))
                            :q2 (->> files
                                     (filter (predicates :q2-weekday))
                                     (sum-support-time))
                            :q3 (->> files
                                     (filter (predicates :q3-weekday))
                                     (sum-support-time))
                            :q4 (->> files
                                     (filter (predicates :q4-weekday))
                                     (sum-support-time))
                            }
                  :weekend {
                            :year (->> files
                                       (filter (predicates :weekend))
                                       (sum-support-time))
                            :q1 (->> files
                                     (filter (predicates :q1-weekend))
                                     (sum-support-time))
                            :q2 (->> files
                                     (filter (predicates :q2-weekend))
                                     (sum-support-time))
                            :q3 (->> files
                                     (filter (predicates :q3-weekend))
                                     (sum-support-time))
                            :q4 (->> files
                                     (filter (predicates :q4-weekend))
                                     (sum-support-time))
                            }
                   }
                     }
   }
  )

;; Example usage:

;; (def dir  "/home/mandark/Documents/journal")
;; (def files (journal-files (comp not nil?) dir))

;; (stats predicates files)

;; (clojure.pprint/pprint
;;  (stats predicates files)
;;  )


