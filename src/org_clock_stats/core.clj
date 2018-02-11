(ns org-clock-stats.core
  (:use [clojure.string])
  (require [clj-time.core :as t])
  (require [clj-time.format :as f])
  (require [clj-time.predicates :as pr])
  (require [clj-time.periodic :as p])
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
        (format "%02d:%02d" h m))
    )
  )

(defn minutes-to-workdays
  "Minutes to a string representing the work days"
  [minutes]
  (let [h (int (/ minutes 60))
        m (mod minutes 60)
        workdays (if (> h 6) (int (/ h 6)))
        ]
    (if workdays (str workdays " workday" (if (> workdays 1) "s") ", " (format "%02d:%02d" (mod h 6) m))
        (format "%02d:%02d" h m)
        )
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
  ([find-fn]
   (partial find-in-file find-fn))
  ([find-fn file]
   {:file file
    :match (try
             (find-fn (slurp file))
             (catch Exception e
               (print "")
               ;; (print (str "\nIgnoring file: " file " since it doesn't contain the values. Error:" (.getMessage e)))
               ))
    })
  )

(defn find-in-files
  "Run the passed function for every journal file passed"
  ([find-fn]
   (partial find-in-files find-fn))
  ([find-fn files]
   (->>
    files
    (map (partial find-in-file find-fn))
    ))
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
  (minutes-to-workdays
   (if (empty? files) 0
       (->>
        files
        (total-clocked-minutes)
        (filter (fn [x] ((comp not nil?) (:match x))))
        (map :match)
        (vec)
        (reduce +)))))

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
  (minutes-to-workdays
   (if (empty? files) 0
       (->>
        files
        (support-time)
        (filter (fn [x] ((comp not nil?) (:match x))))
        (map :match)
        (vec)
        (reduce +)))))

;; -------------
;; Check in time
;; -------------

(defn average-checked-in-time
  "Average checked in time for journal files relating to the weekdays in the directory"
  [files]
  (minutes-to-duration
   (if (empty? files) 0
       (->> files
            (find-in-files find-check-in-time)
            (map :match)
            (vec)
            (filter (comp not nil?))
            ;; average
            ((fn [minutes]
               (if (= minutes 0)
                 0
                 (/ (apply + minutes) (count minutes)))))
            (float)
            (int)))))

(defn off-days
  "Get all the vacation days as a set"
  [vacation]
  (clojure.set/union
   (->> vacation
        (:single)
        (map (comp to-date-time name))
        (set))
   (->> vacation
        (:range)
        (map (fn [x]
               (let [start (to-date-time (name (:start x)))
                     end (to-date-time (name (:end x)))
                     days (clojure.set/union (p/periodic-seq start end (t/days 1)))]
                 (set
                  (conj days end)))))
        (apply clojure.set/union))))

(defn off-day?
  "Check if the given date time is a non-working days using the vacation days and public holidays"
  [vacation-days public-holidays s]
  (boolean
   (or
    (vacation-days s)
    (public-holidays s)
    )
   )
  )

;; (->> files
;;      (filter (predicates :q1-workday)))

(defn stats
  "Get all the stats"
  [predicates files]
  {:clocked-in-time {:total (->> files
                                 (filter (comp not nil?))
                                 (sum-total-clocked-minutes))
                     :work {:year (->> files
                                     (filter (predicates :workday))
                                     (sum-total-clocked-minutes))
                          :q1 (->> files
                                   (filter (predicates :q1-workday))
                                   (sum-total-clocked-minutes))
                          :q2 (->> files
                                   (filter (predicates :q2-workday))
                                   (sum-total-clocked-minutes))
                          :q3 (->> files
                                   (filter (predicates :q3-workday))
                                   (sum-total-clocked-minutes))
                          :q4 (->> files
                                   (filter (predicates :q4-workday))
                                   (sum-total-clocked-minutes))}
                     :off {:year (->> files
                                      (filter (predicates :offday))
                                      (sum-total-clocked-minutes))
                           :q1 (->> files
                                    (filter (predicates :q1-offday))
                                    (sum-total-clocked-minutes))
                           :q2 (->> files
                                    (filter (predicates :q2-offday))
                                    (sum-total-clocked-minutes))
                           :q3 (->> files
                                    (filter (predicates :q3-offday))
                                    (sum-total-clocked-minutes))
                           :q4 (->> files
                                    (filter (predicates :q4-offday))
                                    (sum-total-clocked-minutes))}}
   :check-in-time {:work {:year (->> files
                                   (filter (predicates :workday))
                                   (average-checked-in-time))
                        :q1 (->> files
                                 (filter (predicates :q1-workday))
                                 (average-checked-in-time))
                        :q2 (->> files
                                 (filter (predicates :q2-workday))
                                 (average-checked-in-time))
                        :q3 (->> files
                                 (filter (predicates :q3-workday))
                                 (average-checked-in-time))
                        :q4 (->> files
                                 (filter (predicates :q4-workday))
                                 (average-checked-in-time))}
                   :off {:year (->> files
                                    (filter (predicates :offday))
                                    (average-checked-in-time))
                         :q1 (->> files
                                  (filter (predicates :q1-offday))
                                  (average-checked-in-time))
                         :q2 (->> files
                                  (filter (predicates :q2-offday))
                                  (average-checked-in-time))
                         :q3 (->> files
                                  (filter (predicates :q3-offday))
                                  (average-checked-in-time))
                         :q4 (->> files
                                  (filter (predicates :q4-offday))
                                  (average-checked-in-time))}}
   :support-time {:work {:year (->> files
                                  (filter (predicates :workday))
                                  (sum-support-time))
                       :q1 (->> files
                                (filter (predicates :q1-workday))
                                (sum-support-time))
                       :q2 (->> files
                                (filter (predicates :q2-workday))
                                (sum-support-time))
                       :q3 (->> files
                                (filter (predicates :q3-workday))
                                (sum-support-time))
                       :q4 (->> files
                                (filter (predicates :q4-workday))
                                (sum-support-time))}
                  :off {:year (->> files
                                   (filter (predicates :offday))
                                   (sum-support-time))
                        :q1 (->> files
                                 (filter (predicates :q1-offday))
                                 (sum-support-time))
                        :q2 (->> files
                                 (filter (predicates :q2-offday))
                                 (sum-support-time))
                        :q3 (->> files
                                 (filter (predicates :q3-offday))
                                 (sum-support-time))
                        :q4 (->> files (filter (predicates :q4-offday))
                                 (sum-support-time))}}})


;; Example usage:

(def vacation {
               :half   #{ :20170620 :20171019 }
               :single #{ :20170113 :20170120 :20170505 }
               :range  #{
                         { :start :20170620 :end :20170706 } ;; Pakistan
                         { :start :20171019 :end :20171022 } ;; Barcelona
                         { :start :20171227 :end :20171229 } ;; Mandatory vacation Penneo
                         }
               })

(def public-holidays {
                      :single #{
                                :20170101 ;; New year
                                :20170512 ;; Prayer day (4th Friday after Easter)
                                :20170525 ;; Ascension day (40 days after Easter)
                                :20170605 ;; Whit Monday (7th Monday after Easter)
                                }
                      :range  #{
                                { :start :20170413 :end :20170417 } ;; Easter
                                { :start :20171224 :end :20171226 } ;; Christmas
                                }
                      })

;; TODO: memoize
(def *public-holidays* (off-days public-holidays))
(def *vacation-days* (off-days vacation))


(def predicates
  {
   :workday    (partial journal-file-predicate (every-pred pr/weekday? (comp not (partial off-day? *vacation-days* *public-holidays*))))
   :q1-workday (partial journal-file-predicate (every-pred pr/weekday? (comp not (partial off-day? *vacation-days* *public-holidays*)) q1?))
   :q2-workday (partial journal-file-predicate (every-pred pr/weekday? (comp not (partial off-day? *vacation-days* *public-holidays*)) q2?))
   :q3-workday (partial journal-file-predicate (every-pred pr/weekday? (comp not (partial off-day? *vacation-days* *public-holidays*)) q3?))
   :q4-workday (partial journal-file-predicate (every-pred pr/weekday? (comp not (partial off-day? *vacation-days* *public-holidays*)) q4?))

   :offday    (partial journal-file-predicate (some-fn (partial off-day? *vacation-days* *public-holidays*) pr/weekend?))
   :q1-offday (partial journal-file-predicate (some-fn (partial off-day? *vacation-days* *public-holidays*) (every-pred pr/weekend? q1?)))
   :q2-offday (partial journal-file-predicate (some-fn (partial off-day? *vacation-days* *public-holidays*) (every-pred pr/weekend? q2?)))
   :q3-offday (partial journal-file-predicate (some-fn (partial off-day? *vacation-days* *public-holidays*) (every-pred pr/weekend? q3?)))
   :q4-offday (partial journal-file-predicate (some-fn (partial off-day? *vacation-days* *public-holidays*) (every-pred pr/weekend? q4?)))
   }
  )


(def *dir*  "/home/mandark/Documents/journal/2017")
(def *files* (journal-files (comp not nil?) *dir*))


(clojure.pprint/pprint
 (stats predicates *files*)
 )


