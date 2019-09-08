(ns org-clock-stats.stats
  (:use [clojure.string])
  (:require [clj-time.periodic :as p]
            [org-clock-stats.predicate :as pred]
            [clj-time.predicates :as tp]
            [org-clock-stats.convert :as convert])
  )

;; Get files
;;
;; TODO refactor the following so that 3 separate functions aren't needed

;; filename :: File -> string
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
  [dir]
  (->> dir
       (clojure.java.io/file)
       (.listFiles)
       (filter #(.isFile %))
       (filter (partial test-filename pred/date-time?))
       )
  )

;; @todo: refactor the following two
;;        everything is the same except for the .* in the regexp
(defn -find-work-time-starting-with
  "Clocked time from string"
  ([title]
   (partial -find-work-time-starting-with title))
  ([title content]
   (let [[match start duration] (re-find (re-pattern (str "(?i)..  (\\d+:\\d+) " title ".*(\\d+:\\d+)")) content)]
     (convert/duration-to-minutes duration)
     )))
(defn -find-work-time-containing
  "Clocked time from string"
  ([title]
   (partial -find-work-time-containing title))
  ([title content]
   (let [[match start duration] (re-find (re-pattern (str "(?i)..  (\\d+:\\d+) .*" title ".*(\\d+:\\d+)")) content)]
     (convert/duration-to-minutes duration)
     )))

;; find-work-time :: name -> title -> content -> minutes
(defn find-work-time
  "Find work time using a specific search function and title in the given content"
  ([search-fn-name]
   (partial find-work-time search-fn-name))
  ([search-fn-name title]
   (partial find-work-time search-fn-name title))
  ([search-fn-name title content]
   (case search-fn-name
     :title-starts-with (-find-work-time-starting-with title content)
     :title-contains    (-find-work-time-containing title content)
     (throw (Exception. (str  "Search fn-name: " search-fn-name " not recognized")))))
  )

;; find-work-time :: content -> minutes
(defn find-total-time
  "Total clocked time from string"
  [content]
  (let [[match duration] (re-find #"\*Total time\*.*\*(\d+:\d+)\*" content)]
    (convert/duration-to-minutes duration)
    ))


;; apply-fn-file :: (name -> title -> content -> hours) -> File -> hours
;;
;; What if instead of the above, we convert the function so that it takes a File instead of content like the following:
;;
;; > apply-fn-file :: (name -> title -> content -> hours) -> (name -> title -> File -> hours)
;;
(defn apply-fn-file
  "Get the total clocked time for the file"
  ([find-fn]
   (partial apply-fn-file find-fn))
  ([find-fn file]
   {:file file
    :value (try
             (find-fn (slurp file))
             (catch Exception e
               (print (str
                       ""
                       ;; "\n"
                       ;; "Nothing found: " file ". "
                       ;; "Error:" (.getMessage e)
                       ))))
    }))

;; SUM functions
;;
;; The following sum the times for different criterion

;; sum-work-time :: type -> [title] -> [File] -> hours
(defn sum-work-time
  "Sum all work time (in hours) for specified work titles in files"
  ([search-fn-name titles]
   (partial sum-work-time search-fn-name titles))
  ([search-fn-name titles files]
   (->> files

        ;; SCENARIO 1 : Call 'f' number of functions
        ;;
        ;; - make find functions for every titls
        ;; - collapse them (juxt) into one big function
        ;; - call that one big function for every file
        ;; - the one big function is like calling every smaller function on the
        ;;   inputs. if any of the smaller function throws an exception, the
        ;;   whole thing returns nil because the exception is caught on the
        ;;   outside, and the whole computation returns a nil

        ;; (map (apply-fn-file
        ;;       (apply juxt (map (find-work-time :title-starts-with) titles))))

        ;; SCENARIO 2 : Call 't*f' number of functions
        ;;
        ;; - for every title, create a function that takes a title and a file
        ;;   i.e. the total number of functions called in the outer computation
        ;;   are t*f. If something fails, it does't effect the other functions

        (map (apply
              juxt
              (map
               (comp apply-fn-file (find-work-time search-fn-name))
               titles)))
        (flatten)

        (map :value)
        (filter (comp not nil?))
        (flatten)
        (reduce +)
        )))


(defn sum-total-time
  "Sum total time"
  [files]
  (->> files
       (map (apply-fn-file find-total-time))
       (map :value)

       (filter (comp not nil?))
       (reduce +)
       ))


;; STATS

(def preds [:total (comp not nil?)
            ;; :q1 (partial pred/journal-file? q1?)
            ;; :q2 (partial pred/journal-file? q2?)
            ;; :q3 (partial pred/journal-file? q3?)
            ;; :q4 (partial pred/journal-file? q4?)
            :january   (partial pred/journal-file? tp/january?  )
            :february  (partial pred/journal-file? tp/february? )
            :march     (partial pred/journal-file? tp/march?    )
            :april     (partial pred/journal-file? tp/april?    )
            :may       (partial pred/journal-file? tp/may?      )
            :june      (partial pred/journal-file? tp/june?     )
            :july      (partial pred/journal-file? tp/july?     )
            :august    (partial pred/journal-file? tp/august?   )
            :september (partial pred/journal-file? tp/september?)
            :october   (partial pred/journal-file? tp/october?  )
            :november  (partial pred/journal-file? tp/november? )
            :december  (partial pred/journal-file? tp/december? )])

(defn stats
  "Get all the stats"
  [preds files]
  (let [
        build-stats (fn [preds search-fn files]
                      (zipmap
                       (vec
                        (take-nth 2 preds))
                       (vec
                        (map (fn [x] (->> files (filter x) search-fn)) (take-nth 2 (rest preds))))))

        support-time (sum-work-time :title-starts-with ["support"])
        ;; personal-time (sum-work-time :title-contains ["blog" "exsqlaim" "prodigy" "aww.yeah" "pine" "emacs"]) ;; night scout?
        personal-time (sum-work-time :title-contains
                                     [
                                      "aww-yeah"
                                      "blog"
                                      "brand"
                                      "career"
                                      "clj-time"
                                      "cljs"
                                      "clojure"
                                      "emacs"
                                      "emp"
                                      "exercise"
                                      "girdwari"
                                      "grephyte"
                                      "hashpipe"
                                      "manning"
                                      "org-clock-stats"
                                      "pdf.validator"
                                      "personal"
                                      "pine"
                                      "prep"
                                      "system"
                                      "yearly retro"
                                      ]
                                     )
        ]
    {
     :clocked-in-time (build-stats preds sum-total-time files)
     :support-time (build-stats preds support-time files)
     :personal-time (build-stats preds personal-time files)
     }))

(def *files* (journal-files "/home/darkman/Documents/journal/2018"))

(def s (stats preds *files*))

(defn data
  "Get stats data"
  []
  (let [
        labels (take-nth 2 preds)
        ;; all (rest ((apply juxt labels) (:clocked-in-time s)))
        all (->> (:clocked-in-time s)
                 ((apply juxt labels))
                 rest
                 )

        support (->> (:support-time s)
                      ((apply juxt labels))
                      rest
                      )

        personal (->> (:personal-time s)
                      ((apply juxt labels))
                      rest
                      )

        work (->> (map list all support personal) ;; pivot the data
                  (map (fn [[a s p]] (- a s p)))  ;; get only work time
                  )
        ]
    {
     :labels (rest (map name labels))
     :work (map convert/minutes-to-work-days work)
     :support (map convert/minutes-to-work-days support)
     :personal (map convert/minutes-to-work-days personal)
     }
    )
  )

(defn mock-data
  "Get mock data for testing"
  []
  {:labels ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]
   :work (range 26 14 -1)
   :support (range 34 46)
   :personal (range 8 20)
   }
  )
