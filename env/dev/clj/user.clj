(ns user
  (:require
    [org-clock-stats.config :refer [env]]
    [clojure.spec.alpha :as s]
    [expound.alpha :as expound]
    [mount.core :as mount]
    [org-clock-stats.figwheel :refer [start-fw stop-fw cljs]]
    [org-clock-stats.core :refer [start-app]]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(defn start []
  (mount/start-without #'org-clock-stats.core/repl-server))

(defn stop []
  (mount/stop-except #'org-clock-stats.core/repl-server))

(defn restart []
  (stop)
  (start))


