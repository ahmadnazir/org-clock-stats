(ns org-clock-stats.app
  (:require [org-clock-stats.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
