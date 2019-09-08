(ns org-clock-stats.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [org-clock-stats.core-test]))

(doo-tests 'org-clock-stats.core-test)

