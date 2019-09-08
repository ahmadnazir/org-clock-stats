(ns org-clock-stats.events
  (:require
    [re-frame.core :as rf]
    [ajax.core :as ajax]
    [cognitect.transit :as t]
    ))

;;dispatchers

(rf/reg-event-db
  :navigate
  (fn [db [_ route]]
    (assoc db :route route)))

(rf/reg-event-db
 :set-template
 (fn [db [_ template]]
   (assoc db :template template)))

(rf/reg-event-fx
  :fetch-template
  (fn [_ _]
    {:http-xhrio {:method          :get
                  :uri             "/overview"
                  :response-format (ajax/raw-response-format)
                  :on-success       [:set-template]}}))

;; Move the actions to a separate namespace

;; start --------------------------------------------------------------------------
(defn render-overview
  [labels official support personal]
  (let [context (.getContext (.getElementById js/document "rev-chartjs") "2d")
        b1 "rgb(66, 95, 135)"
        b2 "rgb(199, 210, 226)"
        o1 "rgb(255, 140, 0)"
        o2  "rgb(255, 185, 100)"
        a1 "rgb(154, 205, 178)"
        a2  "rgb(200, 230, 214)"
        chart-data {:type "line"
                    :data {:labels labels
                           :datasets [{:data official
                                       :label "Official"
                                       :backgroundColor b1
                                       :borderColor b1
                                       :fill false
                                       }
                                      {:data support
                                       :label "Support"
                                       :backgroundColor o1
                                       :borderColor o1
                                       :fill false
                                       }
                                      {:data personal
                                       :label "Side projects"
                                       :backgroundColor a1
                                       :borderColor a1
                                       :fill false
                                       }
                                      ]}}
        ]
    (js/Chart. context (clj->js chart-data))
    (print "rendered")))


(rf/reg-event-fx
 :render-overview
 (fn [db [_ stats]]
   (render-overview
    (stats "labels")
    (stats "work")
    (stats "support")
    (stats "personal")
    ))
 )

(rf/reg-event-fx
 :set-stats
 (fn [cofx [_ stats]]
   (let [s (t/read (t/reader :json) stats)]
     {:db       (assoc (:db cofx) :stats s)
      :dispatch [:render-overview s]
      })
   ))

;; end   --------------------------------------------------------------------------

(rf/reg-event-db
 :set-docs
 (fn [db [_ docs]]
   (assoc db :docs docs)))

(rf/reg-event-fx
 :fetch-docs
 (fn [_ _]
   {:http-xhrio {:method          :get
                 :uri             "/docs"
                 :response-format (ajax/raw-response-format)
                 :on-success       [:set-docs]}}))

(rf/reg-event-fx
  :fetch-stats
  (fn [_ _]
    {:http-xhrio {:method          :get
                  :uri             "/stats"
                  :response-format (ajax/raw-response-format)
                  :on-success       [:set-stats]}}
    ))

(rf/reg-event-db
  :common/set-error
  (fn [db [_ error]]
    (assoc db :common/error error)))

(rf/reg-event-db
 :init-db
 (fn [db [_ error]]
   {:stats {"labels" ["test"],
           "work" [1 2],
           "support" [2 1]
           "personal" [3 6]
           }}))

;;subscriptions

(rf/reg-sub
  :route
  (fn [db _]
    (-> db :route)))

(rf/reg-sub
  :page
  :<- [:route]
  (fn [route _]
    (-> route :data :name)))

(rf/reg-sub
  :docs
  (fn [db _]
    (:docs db)))

(rf/reg-sub
 :template
 (fn [db _]
   (:template db)))

(rf/reg-sub
 :stats
 (fn [db _]
   (:stats db)))

(rf/reg-sub
  :common/error
  (fn [db _]
    (:common/error db)))
