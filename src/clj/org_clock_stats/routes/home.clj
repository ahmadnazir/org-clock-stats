(ns org-clock-stats.routes.home
  (:require
    [org-clock-stats.layout :as layout]
    [clojure.java.io :as io]
    [org-clock-stats.middleware :as middleware]
    [org-clock-stats.stats :as stats]
    [ring.util.http-response :as response]))

(defn home-page [request]
  (layout/render request "home.html"))

(defn home-routes []
  [""
   {:middleware [middleware/wrap-csrf
                 middleware/wrap-formats]}
   ["/" {:get home-page}]
   ["/docs" {:get (fn [_]
                    (->
                     (response/ok (-> "docs/docs.md" io/resource slurp))
                     (response/header "Content-Type" "text/plain; charset=utf-8")))}]
   ["/stats" {:get (fn [_]
                    (->
                     (response/ok (stats/data)
                                  )
                     ))}]
   ["/overview" {:get (fn [_]
                    (->
                     (response/ok (-> "overview/overview.html" io/resource slurp))
                     (response/header "Content-Type" "text/plain; charset=utf-8")))}]])

