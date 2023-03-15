(ns timetable.scraper)
(use 'hickory.core)
(require '[clj-http.client :as http])
(require '[hickory.select :as s])
(require '[clojure.string :as str])

;; sengakuji: 250-0
;; uraga: 250-49

;; horinouti: 679-0
;; misakiguti: 679-8

;; dw=0, 1, 2
;;    平 土 休日

(defn to-hickory [url]
  (-> (http/get url)
      :body parse as-hickory))

(def page (to-hickory "https://ekitan.com/timetable/railway/line-station/250-25/d1?view=list&dw=0"))
(def base "https://ekitan.com/")

(defn construct-url [line sta dir dw]
  (str base "timetable/railway/line-station/" line "-" sta "/d" dir
       "?dw="dw "&view=list"))

(defrecord Stop [station time])
(defrecord Train [stop-list type direction])

(defn stop->time [entry]
  (let [content (->> entry
                     (s/select (s/class "td-dep-and-arr-time"))
                     first
                     :content)
        arr (re-seq #"(\d\d:\d\d)着" (first content))
        dep (re-seq #"(\d\d:\d\d)発" (last content))]
    (->> (if (some? dep)
           dep arr)
         first
         last)))

(defn stop->station [entry]
  (->> entry
       (s/select (s/child (s/class "td-station-name")
                          (s/tag "a")))
       first :content first))

(defn train->stop-list [train]
  (println "fetching train...")
  (->> train
       (s/select (s/descendant
                  (s/or (s/class "result-route-departure")
                        (s/class "result-route-transfer"))
                  (s/tag "tr")))
       (map (fn [stop]
              (->Stop
               (stop->station stop)
               (stop->time stop))))))

(defn station->train [station use-all-trains]
  (let [first-trains
        (->> (s/select (s/descendant
                        (s/and (s/class "tab-content-inner")
                               (s/class "active"))
                        (s/class "ek-train-link")) station)
             (filter (if use-all-trains (constantly true)
                         (fn [el]
                           (->> el
                                (s/select
                                 (s/class "this-first-train"))
                                first :content
                                some?)))))
        train-info (map (fn [el]
                          {:link (->> el
                                      :attrs :href
                                      (str base))
                           :type (->> el
                                      (s/select
                                       (s/class "train-type"))
                                      first :content first)})
                        first-trains)
        direction (->> station
                       (s/select
                        (s/and (s/class "ek-direction_tab")
                               (s/class "active")))
                       first
                       :attrs
                       :data-ek-direction_name)]
    (map #(->Train
           ((comp train->stop-list to-hickory :link) %)
           (:type %)
           direction)
         train-info)))

(defn get-stations [line-page]
  (map (comp first :content)
       (s/select (s/child
                  (s/tag "dl")
                  (s/tag "dt")
                  (s/tag "a")) line-page)))

(defn try-get [url]
  (try
    (println url)
    (to-hickory url)
    (catch Exception e
      (if (not (= (:status (ex-data e)) 404))
        (throw (ex-info
                "uncaught exception"
                {:causes "website failed but not 404"}))))))
