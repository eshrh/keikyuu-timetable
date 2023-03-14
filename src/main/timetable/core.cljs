(ns timetable.core
  (:require [cljs.reader :as reader]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [ajax.core :refer [GET POST]])
  (:require-macros [cljs.core.async.macros :refer [go]]))



(defn add-station [station]
  (let [table (-> js/document
                  (.getElementById "data"))
        row (.insertRow table)
        cell (.insertCell row)]
    (.appendChild cell (.createTextNode js/document station))))

(defn add-listeners [update-func]
  (map #(.addEventListener % "click" update-func)
       (.getElementsByTagName js/document "input")))

(defn get-element-by-id [id]
  (-> js/document
      (.getElementById id)))

(defn get-state []
  (let [up-checked (.-checked (get-element-by-id "up"))
        weekday-checked (.-checked (get-element-by-id "weekday"))]
    {:dir (if up-checked :up :down)
     :schedule (if weekday-checked :weekday :holiday)}))

(defn minutes-after-three [h m]
  (let [minutes (+ m (* h 60))]
    (- (+ (if (< minutes 180) 1440 0)
          minutes)
       180)))

(defn time-after-three [time]
  (let [[h m] (map js/parseInt (clojure.string/split time ":"))]
    (minutes-after-three h m)))

(defn get-stop [station state train]
  (let [time (->> (:stop-list train)
                  (filter #(= station (:station %)))
                  first
                  :time)]
    (when time
      {:type (:type train)
       :time time
       :sort-time (time-after-three time)
       :index (:index train)
       :state state
       :dest (->> (:stop-list train) last :station)})))

(defn index [state data]
  ((:schedule state) ((:dir state) data)))

(defn get-stops [data station state]
  (let [time (get-time-after-three)]
    (->> data
         (index state)
         (map (partial get-stop station state))
         (filter some?)
         (sort-by :sort-time)
         (drop-while (fn [stop]
                       (< (:sort-time stop) time))))))

(def teststate {:dir :down :schedule :weekday})

(take 3 (get-stops kk "品川" teststate))
(map (comp :station last :stop-list)
     (index {:dir :down :schedule :weekday} kk))
(nth ())


(defn get-time-after-three []
  (let [time (new js/Date)]
    (minutes-after-three
     (.getHours time)
     (.getMinutes time))))

(go
  (let [trains (->> (<! (http/get "/resources/keikyuu.edn"))
                    (:body)
                    (reader/read-string))
        stations (->> (<! (http/get "/resources/keikyuu-stations.edn"))
                      (:body)
                      (reader/read-string))]
    ))

(def resp (doto (new js/XMLHttpRequest)
            (.open "GET" "/resources/keikyuu.edn")
            (.send)))

(defn append-index [coll]
  (map-indexed (fn [i el]
                 (assoc el :index i)
                 ) coll))

(def kknoindex (reader/read-string resp.response))

(def kk {:down {:weekday (append-index (:weekday (:down keik)))
                :holiday (append-index (:holiday (:down keik)))}
         :up {:weekday (append-index (:weekday (:up keik)))
              :holiday (append-index (:holiday (:up keik)))}})
