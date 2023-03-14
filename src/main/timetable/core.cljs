(ns timetable.core
  (:require [cljs.reader :as reader]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! >!]]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn add-listeners [update-func]
  (doseq [radio (.getElementsByTagName js/document "input")]
    (.addEventListener radio "click" update-func)))

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
                  :time)
        dest (->> (:stop-list train) last :station)]
    (println station)
    (when (and (some? time)
               (not= dest station))
      {:type (:type train)
       :time time
       :sort-time (time-after-three time)
       :index (:index train)
       :state state
       :dest dest})))

(defn index [state data]
  ((:schedule state) ((:dir state) data)))

(defn get-time-after-three []
  (let [time (new js/Date)]
    (minutes-after-three
     (.getHours time)
     (.getMinutes time))))

(defn get-stops [data station state]
  (let [time (get-time-after-three)]
    (->> data
         (index state)
         (map (partial get-stop station state))
         (filter some?)
         (sort-by :sort-time)
         (drop-while (fn [stop]
                       (< (:sort-time stop) time))))))



(defn add-cell [row content]
  (let [element (if (string? content)
                  (.createTextNode js/document content)
                  content)]
    (-> (.insertCell row)
        (.appendChild element))))

(defn add-station [station data state num]
  (let [table (-> js/document
                  (.getElementById "data"))
        row (.insertRow table)
        station-name (if true
                       (.slice station 0 -1)
                       station)]
    (if (= (last "品川駅") "駅")
      (println "yes")
      (println "no"))
    (println (.slice "品川駅" 0 -1))
    (add-cell row station-name)
    (println (->> data
                  (index state)
                  first
                  (get-stop station-name state)))
    (doseq [stop (take num (get-stops data station-name state))]
      (add-cell row (:time stop)))))

(defn reset []
  (-> js/document
      (.getElementById "data")
      (.-innerHTML)
      (set! "")))

(defn add-all [stations data state num]
  (reset)
  (doseq [station stations]
    (add-station station data state 3)))

(def state-channel (async/chan))

(add-listeners #(go (>! state-channel (get-state))))
(async/put! state-channel (get-state))

(go
  (let [trains (->> (<! (http/get "/resources/keikyuu.edn"))
                    (:body)
                    (reader/read-string))
        stations (->> (<! (http/get "/resources/keikyuu-stations.edn"))
                      (:body)
                      (reader/read-string))]
    (println (first (index {:dir :up :schedule :weekday}
                           trains)))
    (while true
      (add-all stations trains (<! state-channel) 3))))
