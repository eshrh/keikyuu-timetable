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

(defn set-innerHTML! [el content]
  (-> el
      (.-innerHTML)
      (set! content)))

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

(defn find-in-stop-list [station train]
  (->> (:stop-list train)
       (filter #(= station (:station %)))
       first))

(defn get-stop [station state train]
  (let [time (->> (find-in-stop-list station train)
                  :time)
        dest (->> (:stop-list train) last :station)]
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

(defn type->english [type]
  (-> type
      (.split ":")
      first
      (case
        "快特" "lim-exp"
        "特急" "spe-exp"
        "エアポート急行" "airport-spe"
        "普通" "local"
        "エアポート快特" "airport-lim"
        "")))

(defn stop->id [stop]
  (let [state (:state stop)]
    (str (case (:dir state)
           :up "u"
           :down "d")
         (case (:schedule state)
           :weekday "w"
           :holiday "h")
         (:index stop))))

(defn truncate [station]
  (if (> (count station) 16)
    (.slice station 0 2)
    station))

(defn stop->html [stop]
  (str "<ruby class=\"stop "
       (type->english (:type stop)) "\""
       "train-id=\"" (stop->id stop) "\""
       ">"
       (:time stop)
       "<rt>" (truncate (:dest stop)) "</rt>"
       "</ruby>"))

(defn add-cell [row content]
  (let [cell (.insertCell row)]
    (-> cell
        (set-innerHTML! content))
    cell))

(def train-clicked-channel (async/chan 1))
(defn add-stop-listener [stop-el stop station]
  (-> stop-el
      (.addEventListener "click" #(go (>! train-clicked-channel
                                          {:stop stop
                                           :station station})))))

(defn add-stop [row stop station]
  (when-not (nil? stop)
    (-> (add-cell row (stop->html stop))
        (add-stop-listener stop station))))

(def special-stations
  '("品川"
    "京急蒲田"
    "京急川崎"
    "横浜"
    "上大岡"
    "金沢八景"
    "横須賀中央"
    "京急久里浜"
    "ＹＲＰ野比"))

(defn in? [coll el]
  (pos? (.indexOf el coll)))

(defn format-station-name [station]
  (let [special? (in? station special-stations)]
    (str "<p class=\"station "
         (if special?
           "spe-station") "\">" station "</p>")))

(def station-clicked-channel (async/chan 1))
(defn add-station-name [row station]
  (-> (add-cell row (format-station-name station))
      (.addEventListener "click" #(go (>! station-clicked-channel
                                          station)))))

(defn add-station [station data state num]
  (let [table (get-element-by-id "data")
        row (.insertRow table)
        stops (get-stops data station state)]
    (add-station-name row station)
    (doseq [stop (take num stops)]
      (add-stop row stop station))))

(defn reset [id]
  (-> (get-element-by-id id)
      (set-innerHTML! "")))

(defn add-all [stations data state num]
  (reset "data")
  (doseq [station stations]
    (add-station station data state 5)))

(defn make-train-info-visible []
  (-> (get-element-by-id "info-overlay")
      (.-style)
      (.-display)
      (set! "block")))

(defn reset-train-info []
  (reset "info-type")
  (reset "info-station")
  (reset "info-table"))

(defn add-train-info [train station]
  (let [table (get-element-by-id "info-table")
        stop-list (drop-while #(not= (:station %)
                                     station)
                   (:stop-list train))]
    (doseq [stop stop-list]
      (let [row (.insertRow table)]
        (add-cell row (:station stop))
        (add-cell row (:time stop))))
    (-> (get-element-by-id "info-type")
        (set-innerHTML! (str "<p> type: "
                            (:type train) "</p>")))
    (-> (get-element-by-id "info-station")
        (set-innerHTML! (str "<p> dest: "
                            ((comp :station last) stop-list)
                            "</p>")))))

(defn show-train-info [trains clicked]
  (let [stop (:stop clicked)
        station (:station clicked)
        train (nth (index (:state stop) trains)
                   (:index stop))]
    (reset-train-info)
    (add-train-info train station)
    (make-train-info-visible)))

(defn find-last-trains [station stations trains state]
  (let [stations-in-dir ((if (= (:dir state) :up)
                           take-while drop-while)
                         (partial not= station) stations)
        trains-at-sta (->> trains
                           (index state)
                           (filter #(in? station
                                         (map :station (:stop-list %))))
                           (sort-by #(time-after-three
                                      (:time (find-in-stop-list station %)))
                                    >))]
    (map (fn [sta]
           {:dest sta
            :depart (->> (filter #(some? (find-in-stop-list sta %)) trains-at-sta)
                         first
                         (find-in-stop-list station)
                         :time)})
         stations-in-dir)))

(defn show-last-trains [station last-trains]
  (let [table (get-element-by-id "info-table")]
    (reset-train-info)
    (-> (get-element-by-id "info-station")
        (set-innerHTML! (str "<p> from: " station "</p>")))
    (-> (get-element-by-id "info-type")
        (set-innerHTML! (str "<p> 終電 </p>")))
    (doseq [departure last-trains]
      (let [row (.insertRow table)]
        (add-cell row (:dest departure))
        (add-cell row (:depart departure))))
    (make-train-info-visible)))

(defn update-time []
  (-> (get-element-by-id "time")
      (set-innerHTML! (.toTimeString (new js/Date)))))

(-> js/window (.setInterval update-time 1000))

(defn set-schedule-state [schedule]
  (-> (get-element-by-id
       (if (= schedule :weekday) "weekday" "holiday"))
      (.-checked)
      (set! true)))

(defn init-state []
  (let [day (.getDay (new js/Date))
        time-after-three (get-time-after-three)]
    (if (or (and (= day 6)
                 (< time-after-three 1260))
            (and (= day 0)
                 (> time-after-three 1260)))
      (set-schedule-state :holiday)
      (set-schedule-state :weekday))))

(init-state)

(def state-channel (async/chan 1))

(add-listeners #(go (>! state-channel (get-state))))
(async/put! state-channel (get-state))

(def publish-data-channel (async/chan 1))
(def sub-data-channel (async/pub publish-data-channel :data))

(def subscriber-for-radio-buttons (async/chan 1))
(async/sub sub-data-channel :all subscriber-for-radio-buttons)
(go
  (let [data (<! subscriber-for-radio-buttons)
        trains (:trains data)
        stations (:stations data)]
    (while true
      (add-all stations trains (<! state-channel) 3))))

(def subscriber-for-train-info (async/chan 1))
(async/sub sub-data-channel :all subscriber-for-train-info)
(go
  (let [trains (:trains (<! subscriber-for-train-info))]
    (while true
      (show-train-info trains (<! train-clicked-channel)))))

(def subscriber-for-last-trains (async/chan 1))
(async/sub sub-data-channel :all subscriber-for-last-trains)
(go
  (let [data (<! subscriber-for-last-trains)
        trains (:trains data)
        stations (:stations data)]
    (while true
      (let [station-clicked (<! station-clicked-channel)]
        (show-last-trains station-clicked (find-last-trains
                                           station-clicked
                                           stations trains
                                           (get-state)))))))

(go
  (>! publish-data-channel
      {:data :all
       :trains (->> (<! (http/get "/resources/keikyuu.edn"))
                    (:body)
                    (reader/read-string))
       :stations (->> (<! (http/get
                           "/resources/keikyuu-stations.edn"))
                      (:body)
                      (reader/read-string))}))
