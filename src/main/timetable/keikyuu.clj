(ns timetable.keikyuu
  (:require [timetable.scraper :as scraper])
  (:require [clj-http.client :as http])
  (:require [clojure.string :as str])
  (:require [hickory.select :as s]))

(defn get-trains
  ([line sta dir dw get-all-trains]
   (let [page (scraper/try-get
               (scraper/construct-url line sta dir dw))]
     (if (some? page)
       (scraper/station->train page get-all-trains))))
  ([line sta dir dw]
   (get-trains line sta dir dw false)))

(defn get-all-trains [line [station-s station-e] dir dw]
  (apply concat (mapv #(get-trains line % dir 0)
                      (range station-s station-e))))

(defn write-fast [name data]
  (with-open [w (clojure.java.io/writer name)]
    (.write w (pr-str data))))

(defn apply-both-schedules [func]
  {:weekday (apply func [0])
   :holiday (apply func [1])})

(def honsen {:up (apply-both-schedules
                  #(get-all-trains 250 [1 50] 1 %))
             :down (apply-both-schedules
                    #(concat
                      (get-trains 250 0 1 % true)
                      (get-all-trains 250 [1 50] 2 %)))})

(write-fast "resources/honsen.edn" honsen)
;; (def honsen (read-string (slurp "resources/honsen.edn")))

(def kurihamasen {:up (apply-both-schedules
                       #(get-all-trains 679 [0 9] 1 %))
                  :down (apply-both-schedules
                         #(get-all-trains 679 [0 9] 2 %))})

(write-fast "resources/kurihamasen.edn" kurihamasen)
;; (def kurihamasen
;;   (read-string (slurp "resources/kurihamasen.edn")))

(def zushisen {:down nil
               :up (apply-both-schedules
                    #(get-trains 252 4 1 %))})

(def airport (apply-both-schedules
              #(get-trains 253 5 1 %)))

(defn airport-down? [train]
  (let [dest ((comp :station last :stop-list) train)]
    (or (= dest "逗子・葉山")
        (= dest "金沢文庫"))))

(defn indexer [data dir day]
  (day (dir data)))

(def airport-split
  {:up (apply-both-schedules
        #(filter (comp not airport-down?)
                 ((if % :weekday :holiday) airport)))
   :down (apply-both-schedules
          #(filter (comp airport-down?)
                   ((if % :weekday :holiday) airport)))})

(defn cc-all [dir day]
  (mapcat #(indexer % dir day)
          [honsen kurihamasen zushisen airport-split]))

(defn apply-func-to-all [f]
  {:down {:weekday (f :down :weekday)
          :holiday (f :down :holiday)}
   :up {:weekday (f :up :weekday)
        :holiday (f :up :holiday)}})

(def keikyuu (apply-func-to-all cc-all))

(defn train-to-map [train]
  {:type (:type train)
   :direction (:direction train)
   :stop-list (->> (:stop-list train)
                   (map #(into {} %)))})

(defn append-index [coll]
  (map-indexed (fn [i el]
                 (assoc el :index i)) coll))

(def keikyuu-map
  (apply-func-to-all #(append-index
                       (map train-to-map
                            (indexer keikyuu %1 %2)))))

(write-fast "public/resources/keikyuu.edn" keikyuu-map)
(def keikyuu-map
  (read-string (slurp "resources/keikyuu-map.edn")))


(def station-list (concat
                   (scraper/get-stations
                    (scraper/try-get
                     "https://ekitan.com/timetable/railway/line/8200"))
                   (scraper/get-stations
                    (scraper/try-get
                     "https://ekitan.com/timetable/railway/line/8300"))))

(write-fast "public/resources/keikyuu-stations.edn" station-list)
