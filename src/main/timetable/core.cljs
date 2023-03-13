(ns timetable.core
  (:require [cljs.reader :as reader]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [ajax.core :refer [GET POST]])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(defn setup-station-list [stations]
  (prn stations))


(go (let [trains (->> (<! (http/get "/resources/keikyuu.edn"))
                      (:body)
                      (reader/read-string))
          stations (->> (<! (http/get "/resources/keikyuu-stations.edn"))
                        (:body)
                        (reader/read-string))]
      (setup-station-list stations)))


