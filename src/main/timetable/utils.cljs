(ns timetable.utils)

(defrecord Stop [station time])
(defrecord Train [stop-list type direction])
