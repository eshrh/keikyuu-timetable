(defproject ekitan "0.0.1"
  :description "Scraper for ekitan.com"
  :license {:name "GPL-3.0-or-later"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clj-commons/hickory "0.7.3"]
                 [clj-http "3.12.3"]]
  :repl-options {:init-ns timetable.scraper})
