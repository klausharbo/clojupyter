(ns clojupyter.misc.util
  (:require
   [clj-time.core				:as time]
   [clj-time.format				:as time-format]
   [clojure.pprint				:as pp]
   [net.cgrand.sjacket.parser			:as p]
   [taoensso.timbre				:as log]
   [zprint.core					:as zp]
   ))

(defn uuid
  []
  (str (java.util.UUID/randomUUID)))

(defn now
  []
  "Returns current ISO 8601 compliant date."
  (let [current-date-time (time/to-time-zone (time/now) (time/default-time-zone))]
    (time-format/unparse
     (time-format/with-zone (time-format/formatters :date-time-no-ms)
       (.getZone current-date-time))
     current-date-time)))

(defn pp-str
  [v]
  (with-out-str (pp/pprint v)))

(defn rcomp
  [& fs]
  (apply comp (reverse fs)))

(def reformat-form
  (rcomp read-string zp/zprint-str pr-str println)) 
