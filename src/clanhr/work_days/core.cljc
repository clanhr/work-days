(ns clanhr.work-days.core
  "Calculates work days on two dates"
  (:require [clj-time.periodic :as p]
            [clj-time.predicates :as pr]
            [clj-time.coerce :as c]
            [clj-time.core :as t]))

(defn- build-date
  "Creates a joda date from string"
  [absence field]
  (assoc absence field (c/from-string (field absence))))

(defn- build
  "Prepares an absence"
  [absence]
   (cond-> absence
     (nil? (:duration-type absence)) (assoc :duration-type "days")
     (string? (:start-date absence)) (build-date :start-date)
     (nil? (:end-date absence)) (assoc :end-date (:start-date absence))
     (string? (:end-date absence)) (build-date :end-date)))

(defn hours-per-day
  "Gets the working hours per day"
  [settings]
  (or (:hours-per-day settings) 8))

(defn days-interval
  "Gets the number of days between start-date and end-date"
  [settings absence]
  (+ 1 (t/in-days (t/interval (:start-date absence)
                              (:end-date absence)))))

(defn absence->days-coll
  "Transforms the absence duration in a collection will all the days"
  [settings absence]
  (take (days-interval settings absence)
        (p/periodic-seq (:start-date absence) (t/days 1))))

(defn days-interval-remove-dayoff
  "Gets the number of days between start-date and end-date, removing the
  dayoffs defined on the settings"
  [settings absence]
  (->> (absence->days-coll settings absence)
       (filter pr/weekday?)
       count))

(defn total-vacation-days
  "Gets the total vacation days on this absence"
  [settings absence]
  (let [absence (build absence)]
    (if (= "vacations" (:absence-type absence))
      (days-interval-remove-dayoff settings absence)
      0)))

(defn total-absence-hours
  "Gets the total vacation days on this absence"
  [settings absence]
  (let [absence (build absence)]
    (if (not= "vacations" (:absence-type absence))
      (if (= "days" (:duration-type absence))
        (* (hours-per-day settings)
           (days-interval settings absence))
        (:hours absence))
      0)))

(defn calculate
  "Gets the raw duration of the absence, in days or hours, depending on the
  duration type"
  [settings absence]
  (let [absence (build absence)]
    (if (= (:duration-type absence) "days")
      (if (= (:absence-type absence) "vacations")
        (days-interval-remove-dayoff settings absence)
        (days-interval settings absence))
      (:hours absence))))