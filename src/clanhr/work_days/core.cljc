(ns clanhr.work-days.core
  "Calculates work days on two dates"
  (:require [clj-time.periodic :as p]
            [clj-time.predicates :as pr]
            [clj-time.coerce :as c]
            [clj-time.core :as t]))

(defn- absence-type
  "Gets the absence type"
  [absence]
  (or (:absence-type absence)
      (:type absence)))

(defn- build-date
  "Creates a joda date from string"
  [absence field]
  (assoc absence field (c/from-string (field absence))))

(defn build
  "Prepares an absence"
  [absence]
   (cond-> absence
     (nil? (:duration-type absence)) (assoc :duration-type "days")
     (string? (:start-date absence)) (build-date :start-date)
     (instance? java.sql.Date (:start-date absence)) (assoc :start-date (c/from-sql-date (:start-date absence)))
     (instance? java.sql.Date (:end-date absence)) (assoc :end-date (c/from-sql-date (:end-date absence)))
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

(defn regular-week-work-day?
  "True if the given day is a rest day of the week"
  [settings day]
  (if-let [days-off (get-in settings [:work-days :days-off])]
    (let [week-day (t/day-of-week day)]
      (not (some #{week-day} days-off)))
    (pr/weekday? day)))

(defn holiday-match?
  "True if the given day matches the given holiday-data"
  [holiday-data day]
  (let [holiday (c/from-string (:day holiday-data))]
    (and (= (t/day day) (t/day holiday))
         (= (t/month day) (t/month holiday))
         (or (:recur holiday-data)
             (= (t/year day) (t/year holiday))))))

(defn holiday?
  "True if the given day is a holiday"
  [settings day]
  (when-let [holidays (get-in settings [:work-days :holidays])]
    (some #(holiday-match? % day) holidays)))

(defn work-on-holidays?
  "True if the settings state the a holiday is a work day"
  [settings]
  (get-in settings [:work-days :work-on-holidays]))

(defn work-day?
  "True if the given day is a work day, based on the settings"
  [settings day]
  (and (regular-week-work-day? settings day)
       (or (work-on-holidays? settings)
           (not (holiday? settings day)))))

(defn days-interval-remove-dayoff
  "Gets the number of days between start-date and end-date, removing the
  dayoffs defined on the settings"
  [settings absence]
  (->> (absence->days-coll settings absence)
       (filter (fn [day]  (work-day? settings day)))
       count))

(defn get-absence-config
  "Gets a specific absence config, for an absence, from a collection of
  absence-configs"
  [absence-configs absence]
  (-> (filter #(= (absence-type absence) (:label %))
              absence-configs)
      first))

(defn consider-absence-days-off?
  "True if it can find settings for this absence type, and that settings
  have consider-days-off not false"
  [settings absence]
  (when-let [absences-config (:absences settings)]
    (when-let [absence-config (get-absence-config absences-config absence)]
      (:consider-days-off absence-config))))

(defn remove-days-off?
  "True if, by the settings, we should remove days off from counting"
  [settings absence]
  (or (= "vacations" (absence-type absence))
      (consider-absence-days-off? settings absence)))

(defn total-vacation-days
  "Gets the total vacation days on this absence"
  [settings absence]
  (let [absence (build absence)]
    (if (= "vacations" (absence-type absence))
      (if (= (:duration-type absence) "partial-day")
        (:partial-day absence)
        (days-interval-remove-dayoff settings absence))
      0)))

(defn total-absence-hours
  "Gets the total vacation days on this absence"
  [settings absence]
  (let [absence (build absence)]
    (if (not= "vacations" (absence-type absence))
      (if (= "days" (:duration-type absence))
        (if (remove-days-off? settings absence)
          (* (hours-per-day settings) (days-interval-remove-dayoff settings absence))
          (* (hours-per-day settings) (days-interval settings absence)))
        (:hours absence))
      0)))

(defn calculate
  "Gets the raw duration of the absence, in days or hours, depending on the
  duration type"
  ([absence]
   (calculate {} absence))
  ([settings absence]
   (let [absence (build absence)]
     (cond (= (:duration-type absence) "days")
           (if (remove-days-off? settings absence)
             (days-interval-remove-dayoff settings absence)
             (days-interval settings absence))

           (= (:duration-type absence) "partial-day")
           (:partial-day absence)

          :else
          (:hours absence)))))
