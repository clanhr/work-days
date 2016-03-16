(ns clanhr.work-days.core-test
  (:require [clojure.test :refer :all]
            [clanhr.work-days.core :as work-days]
            [clj-time.core :as t]))

(deftest default-week-vacation-days
  (let [absence {:start-date "2015-11-09"
                 :end-date "2015-11-13"
                 :absence-type "vacations"}]

    (testing "default duration with 5 days"
      (is (= 5 (work-days/calculate {} absence)))
      (is (= 5 (work-days/total-vacation-days {} absence))))

    (testing "should count weekend for non-vacation absences"
      (let [absence (assoc absence :end-date "2015-11-15"
                                   :absence-type "family")]
        (is (= 7 (work-days/calculate {} absence)))))

    (testing "should not count weekend for absences"
      (let [absence (assoc absence :end-date "2015-11-15")]
        (is (= 5 (work-days/calculate {} absence)))
        (is (= 5 (work-days/total-vacation-days {} absence)))))))

(deftest speficic-consider-days-off-config
  (let [absence {:start-date "2015-11-09"
                 :end-date "2015-11-15"
                 :absence-type "sample-type"}
        settings-on {:work-days {:days-off [6 7]}
                     :absences [{:label "sample-type" :consider-days-off true}]}
        settings-off {:work-days {:days-off [6 7]}
                      :absences [{:label "sample-type" :consider-days-off false}]}
        settings-default {:work-days {:days-off [6 7]}
                          :absences [{:label "sample-type"}]}]

    (testing "should consider 5 work days and disrefard weekends"
      (is (= 5 (work-days/calculate settings-on absence)))
      (is (= 0 (work-days/total-vacation-days settings-on absence))))

    (testing "should consider 5 work days and disrefard weekends, by default"
      (is (= 7 (work-days/calculate settings-default absence)))
      (is (= 0 (work-days/total-vacation-days settings-default absence))))

    (testing "should consider 7 work days, because of consider-days-off = false"
      (is (= 7 (work-days/calculate settings-off absence)))
      (is (= 0 (work-days/total-vacation-days settings-off absence))))))

(deftest full-week-work-days
  (let [absence {:start-date "2015-11-09"
                 :end-date "2015-11-13"
                 :absence-type "vacations"}
        settings {:work-days {:days-off []}}]

    (testing "default duration with 5 days"
      (is (= 5 (work-days/calculate settings absence)))
      (is (= 5 (work-days/total-vacation-days settings absence))))

    (testing "should count weekend for non-vacation absences"
      (let [absence (assoc absence :end-date "2015-11-15"
                                   :absence-type "family")]
        (is (= 7 (work-days/calculate settings absence)))
        (is (= 0 (work-days/total-vacation-days settings absence)))))

    (testing "should count weekend for absences"
      (let [absence (assoc absence :end-date "2015-11-15")]
        (is (= 7 (work-days/calculate settings absence)))
        (is (= 7 (work-days/total-vacation-days settings absence)))))))

(def monday (t/date-time 2016 2 1))
(def tuesday (t/date-time 2016 2 2))
(def wednesday (t/date-time 2016 2 3))
(def thursday (t/date-time 2016 2 4))
(def friday (t/date-time 2016 2 5))
(def saturday (t/date-time 2016 2 6))
(def sunday (t/date-time 2016 2 7))

(deftest work-day-test?
  (testing "default work week"
    (is (true? (work-days/work-day? {} monday)))
    (is (true? (work-days/work-day? {} tuesday)))
    (is (true? (work-days/work-day? {} wednesday)))
    (is (true? (work-days/work-day? {} thursday)))
    (is (true? (work-days/work-day? {} friday)))
    (is (false? (work-days/work-day? {} saturday)))
    (is (false? (work-days/work-day? {} sunday))))

  (testing "full work week"
    (let [settings {:work-days {:days-off []}}]
      (is (true? (work-days/work-day? settings monday)))
      (is (true? (work-days/work-day? settings tuesday)))
      (is (true? (work-days/work-day? settings wednesday)))
      (is (true? (work-days/work-day? settings thursday)))
      (is (true? (work-days/work-day? settings friday)))
      (is (true? (work-days/work-day? settings saturday)))
      (is (true? (work-days/work-day? settings sunday)))))

  (testing "rest on mondays"
    (let [settings {:work-days {:days-off [1]}}]
      (is (false? (work-days/work-day? settings monday)))
      (is (true? (work-days/work-day? settings tuesday)))
      (is (true? (work-days/work-day? settings wednesday)))
      (is (true? (work-days/work-day? settings thursday)))
      (is (true? (work-days/work-day? settings friday)))
      (is (true? (work-days/work-day? settings saturday)))
      (is (true? (work-days/work-day? settings sunday))))))

(deftest holidays
  (let [absence {:start-date "2016-01-01"
                 :end-date "2016-01-02"
                 :absence-type "vacations"}
        settings {:work-days {:days-off []
                              :holidays [{:name "New Year"
                                          :day "2016-01-01"
                                          :recur true}]}}]

    (testing "should not count holiday"
      (is (= 1 (work-days/calculate settings absence)))
      (is (= 1 (work-days/total-vacation-days settings absence))))

    (testing "should count holiday from other year"
      (let [absence {:start-date "2017-01-01"
                     :end-date "2017-01-02"
                     :absence-type "vacations"}]
      (is (= 1 (work-days/calculate settings absence)))
      (is (= 1 (work-days/total-vacation-days settings absence)))))))

(deftest holidays-as-regular-days
  (let [absence {:start-date "2016-01-01"
                 :end-date "2016-01-02"
                 :absence-type "vacations"}
        settings {:work-days {:days-off []
                              :work-on-holidays true
                              :holidays [{:name "New Year"
                                          :day "2016-01-01"
                                          :recur true}]}}]

    (testing "should not count holiday"
      (is (= 2 (work-days/calculate settings absence)))
      (is (= 2 (work-days/total-vacation-days settings absence))))))

(deftest count-absences-by-settings
  (let [absence {:start-date "2016-01-01"
                 :end-date "2016-01-02"
                 :absence-type "something"}
        settings {:absences [{:label "something" :consider-days-off true}]
                  :work-days {:days-off []
                              :holidays [{:name "New Year"
                                          :day "2016-01-01"
                                          :recur true}]}}]

    (testing "should count holiday"
      (is (= 8 (work-days/total-absence-hours settings absence))))))

(deftest half-vacation-days
  (let [absence {:start-date "2015-11-09"
                 :end-date "2015-11-09"
                 :absence-type "vacations"
                 :duration-type "partial-day"
                 :partial-day 0.5}]

    (testing "should consider half days"
      (is (= 0.5 (work-days/calculate {} absence)))
      (is (= 0.5 (work-days/total-vacation-days {} absence))))))

