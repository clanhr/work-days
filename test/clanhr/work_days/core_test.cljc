(ns clanhr.work-days.core-test
  (:require [clojure.test :refer :all]
            [clanhr.work-days.core :as work-days]))

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
