(ns scraper.core-test
  (:require [clojure.test :refer :all]
            [scraper.fs   :refer :all]
            [scraper.core :refer :all]))

(deftest filename-v*
    (testing "v3 filename"
      (is (= (filename-v3 "http://s017.radikal.ru/i413/1312/6b/3dd009003d85.jpg" "Test" "2013-12-20" 3 "images")
             "images/T/Test/2013-12-20/003-90298005.jpg"))))

(deftest convert-date-*
  (testing "should convert to YYYY-MM-DD"
    (is (= (convert-date "[Jan. 13th, 2014|07:00 am]")
           "2014-01-13-0700"))))

(deftest test-3
  (testing "jpeg?"
    (is (= true (jpeg? "http://example.com/img/123.jpg")))
    (is (= true (jpeg? "http://example.com/img/123.jpeg")))
    (is (= true (jpeg? "http://example.com/img/123.JPG")))
    (is (= false (jpeg? "http://example.com/img/123.jpgg")))))
