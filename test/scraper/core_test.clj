(ns scraper.core-test
  (:require [clojure.test :refer :all]
            [scraper.core :refer :all]))

(deftest filename-v*
  (binding [*images-dir* "images"]
    (testing "v2 filename"
      (is (= (filename-v2 "http://s017.radikal.ru/i413/1312/6b/3dd009003d85.jpg" "Test" "2013-12-20" 3)
             "images/Test/3-i41313126b3dd009003d85.jpg")))
    (testing "v3 filename"
      (is (= (filename-v3 "http://s017.radikal.ru/i413/1312/6b/3dd009003d85.jpg" "Test" "2013-12-20" 3)
             "images/Test/2013-12-20/003-90298005.jpg")))
    (testing "v1 filename"
      (is (= (filename-v1 "http://s017.radikal.ru/i413/1312/6b/3dd009003d85.jpg" "Test" "2013-12-20" 3)
              "images/Test/3dd009003d85.jpg")))))

(deftest convert-date-*
  (testing "should convert to YYYY-MM-DD"
    (is (= (convert-date "[Jan. 13th, 2014|07:00 am]")
           "2014-01-13"))))

(deftest test-1
  (testing "next?"
    (is (= true (next? "http://lj.rossia.org/users/vrotmnen0gi/?skip=20")))))

(deftest test-2
  (testing "post?"
    (is (= true (post? "http://lj.rossia.org/users/vrotmnen0gi/993483.html")))))

(deftest test-3
  (testing "jpeg?"
    (is (= true (jpeg? "http://example.com/img/123.jpg")))
    (is (= true (jpeg? "http://example.com/img/123.jpeg")))
    (is (= true (jpeg? "http://example.com/img/123.JPG")))
    (is (= false (jpeg? "http://example.com/img/123.jpgg")))))