(ns scraper.fs
  (:require [clojure.java.io :as io])
  (:require [me.raynes.fs :as fs])
  (:require [clojure.string  :as str])
  (:require [digest :refer [md5]]))


(defn hostname [url]
  "returns hostname of the url"
  (second (first (re-seq #"http://(\w*\.?\w*\.?\w+\.\w+)" url))))

(defn short-name
  "returns dir/filename.ext"
  [fname]
  (str/join "/"
            (take-last 3 (str/split fname #"/"))))

(defn path [filename]
  (.getParent (io/file filename)))

(defn rename [f1 f2]
  (fs/mkdirs (path f2))
  (fs/rename f1 f2))

(defn filename-from [url]
  (.getName (io/file url)))

(defn cached-name [url dir]
  (format "%s/%s" dir (apply str (take 16 (md5 url)))))

(defn mangled-filename [index url]
  (format "%03d-%s.%s"
          index
          (apply str (take 8 (md5 url)))
          (last (str/split url #"\."))))

(defn filename-pussy-portraits [url _ date index img-dir]
  (format "%s/%s/%s/%s"
          img-dir
          "Pussy Portraits"
          date
          (mangled-filename index url)))

(defn filename-devil-dozen [url _ date index img-dir]
  (format "%s/%s/%s/%s"
          img-dir
          "Чертова дюжина"
          date
          (mangled-filename index url)))

(defn filename-comment-statistics [url _ date index img-dir]
  (format "%s/%s/%s"
          img-dir
          "Несколько дв"
          (mangled-filename index url)))

(defn filename-v3 [url title date index img-dir]
  (format "%s/%s/%s/%s/%s"
          img-dir
          (first title)
          title
          date
          (mangled-filename index url)))
