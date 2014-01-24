(ns scraper.fs
  (:require [clojure.java.io :as io])
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

(defn exists?
  "detects if a filename exists on local fs"
  [f]
  (.exists (io/file f)))

(defn make-dir [& dirs]
  (doseq [v dirs]
    (let [dir (io/file v)]
      (when-not (.exists dir)
        (.mkdirs dir)))))

(defn path [filename]
  (.getParent (io/file filename)))

(defn rename [f1 f2]
  (make-dir (path f2))
  (.renameTo (io/file f1) (io/file f2)))

(defn filename-from [url]
  (.getName (io/file url)))

(defn filename-v3 [url title date index img-dir]
  (format "%s/%s/%s/%s/%03d-%s.%s"
          img-dir
          (first title)
          title
          date
          index
          (apply str (take 8 (md5 url)))
          (last (str/split url #"\."))))
