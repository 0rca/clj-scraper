(ns scraper.ui
  (:require [clojure.java.io :as io]))

(defn- tab-format [n pat v]
  (apply format (apply str "\r" (take n (repeat pat))) v))

(defn- stats-line-numeric [v]
  (tab-format (count v) "%-12s" v))

(defn- stats-line-string [v]
  (tab-format (count v) "%-12s" v))


(defn stats-headers []
  (stats-line-string [
                      "cache"
                      "dlque"
                      "dlact"
                      "compl"
                      "error"
                      "miss"
                      "history"
                      "space"
                      "file"]))

(defn- kbytes [b]
  (int (/ b 1024)))

(defn- gbytes [b]
  (-> b kbytes kbytes kbytes))

(defn- stats-array [m]
  (let [{
         pc :cached-pages
         c  :completed
         e  :errors
         mi :missing
         h  :in-history
         pool :pool
         f  :last-file} m
         cs (count (:page-cache m))
         s  (.getUsableSpace (io/file "."))
         dlq  (-> pool .getQueue .size)
         dla  (-> pool .getActiveCount)]
    [cs dlq dla c e mi h (str (gbytes s) "G") (str f " ")]))

(defn stats-map [m]
  (zipmap [:cached :download-queued :download-active :completed :errors :missing :history :disk-space :last-file]
          (stats-array m)))

(defn stats-str [m]
  (stats-line-numeric (stats-array m)))

(defn print-stats [m]
  (do
    (print (stats-str m))
    (flush)))
