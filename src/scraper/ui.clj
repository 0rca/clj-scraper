(ns scraper.ui
  (:require [clojure.java.io :as io]))

(def ui-state (ref {:stats {}
                    :count 0}))

(defn- tab-format [n pat v]
  (apply format (apply str "\r" (take n (repeat pat))) v))

(defn- stats-line-numeric [v]
  (tab-format (count v) "%-12s" v))

(defn- stats-line-string [v]
  (tab-format (count v) "%-12s" v))


(defn stats-headers []
  (stats-line-string [
                      "pages"
                      "dlque"
                      "dlact"
                      "compl"
                      "error"
                      "miss"
                      "renamed"
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
         r  :renamed
         f  :last-file} m
         cs (count (:page-cache m))
         s  (.getUsableSpace (io/file "."))
         dlq  (-> pool .getQueue .size)
         dla  (-> pool .getActiveCount)]
    [cs dlq dla c e mi r h (str (gbytes s) "G") (str f " ")]))

(defn stats-map [m]
  (zipmap [:cached :download-queued :download-active :completed :errors :missing :renamed :history :disk-space :last-file]
          (stats-array m)))

(defn stats-str [m]
  (stats-line-numeric (stats-array m)))

(defn print-headers []
  (println (stats-headers)))

(defn print-stats [m]
  (when (not (= m (:stats @ui-state)))
    (dosync
      (commute ui-state assoc :stats m)
      (commute ui-state update-in [:count] inc))))

(add-watch ui-state :key (fn [_ _ _ new-state]
                           (when (zero? (rem (:count new-state) 20))
                             (println)
                             (print-headers))
                           (println (stats-str (:stats new-state)))
                           (flush)))

