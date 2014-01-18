(ns scraper.core
  (:require [net.cgrand.enlive-html :as html])
  (:require [clj-http.lite.client   :as client])
  (:require [clojure.java.io        :as io])
  (:require [clojure.string         :as str])
  (:require [clj-time.format        :as tf ])
  (:require [digest                 :refer [md5]])
  (:import  [java.util.concurrent   Executors])
  (:gen-class))

(def ^:dynamic *pool*    (Executors/newFixedThreadPool 2))
;(def ^:dynamic *dl-pool* (Executors/newFixedThreadPool 2))
(def ^:dynamic *debug* false)
(def ^:dynamic *cache-dir* "cache")
(def ^:dynamic *images-dir* "images")
(def ^:dynamic *base-url* "http://lj.rossia.org/users/vrotmnen0gi/?skip=4200")

(def state (atom {
                  :state :running
                  :renamed 0
                  :pending 0
                  :completed 0
                  :errors 0
                  :exists 0
                  :blog-pages 0
                  :cached-pages 0
                  :missing 0
                  :unauth 0
                  :new-pages 0
                  :exceptions []
                  }))

(declare print-stats)

(defn inc-counter [c]
  (swap! state update-in [c] inc))

(defn dec-counter [c]
  (swap! state update-in [c] dec))

(defmacro debug [body]
  `(when *debug* (~@body)))

(defn download-from [url]
  (client/get url {:as :byte-array}))

(defn write-file [f stream]
   (with-open [w (clojure.java.io/output-stream f)]
     (.write w (:body stream))))

(defn fetch-url [url]
  (debug (println "Fetching " url))
  (html/html-resource (java.net.URL. url)))

(defn fetch-cached-url [url]
  (debug (println "Fetching" url "from cache"))
  (let [file (last (str/split url #"/"))
        path (str *cache-dir* "/" file)]
    (if (.exists (io/file path))
      (do
        (debug (println "HIT"))
        (inc-counter :cached-pages))
      (do
        (debug (println "MISS: downloading"))
        (inc-counter :new-pages)
        (write-file path (download-from url))
        (debug (println "OK"))))

    (html/html-resource (io/file path))))


(defn links-all [page]
  (map #(:href (:attrs %))
    (html/select page [:a])))

;; post with tests
(defn post? [link]
  (let [re-post #"http://lj.rossia.org/users/vrotmnen0gi/[0-9]+.html"]
    (seq? (re-seq re-post link))))

;; next page link?
(defn next? [link]
  (let [re-next #"http://lj.rossia.org/users/vrotmnen0gi/\?skip=[0-9]+"]
    (seq? (re-seq re-next link))))

(defn posts [page]
  (filter post? (links-all page)))

(defn jpeg? [url]
  (let [ext (last (str/split url #"\."))]
    (seq? (re-seq #"jpe?g$" (str/lower-case ext)))))

(defn find-all-by-text [page text]
  (map #(-> % :attrs :href)
        (filter #(= text (-> % :content first))
                 (html/select page [:a]))))

(defn find-by-text [page text]
  (first (find-all-by-text page text)))

;; site-specific stuff
(defn prev-page
  "Returns next page url for url given, or nil if none found"
  [url]
  (when (map? url)
    (let [page (fetch-url (url :page))]
      {:page (find-by-text page "earlier")})))

(defn next-page
  "Returns next page url for url given, or nil if none found"
  [url]
  (when-not (nil? (:page url))
    (let [page (fetch-url (url :page))]
      {:page (find-by-text page "later")})))

(defn page-seq
  "Returns lazy sequence of pages, paginated by next-page function"
  [url page-fn]
  (lazy-seq
    (when-not (or (nil? (:page url))
                  (nil? url))
      (cons url (page-seq (page-fn url) page-fn)))))

(defn tag-all-with [xs tag]
  (map #(apply hash-map %)
        (partition 2
                  (interleave (iterate identity tag) xs))))

(defn posts-for [url]
  (when (map? url)
    (let [page (fetch-url (url :page))]
      (inc-counter :blog-pages)
      (tag-all-with (find-all-by-text page "Link") :post))))

(defn- filename-from [url]
  (last (str/split url #"/")))

(defn filename-v0 [url _ _ _]
  (str *images-dir* "/__unsorted/" (filename-from url)))

(defn filename-v1 [url title date index]
  (str *images-dir* "/"
       title "/"
       (filename-from url)))

(defn filename-v2 [url title date index]
  (apply str
         *images-dir* "/"
         title "/"
         index "-"
         (drop 3 (str/split url #"/"))))

(defn filename-v3 [url title date index]
  (format "%s/%s/%s/%03d-%s.%s"
          *images-dir*
          title
          date
          index
          (apply str (take 8 (md5 url)))
          (last (str/split url #"\."))))

(defn post-seq [pages posts-fn]
  (lazy-seq
    (when (seq? pages)
      (concat (posts-fn (first pages)) (post-seq (rest pages) posts-fn)))))

(defn image-seq [url]
  (let [page    (fetch-cached-url (url :post))
        links   (html/select page [:a])
        hrefs   (filter #(not (nil? %)) (map #(get-in % [:attrs :href]) links))
        jpegs   (filter jpeg? hrefs)
        date    (html/text (last (html/select page [:td.index])))
        title   (html/text (last (html/select page [:td.caption])))]
    (map #(merge {:date  date
                  :title title
                  :index %1} %2)
          (range)
          (tag-all-with jpegs :img))))

;; Site-specific stuff
(defn lj-pages []
  (page-seq {:page *base-url*} next-page))

(defn lj-posts []
  (post-seq (lj-pages) posts-for))

(defn lj-images []
  (mapcat image-seq (lj-posts)))

(defn make-dir [& dirs]
  (doseq [v dirs]
    (let [dir (io/file v)]
      (when-not (.exists dir)
        (.mkdirs dir)))))

(defn path-from [filename]
  (str/join "/" (butlast (str/split filename #"/"))))

(defn do-rename [f1 f2]
  (make-dir (path-from f2))
  (.renameTo (io/file f1) (io/file f2)))

(defn exists? [f]
  (.exists (io/file f)))

(defn remote-mime? [type url]
  (debug (println "."))
  (= type (get-in (client/head url) [:headers "content-type"])))

(def remote-jpeg? (partial remote-mime? "image/jpeg"))

(defn still-exists? [url]
  (and
   (not (seq? (re-seq #"fastpic\.ru" url)))))
;   (remote-jpeg? url)))

(defn convert-date [date]
  (let [[_ month day _ year] (first (re-seq #"\[(\w{3})\.\s(\d{1,2})(th|nd|st|rd),\s(\d{4})" date))
        temp-date (str year " " month " " day)
        temp-formatter (tf/formatter "YY MMM DD")
        date (tf/parse temp-formatter temp-date)]
    (tf/unparse (tf/formatters :date) date)))

(defn- tab-format [n pat v]
  (apply format (apply str "\r" (take n (repeat pat))) v))

(defn- stats-line-numeric [v]
  (tab-format 14 "%-10s" v))

(defn- stats-line-string [v]
  (tab-format 14 "%-10s" v))


(defn- stats-headers []
  (stats-line-string ["pages"
                      "postn"
                      "postc"
                      "dlque"
                      "dlact"
                      "compl"
                      "error"
                      "miss"
                      "unauth"
                      "renam"
                      "exist"
                      "disk"
                      "total"
                      "exc"]))

(defn print-stats [m]
  (let [{pg :blog-pages
         pn :new-pages
         pc :cached-pages
         c  :completed
         e  :errors
         mi :missing
         un :unauth
         r  :renamed
         i  :exists } m
         dlq  (-> *pool* .getQueue .size)
         dla  (-> *pool* .getActiveCount)
         ex (:exceptions @state)]
    (do
      ;(print (str "\r" @state))
      (print (stats-line-numeric [pg pn pc dlq dla c e mi un r i (+ r i) (+ c r i) ex]))
      (flush))))

(defn download-to [src fname-v3]
  (try
    (make-dir (path-from fname-v3))
    (write-file fname-v3 (download-from src))
    (inc-counter :completed)
    (catch Exception e (condp = (get-in e [:object :status])
                         404 (inc-counter :missing)
                         403 (inc-counter :unauth)
                         (do
                           ;(swap! state update-in [:exceptions] conj e)
                           (inc-counter :errors))))))

(defn -main []
;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (binding [*debug* false
            *images-dir* "/Users/orca/Downloads/Фото - вротмненоги"]
    (make-dir *images-dir* *cache-dir*)

    (println (stats-headers))

    ;; UI update thread
    (.start (Thread. #(while true
                        (print-stats @state)
                        (Thread/sleep 250))))

    (doseq [image-src (lj-images)]
      (let [title   (str/trim (str/replace (:title image-src) #"[:/\\]" "_"))
            src     (:img   image-src)
            index   (:index image-src)
            date     (convert-date (:date  image-src))
            dir      (str *images-dir* "/" title)
            fname-v1 (filename-v1 src title date index)
            fname-v2 (filename-v2 src title date index)
            fname-v3 (filename-v3 src title date index)
            fname-v0 (filename-v0 src title date index)]
        (cond
         (exists? fname-v0)
         (do
           (do-rename fname-v0 fname-v3)
           (inc-counter :renamed))

         (exists? fname-v1)
         (do
           (do-rename fname-v1 fname-v3)
           (inc-counter :renamed))

         (exists? fname-v2)
         (do
           (do-rename fname-v2 fname-v3)
           (inc-counter :renamed))

         (exists? fname-v3)
         (inc-counter :exists)

         :else
         (do
           (inc-counter :pending)
           (if (still-exists? src)
             (.submit *pool* (partial download-to src fname-v3))
             (inc-counter :missing))))))))
;;            (.submit *pool*
;;                     #(try
;;                        (dec-counter :pending)
;;                        (if (still-exists? src)
;;                          (.submit *dl-pool* (partial download-to src fname-v3))
;;                          (inc-counter :missing))
;;                        (catch Exception e (inc-counter :missing))))))))))
