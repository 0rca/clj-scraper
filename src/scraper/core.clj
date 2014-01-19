(ns scraper.core
  (:require [net.cgrand.enlive-html :as html])
  (:require [org.httpkit.client     :as http])
  (:require [clojure.java.io        :as io])
  (:require [clojure.string         :as str])
  (:require [clj-time.format        :as tf ])
  (:require [digest                 :refer [md5]])
  (:require [scraper.fs :as fs])
  (:require [clojure.java.shell    :refer [sh]])
  (:import  [java.util.concurrent   Executors])
  (:gen-class))

(def ^:dynamic *pool*    (Executors/newFixedThreadPool 4))
(def ^:dynamic *debug* false)
(def ^:dynamic *cache-dir* "cache")
(def ^:dynamic *images-dir* "images")
(def ^:dynamic *base-url* "")
(def ^:dynamic *next-page-fn* (fn []))

(defn wget [& args]
  (apply sh "wget" args))

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
                  :timeouts 0
                  :new-pages 0
                  :exceptions []
                  :last-file  ""
                  :history #{}
                  :url-count 0
                  }))

(declare print-stats)

(defn inc-counter [c]
  (swap! state update-in [c] inc))

(defn dec-counter [c]
  (swap! state update-in [c] dec))

(defmacro debug [body]
  `(when *debug* (~@body)))

;; (defn download-from [url]
;;   (let [resp (http/get url {:as :byte-array :timeout  120000 :keepalive 120000})]
;;     (when (= 200 (:status @resp))
;;       (:body @resp))))

;; (defn write-byte-array-to [fname b]
;;   (with-open [out (io/output-stream (io/file fname))]
;;     (.write out b)))

;; (defn write-file [f stream]
;;    (with-open [w (clojure.java.io/output-stream f)]
;;      (.write w (:body stream))))

(defn fetch-url [url]
  (debug (println "Fetching " url))
  (let [resp (http/get url {:as :stream :keepalive 12000})]
    (html/html-resource (:body @resp))))

(defn fetch-cached-url [url]
  (debug (println "Fetching" url "from cache"))
  (let [file (last (str/split url #"/"))
        path (str *cache-dir* "/" file)]
    (if (.exists (io/file path))
      (do
        (debug (println "HIT"))
        (inc-counter :cached-pages)
        (html/html-resource (io/file path)))
      (do
        (debug (println "MISS: downloading"))
        (inc-counter :new-pages)
        (let [{exit :exit} (wget "-c" url (str "-P" *cache-dir*))]
          (if (zero? exit)
            (html/html-resource (io/file path))
            (inc-counter :errors)))))))

(defn sanitize-path [path]
  (str/replace path #"\s*блеать\s*|#" ""))


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

;; general utility function
(defn page-seq
  "Returns lazy sequence of pages, paginated by next-page function"
  [url page-fn]
  (lazy-seq
    (when-not (or (nil? (:page url))
                  (nil? url))
      (cons url (page-seq (page-fn url) page-fn)))))

;; general utility function
(defn tag-all-with [xs tag]
  (map #(apply hash-map %)
        (partition 2
                  (interleave (iterate identity tag) xs))))

;; site-specific
(defn posts-for [url]
  (when (map? url)
    (let [page (fetch-url (url :page))]
      (inc-counter :blog-pages)
      (tag-all-with (find-all-by-text page "Link") :post))))

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
  (page-seq {:page *base-url*} *next-page-fn*))

(defn lj-posts []
  (post-seq (lj-pages) posts-for))

(defn lj-images []
  (mapcat image-seq (lj-posts)))


(defn remote-mime? [type url]
  (debug (println "."))
  (let [resp (http/head url {:keepalive 600000})]
    (and
     (= 200  (:status @resp))
     (= type (get-in @resp [:headers :content-type])))))


(def remote-jpeg? (partial remote-mime? "image/jpeg"))


(defn still-exists? [url]
   (remote-jpeg? url))

;; Site-specific
(defn convert-date [date]
  (let [[_ month day _ year] (first (re-seq #"\[(\w{3})\.\s(\d{1,2})(th|nd|st|rd),\s(\d{4})" date))
        temp-date (str year " " month " " day)
        temp-formatter (tf/formatter "YY MMM DD")
        date (tf/parse temp-formatter temp-date)]
    (tf/unparse (tf/formatters :date) date)))

;; UI
(defn- tab-format [n pat v]
  (apply format (apply str "\r" (take n (repeat pat))) v))

(defn- stats-line-numeric [v]
  (tab-format (count v) "%-10s" v))

(defn- stats-line-string [v]
  (tab-format (count v) "%-10s" v))


(defn- stats-headers []
  (stats-line-string ["pages"
                      "postn"
                      "postc"
                      "dlque"
                      "dlact"
                      "compl"
                      "error"
                      "miss"
                      "timeouts"
                      "renam"
                      "exist"
                      "disk"
                      "total"
                      "urls"
                      "file"]))

(defn print-stats [m]
  (let [{pg :blog-pages
         pn :new-pages
         pc :cached-pages
         c  :completed
         e  :errors
         mi :missing
         un :timeouts
         r  :renamed
         u  :url-count
         i  :exists } m
         dlq  (-> *pool* .getQueue .size)
         dla  (-> *pool* .getActiveCount)
        f (:last-file @state)]
    (do
      ;(print (str "\r" @state))
      (print (stats-line-numeric [pg pn pc dlq dla c e mi un r i (+ r i) (+ c r i) u f]))
      (flush))))
;; EO UI

;; (defn download-to [src fname-v3]
;;   (fs/make-dir (fs/path fname-v3))
;;   (try
;;     (let [resp (http/get src {:as :byte-array :timeout 300000})]
;;       (if (= 200 (:status @resp))
;;         (do (write-byte-array-to fname-v3 (:body @resp))
;;             (inc-counter :completed)
;;             (swap! state assoc :last-file (fs/short-name fname-v3)))
;;         (println (:error @resp))))
;;     (catch Exception e #(println e))))

(defn download-to-v2 [src fname-v3]
  (let [{:keys [exit err out] } (wget "-c" src "-Ptmp")]
    (when (zero? exit)
      (let [dir (fs/path fname-v3)
            source (str "tmp/" (fs/filename-from src))
            dest   fname-v3]
        (fs/make-dir dir)
        (fs/rename source dest)
        (inc-counter :completed)
        (swap! state assoc :last-file (fs/short-name fname-v3))))))

(defn- write-history []
  (with-open [w (-> "history.txt.gz"
                    io/output-stream
                    java.util.zip.GZIPOutputStream.
                    io/writer)]
    (binding [*out* w]
      (doseq [f (:history @state)]
        (println (.getPath f))))))

(defn- read-history []
  (with-open [in (java.util.zip.GZIPInputStream.
                 (io/input-stream "history.txt.gz"))]
    (map io/file (str/split (slurp in) #"\n"))))

(defn- init-history-from-file []
  (swap! state update-in [:history] into (read-history)))

(defn- init-history-from-filesystem []
  (swap! state update-in [:history] into
         (filter #(.isFile %) (file-seq (io/file *images-dir*)))))



;  (swap! state update-in [:history] into (read-string (slurp "history.clj"))))

(defn -main []
;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread. (fn [] (println "Shutting down..."))))

  (binding [*debug* false
            *base-url* "http://lj.rossia.org/users/vrotmnen0gi/"
            *next-page-fn* prev-page
            ;*images-dir* "/Users/orca/Downloads/Фото - вротмненоги"
            ]
    (println "About"(count (:history @state)) "files already downloaded")

    (fs/make-dir *images-dir* *cache-dir*)

    (println (stats-headers))

    ;; UI update thread
    (.start (Thread. #(while true
                        (print-stats @state)
                        (Thread/sleep 500))))

    (doseq [image-src (lj-images)]
      (let [title   (str/trim (str/replace (:title image-src) #"[:/\\]" "_"))
            src     (:img   image-src)
            index   (:index image-src)
            date     (convert-date (:date  image-src))
            dir      (str *images-dir* "/" title)
            fname-v1 (fs/filename-v1 src title date index *images-dir*)
            fname-v2 (fs/filename-v2 src title date index *images-dir*)
            fname-v3 (sanitize-path (fs/filename-v3 src title date index *images-dir*))
            fname-v0 (fs/filename-v0 src title date index *images-dir*)]

        (cond
         (fs/exists? fname-v0)
         (do
           (fs/rename fname-v0 fname-v3)
           (inc-counter :renamed))

         (fs/exists? fname-v1)
         (do
           (fs/rename fname-v1 fname-v3)
           (inc-counter :renamed))

         (fs/exists? fname-v2)
         (do
           (fs/rename fname-v2 fname-v3)
           (inc-counter :renamed))

         (fs/exists? fname-v3)
         (inc-counter :exists)

         :else
         (let [host (fs/hostname src)
               _    (swap! state update-in [:url-count] inc)]
           (if (still-exists? src)
             (.submit *pool* (partial download-to-v2 src fname-v3))
             (do
               (inc-counter :missing)
               (swap! state assoc :last-file src)))))))))
