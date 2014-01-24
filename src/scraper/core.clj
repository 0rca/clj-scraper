(ns scraper.core
  (:require [net.cgrand.enlive-html :as html])
  (:require [org.httpkit.client     :as http])
  (:require [clojure.java.io        :as io])
  (:require [clojure.string         :as str])
  (:require [clj-time.format        :as tf ])
  (:require [digest                 :refer [md5]])
  (:require [scraper.fs :as fs])
  (:require [clojure.java.shell    :refer [sh]])
  (:require [clojure.tools.cli     :refer [cli]])
  (:import  [java.util.concurrent   Executors])
  (:gen-class))

(def ^:dynamic *pool*)
(def ^:dynamic *debug* false)
(def ^:dynamic *cache-dir*)
(def ^:dynamic *images-dir*)
(def ^:dynamic *base-url* "http://lj.rossia.org/users/vrotmnen0gi/")
(def ^:dynamic *next-page-fn*)
(def ^:dynamic *use-cache*)

(defn wget [& args]
  (apply sh "wget" "-T 30" args))

(def state (atom {
                  :completed 0
                  :errors 0
                  :missing 0
                  :last-file  ""
                  :history {}
                  :url-count 0
                  :in-history 0
                  :page-cache {}
                  }))

(declare print-stats)

(defn inc-counter [c]
  (swap! state update-in [c] inc))

(defn dec-counter [c]
  (swap! state update-in [c] dec))

(defmacro debug [body]
  `(when *debug* (~@body)))

(defn cache-page! [url content]
  (swap! state update-in [:page-cache] assoc url content))

(defn cached-page [url]
  (get-in @state [:page-cache url]))

(defn cached? [url]
  (contains? (:page-cache @state) url))

(defn fetch-resource [url]
  (html/html-resource (java.net.URL. url)))

(defn fetch-maybe-cached-resource [url]
  (if (cached? url)
    (cached-page url)
    (when-let [content (fetch-resource url)]
      (cache-page! url content)
      content)))

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

(defn jpeg? [url]
  (let [ext (last (str/split url #"\."))]
    (seq? (re-seq #"jpe?g$" (str/lower-case ext)))))

(defn find-all-by-text [page text]
  (map #(-> % :attrs :href)
        (filter #(= text (-> % :content first))
                 (html/select page [:a]))))

(defn find-by-text [page text]
  (first (find-all-by-text page text)))


(defn next-by-text [text url]
  (when-not (nil? url)
    (when-let [content (fetch-maybe-cached-resource url)]
      (find-by-text content text))))


(defn convert-date
  ([fmt date]
   (let [[_ month day _ year hh mm] (first
                                     (re-seq
                                      #"\[(\w{3})\.\s(\d{1,2})(th|nd|st|rd),\s(\d{4})\|(\d{1,2}):(\d{1,2})"
                                      date))
         temp-date (str year " " month " " day " " hh ":" mm)
         temp-formatter (tf/formatter "YY MMM DD HH':'mm")
         date (tf/parse temp-formatter temp-date)]
     (tf/unparse fmt date)))
  ([date]
   (convert-date (tf/formatter "YYYY'-'MM'-'DD'-'HHmm") date)))

(defn find-all-jpeg-links [content]
  (filter jpeg?
          (map (fn [tag] (get-in tag [:attrs :href]))
               (html/select content [[:a (html/attr? :href)]]))))


(def vrotmne-prev (partial next-by-text "earlier"))

(def vrotmne-next (partial next-by-text "later"))

(defn vrotmne-posts [url]
  (let [content (fetch-maybe-cached-resource url)]
    (find-all-by-text content "Link")))


(defn vrotmne-images [url]
  (let [content (fetch-maybe-cached-resource url)
        jpegs   (find-all-jpeg-links content)
        date    (html/text (second (html/select content [:td.index])))
        title   (html/text (second (html/select content [:td.caption])))]
    (map #(array-map :index %1
                     :date (convert-date date)
                     :title (str/trim title)
                     :url %2)
         (range)
         jpegs)))

(defn remote-mime? [type url]
  (debug (println "."))
  (let [resp (http/head url {:keepalive 600000})]
    (and
     (= 200  (:status @resp))
     (= type (get-in @resp [:headers :content-type])))))


(def remote-jpeg? (partial remote-mime? "image/jpeg"))

(defn still-exists? [url]
   (remote-jpeg? url))

;; UI
(defn- tab-format [n pat v]
  (apply format (apply str "\r" (take n (repeat pat))) v))

(defn- stats-line-numeric [v]
  (tab-format (count v) "%-12s" v))

(defn- stats-line-string [v]
  (tab-format (count v) "%-12s" v))


(defn- stats-headers []
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

(defn print-stats [m]
  (let [{
         pc :cached-pages
         c  :completed
         e  :errors
         mi :missing
         h  :in-history
         pool :pool} m
         cs (count (:page-cache m))
         s  (.getUsableSpace (io/file "."))
         dlq  (-> pool .getQueue .size)
         dla  (-> pool .getActiveCount)
         f (:last-file @state)]
    (do
      (print (stats-line-numeric [cs dlq dla c e mi h (int (/ s (* 1024 1024 1024))) (str f " ")]))
      (flush))))
;; EO UI

(defn history-entry [file]
  (hash-map (.getName file) file))

(defn add-to-history [f]
  (swap! state update-in [:history] into (history-entry (io/file f))))

(defn download-to [src fname-v3]
  (let [{:keys [exit err out] } (wget "-c" src "-Ptmp")]
    (if (zero? exit)
      (let [dir (fs/path fname-v3)
            source (str "tmp/" (fs/filename-from src))
            dest   fname-v3]
        (fs/make-dir dir)
        (fs/rename source dest)
        (inc-counter :completed)
        (add-to-history dest)
        (swap! state assoc :last-file (fs/short-name fname-v3)))
      (inc-counter :errors))))

(defn- write-history []
  (with-open [w (-> "history.txt.gz"
                    io/output-stream
                    java.util.zip.GZIPOutputStream.
                    io/writer)]
    (binding [*out* w]
      (doseq [f (:history @state)]
        (println f)))))


(defn- read-history []
  (with-open [in (java.util.zip.GZIPInputStream.
                 (io/input-stream "history.txt.gz"))]
    (map #(history-entry (io/file %)) (str/split (slurp in) #"\n"))))


(defn- init-history-from-file []
  (swap! state update-in [:history] merge (read-history)))

(defn- init-history-from-filesystem []
  (swap! state update-in [:history] into
         (map history-entry (filter #(.isFile %) (file-seq (io/file *images-dir*))))))

(defn init-history []
  ;; (when (fs/exists? "history.txt.gz")
    ;; (init-history-from-file))
  (init-history-from-filesystem))


(defn in-history? [f]
  (get (@state :history) (.getName (io/file f))))

(def app-specs [["-s" "--skip" "Number of posts to skip"
                 :default 0 :parse-fn #(Integer. %)]
;;                 ["-r" "--reverse" "Reverse direction of scraping"
;;                  :default false :flag true]
                ["-c" "--cache" "Cache directory"
                 :default "cache"]
                ["-i" "--images" "Images directory"
                 :default "images"]
                ["-C" "--cache-only" "Use only cached posts only"
                 :default false]
                ["-w" "--workers" "Number of download workers"
                 :default 4 :parse-fn #(Integer. %)]
                ["-d" "--debug" "Display debug information"
                 :default false]
                ["-l" "--list-only" "Save url into list, instead"
                 :default false :flag true]
                ["-h" "--help" "Print this help" :default false :flag true]
                ])

(defn paginate [url f]
  (take-while (complement nil?) (iterate f url)))

(defn scrape [urls f]
  (lazy-seq (mapcat f urls)))

(def lj-pages  (paginate *base-url* vrotmne-prev))
(def lj-posts  (scrape lj-pages vrotmne-posts))
(def lj-images (scrape lj-posts vrotmne-images))

(defn -main [& args]
;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (let [[opts args banner] (apply cli args app-specs)]

    (when (:help opts)
      (println banner)
      (System/exit 0))

    (println opts)

    (binding [*debug* (:debug opts)
              *base-url* (if (zero? (:skip opts))
                           "http://lj.rossia.org/users/vrotmnen0gi/"
                           (str "http://lj.rossia.org/users/vrotmnen0gi/?skip=" (:skip opts)))
              *images-dir* (:images opts)
              *cache-dir*  (:cache  opts)
              *pool* (Executors/newFixedThreadPool (:workers opts))
              *use-cache* (:cache-only opts)
              ]

      (swap! state assoc :pool *pool*)

;;       (.addShutdownHook
;;        (Runtime/getRuntime)
;;        (Thread. (fn []
;;                   (println)
;;                   (println "Saving history...")
;;                   (write-history))))

      (init-history)
      (println "About"(count (:history @state)) "files already downloaded")

      (fs/make-dir *images-dir* *cache-dir*)

      ;; UI setup
      (when-not (:list-only opts)
        (println (stats-headers))
        (.start (Thread. #(while true
                            (print-stats @state)
                            (Thread/sleep 150)))))

      (doseq [image lj-images]
        (let [title (:title image)
              src   (:url   image)
              index (:index image)
              date  (:date  image)
              di    (str *images-dir* "/" title)
              fname (sanitize-path (fs/filename-v3 src title date index *images-dir*))]

          ;; (when (< (.getUsableSpace (io/file ".")) (* 100 1024 1024))
          ;;   (do
          ;;     (println "Low disk space. Exiting")
          ;;     (System/exit 0)))

          (cond
           (in-history? fname)
           (inc-counter :in-history)

           :else
           (let [host (fs/hostname src)
                 _    (swap! state update-in [:url-count] inc)]
             (if (:list-only opts)
               (println src)
               (if (still-exists? src)
                 (.submit *pool* (partial download-to src fname))
                 (do
                   (inc-counter :missing)
                   (swap! state assoc :last-file src)))))))))))
