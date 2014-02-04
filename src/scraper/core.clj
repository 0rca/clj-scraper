(ns scraper.core
  (:require [net.cgrand.enlive-html :as html])
  (:require [org.httpkit.client     :as http])
  (:require [clojure.java.io        :as io])
  (:require [clojure.string         :as str])
  (:require [clj-time.format        :as tf ])
  (:require [digest                 :refer [md5]])
  (:require [scraper.fs :as fs])
  (:require [scraper.ui :as ui])
  (:require [clojure.tools.cli     :refer [cli]])
  (:import  [java.util.concurrent   Executors])
  (:import  [org.apache.commons.io  FileUtils])
  (:require [liberator.core :refer [resource defresource]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.adapter.jetty :refer [run-jetty]]
            [compojure.core :refer [defroutes ANY]])
  (:require [cheshire.core :as json :only [generate-string]])
  (:gen-class))

(def ^:dynamic *pool*)
(def ^:dynamic *debug* false)
(def ^:dynamic *cache-dir* "cache")
(def ^:dynamic *images-dir* "images")
(def ^:dynamic *base-url*)

(def state (atom {
                  :running true
                  :completed 0
                  :errors 0
                  :missing 0
                  :last-file  ""
                  :history {}
                  :url-count 0
                  :in-history 0
                  :page-cache {}
                  }))

(defn inc-counter [c]
  (swap! state update-in [c] inc))

(defn dec-counter [c]
  (swap! state update-in [c] dec))

(defmacro debug [body]
  `(when *debug* (~@body)))

(defn jget [url file]
  (FileUtils/copyURLToFile (java.net.URL. url) (io/file file)))

(defn serialize-seq [what where]
  (spit where (pr-str what)))

(defn deserialize-seq [from]
  (read-string (slurp from)))

(defn cache-page! [url content]
  (let [filename (fs/cached-name url *cache-dir*)]
    (serialize-seq content filename)
    (swap! state update-in [:page-cache] assoc url filename)))

(defn cached-page [url]
  (let [filename (or (get-in @state [:page-cache url])
                     (fs/cached-name url *cache-dir*))]
    (swap! state update-in [:page-cache] assoc url filename)
    (deserialize-seq filename)))

(defn cached? [url]
  (or (contains? (:page-cache @state) url)
      (fs/exists? (fs/cached-name url *cache-dir*))))

(defn fetch-resource [url]
  (try
    (html/html-resource (java.net.URL. url))
    (catch java.io.IOException e (inc-counter :errors) nil)))

(defn fetch-maybe-cached-resource [url]
  (if (cached? url)
    (cached-page url)
    (when-let [content (fetch-resource url)]
      (cache-page! url content)
      content)))

(defn sanitize-path [path]
  (str/replace path #"\s*блеать\s*|#" ""))

(defn jpeg? [url]
  (let [ext (last (str/split url #"\."))]
    (seq? (re-seq #"jpe?g$" (str/lower-case ext)))))

(defn find-all-by-text [page text]
  (map #(-> % :attrs :href)
        (filter #(= text (-> % :content first))
                 (html/select page [[:a (html/attr? :href)]]))))

(defn find-by-text [page text]
  (first (find-all-by-text page text)))


(defn next-by-text [text url]
  (when-not (nil? url)
    (when-let [content (fetch-resource url)]
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

(defn remote-mime? [type url]
  (debug (println "."))
  (let [resp (http/head url {:keepalive 600000})]
    (and
     (= 200  (:status @resp))
     (= type (get-in @resp [:headers :content-type])))))


(def remote-jpeg? (partial remote-mime? "image/jpeg"))

(defn still-exists? [url]
   (remote-jpeg? url))


(defn history-entry [file]
  (hash-map (.getName file) file))

(defn add-to-history [f]
  (swap! state update-in [:history] into (history-entry (io/file f))))

(defn download-to
  "Download a file from a URI"
  [src fname]
  (try
    (let [tmpfile (str "tmp/" (fs/filename-from src))]
      (jget src tmpfile)
      (fs/make-dir (fs/path fname))
      (fs/rename tmpfile fname)
      (inc-counter :completed)
      (add-to-history fname)
      (swap! state assoc :last-file (fs/short-name fname)))
    (catch java.io.IOException e
      (inc-counter :errors))))


(defn zserialize-seq [what where]
  (with-open [w (-> where
                    io/output-stream
                    java.util.zip.GZIPOutputStream.
                    io/writer)]
    (binding [*out* w] (pr w))))

(defn- write-history []
  (serialize-seq (:history @state) "history.txt.gz"))

(defn zdeserialize-seq [from]
  (with-open [in (java.util.zip.GZIPInputStream.
                 (io/input-stream from))]
    (mapcat read-string (str/split (slurp in) #"\n"))))

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

(def app-specs [
                ["-c" "--cache" "Cache directory"
                 :default "cache"]
                ["-o" "--output" "Images directory"
                 :default "images"]
                ["-w" "--workers" "Number of download workers"
                 :default 4 :parse-fn #(Integer. %)]
                ["-d" "--debug" "Display debug information"
                 :default false]
                ["-s" "--source" "Website to download from"
                 :default "vrotmne"]
                ["-l" "--list-only" "Save url into list, instead"
                 :default false :flag true]
                ["-h" "--help" "Print this help" :default false :flag true]
                ])

(defn get-attr
  "Return atribute value of an html element"
  [el attr]
  (get-in el [:attrs attr]))

(defn paginate
  "Generic low-level pagination function"
  [f url]
  (lazy-seq
   (when-not (nil? url)
     (cons url (paginate f (f url))))))

(defn paginate-2
  "Mid-level pagination function that takes care of resource fetching and caching"
  [url f]
  (paginate (fn [url]
              (let [uri (java.net.URI. url)
                    content (fetch-maybe-cached-resource url)
                    paginator (f content)]
                (when-not (nil? paginator)
                  (when-let [href (get-attr paginator :href)]
                    (str (.resolve uri href))))))
            url))

(defn paginate-1
  "Mid-level pagination function that takes care of resource fetching,
   non-caching variant"
  [url f]
  (paginate (fn [url]
              (let [uri (java.net.URI. url)
                    content (fetch-resource url)
                    paginator (f content)]
                (when-not (nil? paginator)
                  (when-let [href (get-attr paginator :href)]
                    (str (.resolve uri href))))))
            url))

(defn scrape
  "Generic low-level scraping function"
  [f urls]
  (concat [] (lazy-seq
              (when-not (empty? urls)
                (when-let [head (f (first urls))]
                  (concat head (scrape f (rest urls))))))))


(defn lj-pages []
  (paginate
   (partial next-by-text "earlier")
   "http://lj.rossia.org/users/vrotmnen0gi/"))

(defn lj-posts [pages]
  (scrape
   (fn [url]
     (let [content (fetch-resource url)]
       (find-all-by-text content "Link")))
   (pages)))

(defn lj-images [pages]
  (scrape
   (fn [url]
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
   (lj-posts pages)))


(defn paginate-rel [rel-text]
  (fn [content]
    (first (filter #(= rel-text (get-attr % :rel))
                   (html/select content [[:a (html/attr? :rel)]])))))

(defn paginate-text [text]
  (fn [content]
    (first
      (filter #(= text (-> % :content first))
              (html/select content [[:a (html/attr? :href)]])))))

(defn paginate-class [klass]
  (fn [content]
    (first
      (filter #(= klass (get-attr % :class))
              (html/select content [[:a (html/attr? :href)]])))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RAILSCASTS
;;
(defn railscasts-pages-count []
  (count
    (paginate-2 "http://www.railscasts.com/?page=41"
                (paginate-rel "next"))))

(defn railscasts-episodes []
  (scrape (fn [url]
            (let [content (fetch-maybe-cached-resource url)]
              (map #(array-map url (html/text %)) (html/select content [:h2 :a]))))
          (paginate-2 "http://railscasts.com/?page=41"
                      (paginate-rel "next"))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; National Geographic
;;
(defn ngo-pages-landscapes []
  (paginate-2 "http://photography.nationalgeographic.com/photography/photo-of-the-day/landscapes/"
              (paginate-text "Next »")))

(defn ngo-pages-nature-and-weather []
  (paginate-2 "http://photography.nationalgeographic.com/photography/photo-of-the-day/nature-weather/"
              (paginate-text "Next »")))

(defn ngo-image-pages [pages]
  (scrape
   (fn [url]
     (let [content (fetch-maybe-cached-resource url)
           links   (html/select content [:div#search_results :> :div :> :a])
           url-fn  (fn [url]
                     (if (= \/ (first url))
                       (str "http://photography.nationalgeographic.com" url)
                       url))]
       (map url-fn (map #(get-in % [:attrs :href]) links))))
   pages))

(defn ngo-download-links [pages]
  (scrape
   (fn [url]
     (let [content (fetch-maybe-cached-resource url)]
       (map #(str "http:" %) (map #(get-attr % :href) (html/select content [:div.download_link :a])))))
   (ngo-image-pages pages)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Motherless
;;

(defn motherless-ladyboys-pages []
  (paginate-2 "http://motherless.com/search/images?term=ladyboy&member=&sort=relevance&range=0&size=3"
              (paginate-text "NEXT »")))

;;---------------------------------------

(defresource status
  :available-media-types ["application/json"]
  :handle-ok (fn [_] (json/generate-string (ui/stats-map @state))))

(defroutes app
  (ANY "/foo" [] (resource :available-media-types ["text/html"]
                           :handle-ok "<html>Hello, Internet.</html>"))

  (ANY "/status" [] status))

(def handler
  (-> app
      (wrap-params)))


(defn -main [& args]
;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (.start (Thread. #(run-jetty #'handler {:port 3000}) "Jetty"))

  (let [[opts args banner] (apply cli args app-specs)]

    (when (:help opts)
      (println banner)
      (System/exit 0))

    (println opts)

    (binding [*debug* (:debug opts)
;;               *base-url* (if (zero? (:skip opts))
;;                            "http://lj.rossia.org/users/vrotmnen0gi/"
;;                            (str "http://lj.rossia.org/users/vrotmnen0gi/?skip=" (:skip opts)))
              *images-dir* (:output opts)
              *cache-dir*  (:cache  opts)
              *pool* (Executors/newFixedThreadPool (:workers opts))
              ]

      (swap! state assoc :pool *pool*)

;;       (.addShutdownHook
;;        (Runtime/getRuntime)
;;        (Thread. (fn []
;;                   (println)
;;                   (println "Saving history...")
;;                   (write-history))))

      (init-history)
      (println "About" (count (:history @state)) "files already downloaded")

      (fs/make-dir *images-dir* *cache-dir*)

      ;; UI setup
      (when-not (:list-only opts)
        (println (ui/stats-headers))
        (.start (Thread. #(while (@state :running)
                            (ui/print-stats @state)
                            (Thread/sleep 150))
                         "UI Updates")))

      (cond
        (= "ngo" (:source opts))
        (doseq [image (ngo-download-links (concat (ngo-pages-landscapes)
                                                  (ngo-pages-nature-and-weather)))]
          (if-not (in-history? image)
            (.submit *pool* (partial download-to image (str *images-dir* "/" (fs/filename-from image))))
            (inc-counter :in-history)))

        (= "vrotmne" (:source opts))
        (doseq [image (lj-images (lj-pages))]
          (let [title (:title image)
                src   (:url   image)
                index (:index image)
                date  (:date  image)
                di    (str *images-dir* "/" title)
                fname (sanitize-path (fs/filename-v3 src title date index *images-dir*))]

            (if-not (in-history? fname)
              (if (still-exists? src)
                (.submit *pool* (partial download-to src fname))
                (inc-counter :missing))
              (do
                ;(swap! state assoc :running false)
                ;(System/exit 0)
                (inc-counter :in-history))))))

;;      (swap! state assoc :running false)
      )))

