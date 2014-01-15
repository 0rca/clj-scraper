(ns scraper.core
  (:require [net.cgrand.enlive-html :as html])
  (:require [clj-http.lite.client :as client])
  (:require [clojure.java.io      :as io])
  (:require [clojure.string       :as str])
  (:gen-class))

(def ^:dynamic *debug* false)
(def ^:dynamic *cache-dir* "cache")

(def base-url "http://lj.rossia.org/users/vrotmnen0gi/")
;; (def base-url "http://lj.rossia.org/users/vrotmnen0gi/?skip=40")

(defn download-from [url]
  (client/get url {:as :byte-array}))

(defn write-file [f stream]
   (with-open [w (clojure.java.io/output-stream f)]
     (.write w (:body stream))))

(defn fetch-url [url]
  (when *debug* (println "Fetching " url))
  (html/html-resource (java.net.URL. url)))

(defn fetch-cached-url [url]
  (when *debug* (println "Fetching" url "from cache"))
  (let [file (last (str/split url #"/"))
        path (str *cache-dir* "/" file)]
    (if (.exists (io/file path))
      (println "HIT")
      (do
        (println "MISS: downloading")
        (write-file path (download-from url))
        (println "OK")))

    (html/html-resource (io/file path))))


(defn links-all [page]
  (map #(:href (:attrs %))
    (html/select page [:a])))


;; post with tests
(defn post? [link]
  (let [re-post #"http://lj.rossia.org/users/vrotmnen0gi/[0-9]+.html"]
    (not (nil? (re-matches re-post link)))))

(assert (= true (post? "http://lj.rossia.org/users/vrotmnen0gi/993483.html")))


;; next page link?
(defn next? [link]
  (let [re-next #"http://lj.rossia.org/users/vrotmnen0gi/\?skip=[0-9]+"]
    (not (nil? (re-matches re-next link)))))

(assert (= true (next? "http://lj.rossia.org/users/vrotmnen0gi/?skip=20")))


(defn posts [page]
  (filter post? (links-all page)))

(defn jpeg? [url]
  (let [file (-> url (str/split #"/") last)
        ext  (-> file (str/split #"\.") last)]
    (or (= ext "jpeg") (= ext "jpg"))))


(assert (= true (jpeg? "http://example.com/img/123.jpg")))
(assert (= true (jpeg? "http://example.com/img/123.jpeg")))


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
      (tag-all-with (find-all-by-text page "Link") :post))))


(defn filename-v1 [url]
  (last (str/split url #"/")))

(defn filename-v2 [url]
  (str/join ""
    (drop 3
      (str/split url #"/"))))

(assert (= (filename-v2 "http://s017.radikal.ru/i413/1312/6b/3dd009003d85.jpg")
            "i41313126b3dd009003d85.jpg"))

(assert (= (filename-v1 "http://s017.radikal.ru/i413/1312/6b/3dd009003d85.jpg")
            "3dd009003d85.jpg"))

(defn post-seq [pages posts-fn]
  (lazy-seq
    (when (seq? pages)
      (concat (posts-fn (first pages)) (post-seq (rest pages) posts-fn)))))

(defn image-seq [url]
  (let [page    (fetch-cached-url (url :post))
        links   (html/select page [:a])
        hrefs   (filter #(not (nil? %)) (map #(-> % :attrs :href) links))
        jpegs   (filter jpeg? hrefs)
        title   (html/text (last (html/select page [:td.caption])))]
    (map #(merge {:title title :index %1} %2)
          (range)
          (tag-all-with jpegs :img))))

;; Site-specific stuff
(defn lj-pages []
  (page-seq {:page base-url} prev-page))

(defn lj-posts []
  (post-seq (lj-pages) posts-for))

(defn lj-images []
  (mapcat image-seq (lj-posts)))

(defn make-dir [& dirs]
  (doseq [v dirs]
    (let [dir (io/file v)]
      (when-not (.exists dir)
        (.mkdirs dir)))))

(defn remote-type? [type url]
  (= type (get-in (client/head url) [:headers "content-type"])))

(def remote-jpeg? (partial remote-type? "image/jpeg"))

(defn -main []
;; work around dangerous default behaviour in Clojure
;;   (alter-var-root #'*read-eval* (constantly false))

  (binding [*debug* true]
    (make-dir "images" "cache")

    (doseq [image-src (lj-images)]
      (let [title   (str/trim (str/replace (:title image-src) #"[:)]" "_"))
            src     (:img   image-src)
            index   (:index image-src)
            dir     (str "images/" title)
            oldfile (str dir "/" (filename-v1 src))
            file    (str dir "/" index "-" (filename-v2 src))]

        (do

          (let [file-v1 (io/file oldfile)
                file-v2 (io/file file)]
            (cond (.exists file-v2)
                  (println "file exists - skipping")
                  (.exists file-v1)
                  (do
                    (println "old file exists - renaming to" file)
                    (when-not (.renameTo file-v1 file-v2)
                      (println "Failure...")))
                  :else
                  (try
                    (println ".")
                    (when (remote-jpeg? src)
                      (println file "<-" src)
                      (make-dir dir)
                      (write-file file (download-from src))
                      (println "Saved as " file))
                    (catch Exception e (println (str e " - skipping")))))))))))
