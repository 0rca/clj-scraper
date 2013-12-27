(ns scraper.core
  (:require [net.cgrand.enlive-html :as html])
  (:require [clj-http.lite.client :as client])
  (:require [clojure.java.io      :as io])
  (:require [clojure.string       :as str])
  (:gen-class))

(def base-url "http://lj.rossia.org/users/vrotmnen0gi/")

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

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


(defn next-page [page]
  (let [paginators (filter #(-> % :attrs :href next?) (html/select page [:a]))
        prev       (first (filter #(= "earlier" (first (:content %))) paginators))]
  (-> prev :attrs :href)
))


(defn jpeg? [url]
  (let [file (-> url (str/split #"/") last)
        ext  (-> file (str/split #"\.") last)]
    (or (= ext "jpeg") (= ext "jpg"))))


(assert (= true (jpeg? "http://example.com/img/123.jpg")))
(assert (= true (jpeg? "http://example.com/img/123.jpeg")))


(defn download-from [url]
  (client/get url {:as :byte-array}))


(defn write-file [f stream]
   (with-open [w (clojure.java.io/output-stream f)]
     (.write w (:body stream))))


(defn filename-from [url]
  (last (clojure.string/split url #"/")))


(defn download-and-save [url title]
  (let [dir  (str/join "/" ["images" title])
        file (str/join "/" [dir (filename-from url)])]
  (.mkdir (io/file dir))
  (if (.exists (io/file file))
    (println "file exists - skipping")
    (try
      (write-file file (download-from url))
      (catch Exception _ (println "some error happened - skipping"))))))


(defn scrape-post [url]
  (let [page    (fetch-url url)
        links   (html/select page [:a])
        hrefs   (filter #(not (nil? %)) (map #(-> % :attrs :href) links))
        jpegs   (filter jpeg? hrefs)
        title   (html/text (last (html/select page [:td.caption])))]

    (println "\t" (count jpegs) " images at " title )
    (doseq [url jpegs]
      (println "Downloading from " url)
      (download-and-save url title)
      )))


(defn scrape-page [page]
  (println "Page: " (-> (html/select page [:title]) first :content))
  (doseq [item (posts page)]
    (println "\tPost: " item)
    (scrape-post item)))


(defn scrape-all [url]
  (when (not (nil? url))
    (let [page (fetch-url url)]
      (scrape-page page)
      (println "scraping from " url)
      (recur (next-page page)))))

(defn find-all-by-text [page text]
  (map #(-> % :attrs :href)
        (filter #(= text (-> % :content first))
                 (html/select page [:a]))))

(defn find-by-text [page text]
  (first (find-all-by-text page text)))

;; site-specific stuff
(defn next-page
  "Returns next page url for url given, or nil if none found"
  [url]
  (when (map? url)
    (let [page (fetch-url (url :page))]
      {:page (find-by-text page "earlier")})))


(defn page-seq
  "Returns lazy sequence of pages, paginated by next-page function"
  [base-url page-fn]
  (iterate page-fn base-url))

(defn posts-for [url]
  (when (map? url)
    (let [page (fetch-url (url :page))]
      (map #(apply hash-map %)
            (partition 2 (interleave (iterate identity :post) (find-all-by-text page "Link")))))))


(defn post-seq [pages posts-fn]
  (mapcat posts-fn pages))

(defn image-seq [url]
  (let [page    (fetch-url (url :post))
        links   (html/select page [:a])
        hrefs   (filter #(not (nil? %)) (map #(-> % :attrs :href) links))
        jpegs   (filter jpeg? hrefs)
        title   (html/text (last (html/select page [:td.caption])))]
    (map #(apply hash-map %) jpegs))

;; Site-specific stuff
(defn lj-pages []
  (page-seq {:page base-url} next-page))

(defn lj-posts []
  (post-seq (lj-pages) posts-for))

(defn lj-images []
  mapcat image-seq (lj-posts))


(defn -main []
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  ;; (.mkdir (io/file "images"))

  ;; (scrape-all base-url)


  (doseq [post-url images]
    (println "scraping from " post-url))

)
