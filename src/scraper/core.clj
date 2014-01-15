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


;; legacy code
;; (defn scrape-post [url]
;;   (let [page    (fetch-url url)
;;         links   (html/select page [:a])
;;         hrefs   (filter #(not (nil? %)) (map #(-> % :attrs :href) links))
;;         jpegs   (filter jpeg? hrefs)
;;         title   (html/text (last (html/select page [:td.caption])))]
;;
;;     (println "\t" (count jpegs) " images at " title )
;;     (doseq [url jpegs]
;;       (println "Downloading from " url)
;;       (download-and-save url title)
;;       )))


;; (defn scrape-page [page]
;;   (println "Page: " (-> (html/select page [:title]) first :content))
;;   (doseq [item (posts page)]
;;     (println "\tPost: " item)
;;     (scrape-post item)))


;; (defn scrape-all [url]
;;   (when (not (nil? url))
;;     (let [page (fetch-url url)]
;;       (scrape-page page)
;;       (println "scraping from " url)
;;       (recur (next-page page)))))

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
  (mapcat posts-fn pages))

(defn image-seq [url]
  (let [page    (fetch-url (url :post))
        links   (html/select page [:a])
        hrefs   (filter #(not (nil? %)) (map #(-> % :attrs :href) links))
        jpegs   (filter jpeg? hrefs)
        title   (html/text (last (html/select page [:td.caption])))]
    (map #(merge {:title title :index %1} %2)
          (range)
          (tag-all-with jpegs :img))))

;; Site-specific stuff
(defn lj-pages []
  (page-seq {:page base-url} next-page))

(defn lj-posts []
  (post-seq (lj-pages) posts-for))

(defn lj-images []
  (mapcat image-seq (lj-posts)))


;; (De-)serializing functions (defunct)
(defn cache-page [page filename]
  (spit filename (binding [*print-dup* true] (pr-str page))))

(defn uncache-page [filename]
  (with-in-str (slurp filename) (read)))
;;

(defn make-dir [& dirs]
  (for [v dirs]
    (let [d (io/file v)]
      (when-not (.exists d) (.mkdir d)))))


(defn -main []
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (make-dir "images" "cache")

  (doseq [image-src (lj-images)]
    (let [title (str/trim (:title image-src))
          src   (:img   image-src)
          index (:index image-src)
          dir   (str "images/" title)
          oldfile (str dir "/" (filename-v1 src))
          file  (str dir "/" index "-" (filename-v2 src))]

      (do
        (println title " <- " src)
        (make-dir dir)
        (cond (.exists (io/file file))
              (println "file exists - skipping")
              (.exists (io/file oldfile))
              (println "v1 file exists - skipping (but could rename, instead)")
              :else
              (try
                (write-file file (download-from src))
                (println "Saved as " file)
                (println "--")
                (catch Exception e (println (str e " - skipping")))))))))
