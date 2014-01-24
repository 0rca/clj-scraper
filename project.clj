(defproject scraper "0.2.2-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [http-kit "2.1.16"]
                 [enlive "1.1.5"]
                 [clj-time "0.6.0"]
                 [org.clojure/tools.cli "0.2.4"]
                 [digest "1.4.3"]]
  :main scraper.core)
