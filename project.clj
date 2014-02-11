(defproject scraper "0.3.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [http-kit "2.1.16"]
                 [enlive "1.1.5"]
                 [clj-time "0.6.0"]
                 [me.raynes/fs "1.4.5"]
                 [org.clojure/tools.cli "0.2.4"]
                 [org.apache.commons/commons-io "1.3.2"]
                 [liberator "0.10.0"]
                 [compojure "1.1.6"]
                 [ring/ring-core "1.2.1"]
                 [ring/ring-jetty-adapter "1.2.1"]
                 [cheshire "5.3.1"]
                 [org.clojure/tools.cli "0.3.1"]
                 [digest "1.4.3"]]
  :main scraper.core)
