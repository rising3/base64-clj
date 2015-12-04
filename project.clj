(defproject base64-clj "0.1.0"
  :description "BASE64 encode/decode libraries."
  :url "https://github.com/rising3/base64-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :plugins [[no-man-is-an-island/lein-eclipse "2.0.0"]]
  :main ^:skip-aot base64.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
