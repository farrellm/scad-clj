(defproject scad-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [opencv "2.4.4"]]
  :repositories {"local" "file:repo"}
  :jvm-opts ["-Djava.library.path=native"]
  ;; :jvm-opts ["-Djava.library.path=/usr/local/opencv/share/OpenCV/java"]
  :java-source-paths ["src"]
  )
