(defproject binary "0.0.2-SNAPSHOT"
  
  :description  "clojure tools for reading writing binary data"

  :url "https://github.com/pupcus/binary"

  :scm {:url "git@github.com:pupcus/binary.git"}

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/tools.logging "0.3.1"]]

  :profiles {:dev  {:resource-paths ["dev-resources"]
                    :dependencies [[org.clojure/clojure "1.8.0"]
                                   [org.slf4j/slf4j-log4j12 "1.7.5"]]}}

  :deploy-repositories [["snapshots"
                         {:url "https://clojars.org/repo"
                          :creds :gpg}]
                        ["releases"
                         {:url "https://clojars.org/repo"
                          :creds :gpg}]]

  :global-vars {*warn-on-reflection* true
                *assert* false})
