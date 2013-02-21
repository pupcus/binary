(defproject binary "0.0.1-SNAPSHOT"
  
  :description          "tools for reading writing binary data"

  :repositories         { "sonatype-oss-public" "https://oss.sonatype.org/content/groups/public" }

  :dependencies         [[org.clojure/clojure "1.3.0"]
                         [log4j "1.2.15"
                                :exclusions [javax.mail/mail javax.jms/jms com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                         [org.slf4j/slf4j-log4j12 "1.6.1"]]
  
  :dev-dependencies     [[swank-clojure "1.3.3-SNAPSHOT"]]

  :dev-resources-path   "dev-resources/")
