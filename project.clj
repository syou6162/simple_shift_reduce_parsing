(defproject simple-shift-reduce-parsing "0.0.2"
  :description "Dependency parser based on shift reduce algorithm"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.cli "0.2.1"]
                 [fobos_multiclass_clj "0.1.4"]
                 [dorothy "0.0.3"]
                 [info.yasuhisay/clj-utils "0.1.0-SNAPSHOT"]
                 [log4j/log4j "1.2.16"
                  :exclusions [javax.mail/mail javax.jms/jms
                               com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [org.slf4j/slf4j-log4j12 "1.6.4"]
                 [org.clojure/tools.logging "0.2.3"]
                 [clj-logging-config "1.9.7"]]
  :plugins [[lein-swank "1.4.4"]]
  :jvm-opts ["-Xmx8g" "-server" "-Dfile.encoding=UTF-8"]
  :main simple_shift_reduce_parsing.core)
