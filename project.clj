(defproject simple-shift-reduce-parsing "0.0.3"
  :description "Dependency parser based on shift reduce algorithm"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.1"]
                 [dorothy "0.0.3"]
                 [info.yasuhisay/clj-utils "0.1.1"]
                 [de.bwaldvogel/liblinear "1.92"]
                 [info.yasuhisay/liblinear "0.0.1"]
                 [log4j/log4j "1.2.16"
                  :exclusions [javax.mail/mail javax.jms/jms
                               com.sun.jdmk/jmxtools com.sun.jmx/jmxri]]
                 [org.slf4j/slf4j-log4j12 "1.6.4"]
                 [org.clojure/tools.logging "0.2.3"]
                 [clj-logging-config "1.9.7"]
                 [net.sf.trove4j/trove4j "3.0.3"]]
  :jvm-opts ["-Xms100G" "-Xmx100G" "-server"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSParallelRemarkEnabled"
             "-XX:+UseParNewGC"
             "-Dfile.encoding=UTF-8"]
  :global-vars {*warn-on-reflection* true}
  :main simple_shift_reduce_parsing.core)
  :java-source-paths ["src/java"]
