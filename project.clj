(defproject simple-shift-reduce-parsing "0.0.1"
  :description "Dependency parser based on shift reduce algorithm"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.cli "0.2.1"]
                 [fobos_multiclass_clj "0.1.4"]
                 [dorothy "0.0.3"]]
  :plugins [[lein-swank "1.4.4"]]
  :jvm-opts ["-Xmx8g" "-server" "-Dfile.encoding=UTF-8"]
  :main simple_shift_reduce_parsing.core)
