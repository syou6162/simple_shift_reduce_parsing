(defproject simple_shift_reduce_parsing "0.0.1"
  :description "Dependency parser based on shift reduce algorithm"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.2"]
                 [fobos_multiclass_clj "0.1.0-SNAPSHOT"]
		 [info.yasuhisay/vijual "0.2.0-SNAPSHOT"]]
  :plugins [[lein-swank "1.4.4"]]
  :main simple_shift_reduce_parsing.core)
