(ns laheadle.munzip
  (:require [clojure.string :as str]))

(defn -main [& args]
  (let [arg (str/join " " args)
        [one two] (str/split arg #".zip")]
    (println (format "unzip '%s' -d '%s'" arg one))))

;; awk -F'.zip' '{print "unzip "$0" -d "$1}' | sh