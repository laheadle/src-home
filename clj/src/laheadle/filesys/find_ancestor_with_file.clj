(ns laheadle.filesys.find-ancestor-with-file
  (:require [laheadle.scripting.util :as util]
            [clojure.string :as str]))

(defn pwd [& [dir]]
  (-> (if dir (util/run-out ["pwd" :dir dir]) (util/run-out ["pwd"]))
      (str/trim)))

(defn get-parent [dir]
  (util/pray "get-parent" dir)
  (pwd (str dir "/..")))

(defn contains? [dir file]
  (as-> (util/run-out ["ls" :dir dir]) $
    (util/as-lines $)
    (filter #(= % file) $)
    (seq $)))

(defn get-ancestors [dir]
  (loop [v [] d dir]
    (if (= d "/") (conj v d)
        (recur (conj v d) (get-parent d)))))

(def file (first *command-line-args*))

(println (first (drop-while #(not (contains? % file)) (get-ancestors (pwd)))))
