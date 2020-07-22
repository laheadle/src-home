(ns laheadle.wrapper.curl
  (:require [laheadle.scripting.util :as util]
            [cheshire.core :as json]
            [clojure.string :as str]))

(defn as-string [s]
  (if (keyword? s) (name s)
      (str s)))

(defn str-opt [s] (str "--" (as-string s)))

(defn make-opt [[k v]] [(str-opt k) (as-string v)])

(defn make-opts-vector [opts]
  (vec (mapcat make-opt opts)))

(defn format-key [[k v]] (format "%s=%s" (as-string k) (as-string v)))

(defn query-string [query-map]
  (case (count query-map)
    0 "" 
    (let [equals (map format-key query-map)
          joined (str/join "&" equals)]
      (str "?" joined))))

(defn make-url [base path query & [fullpath]]
  (if fullpath fullpath
      (str "https://" base path (query-string query))))

(defn make-header [[k v]] (format "%s: %s" k v))

(defn make-headers [headers]
  (mapcat (fn [[k v]] (make-opt [:header (make-header [k v])]))
          (seq headers)))

(defn make-flags-vector [flags]
  (mapv str-opt flags))

(defn curl-command [{:keys [:headers :flags :opts :base :path :query :fullpath]}]
  (flatten ["curl"
            (make-opts-vector opts)
            (make-flags-vector flags)
            (if headers (make-headers headers) [])
            
            (make-url base path query fullpath)]))

(defn merge-curl-command-maps [c1 c2]
  (merge c1 c2
         {:headers (merge (:headers c1) (:headers c2))
          :query (merge (:query c1) (:query c2))
          :opts (merge (:opts c1) (:opts c2))
          :flags (into (:flags c1) (:flags c2))}))

(defn make-curl-command-map [cmd]
  (merge {:headers {} :query {} :opts {} :flags [] :base "" :path "" :fullpath nil}
         cmd))

(comment
  (def cc4
    (curl-command {:opts {:k "v" :y "z"} :base "api.github.com" :path "users/laheadle/repos"
                   :query {:time "now" :where "here" :when "then"}
                   :headers {"Content-type" "application/javascript"
                             "ETag" "none"
                             "Authorization" "Bearer aavvvvv"}})))
