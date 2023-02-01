(ns laheadle.scripting.util
  (:require [cheshire.core :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def debug nil)

(defn pray [msg cond & data]
  (if debug
    (if cond
      (println (if (= debug :heavy)
                 (str "success: " msg ": " cond "[" (first data) "]")
                 (str "success: " msg ": " cond)))
      (throw (ex-info "assertion failed" {:message msg :data data})))
    (when-not cond (throw (ex-info "assertion failed" {:message msg :data data})))))

(defn str-exists? [s]
  (not (str/blank? s)))

(defn run-and-then [command and-then]
  (pray "run-and-then" (vec command) (vec command))
  (let [{:keys [:exit :err :out]} (apply shell/sh command)]
    (if (zero? exit)
      (do (when (and err (str-exists? err)) (println err))
          (and-then out))
      (do (println "ERROR:" err)
          (println "OUT:" out)
          (System/exit 1)))))


(defn run-out [command]
  (pray "run-out" command (vec command))
  (let [{:keys [:exit :err :out]} (apply shell/sh command)]
    (if (zero? exit)
      (do (when (str-exists? err) (println "ERROR:" err))
          out)
      (do (println "ERROR:" err)
          (System/exit 1)))))

(defn save-json [object file]
  (as-> object $
      (json/generate-string $ {:pretty true})
      (spit file $)))

(defn ingest [file]
  (as-> (slurp file) $
    (json/parse-string $ true)))


(defn str-is-integer? [str] (boolean (re-matches #"\d+" str)))

(defn print-errors [json-str]           ;; from graphql call
  (let [errs (-> (json/parse-string json-str true)
                 :errors)]
    (when (> (count errs) 0)
      (println errs))
    json-str))

(defn as-lines [long-str]
  (str/split long-str #"\r?\n"))

(defn sleep [n] (Thread/sleep n))