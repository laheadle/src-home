(ns laheadle.wrapper.git
  (:require [laheadle.scripting.util :as util]
            [clojure.java.shell :as shell]))

(defn git-fetch [dir]
  (util/run-and-then ["git" "fetch" :dir dir]
                (fn [out] true)))

(defn is-up-to-date [dir branch]
  (util/run-and-then ["git" "status" :dir dir]
                (fn [out]
                  (boolean (and (re-find (re-pattern (str "On branch " branch)) out)
                                (re-find #"Your branch is up to date" out)
                                (re-find #"nothing to commit, working tree clean" out))))))

