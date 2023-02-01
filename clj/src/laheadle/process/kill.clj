
(load-file "util.clj")

(defn get-pids [process-name]
  (util/run-and-then '("ps" "auxw")
                (fn [out]   
                  (let [lines (-> out
                                  (str/split #"\n"))
                        matching (filter #(and  (str/includes? % process-name)
                                                (not (str/includes? % "kill.clj")))
                                         lines)
                        fields (map #(str/split % #"[\s]+") matching)]
                    (map #(nth % 1) fields)))))

(doseq [pid (get-pids (first *command-line-args*))]
  (util/run-and-then ["kill" "-9" pid]
                (fn [_] (println "killed" pid))))

