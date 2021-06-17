#!/usr/bin/env bb

(require '[babashka.process :refer [process]]
         '[cheshire.core :as cheshire]
         '[clojure.string :as str])

(def trace-cmd *command-line-args*)

(def trace-agent-env "-agentlib:native-image-agent=trace-output=trace-file.json")
(def config-agent-env "-agentlib:native-image-agent=config-output-dir=.")

@(process trace-cmd {:inherit true :extra-env {"JAVA_TOOL_OPTIONS" trace-agent-env}})
@(process trace-cmd {:inherit true :extra-env {"JAVA_TOOL_OPTIONS" config-agent-env}})

(def trace-json (cheshire/parse-string (slurp "trace-file.json") true))

;; [Z = boolean
;; [B = byte
;; [S = short
;; [I = int
;; [J = long
;; [F = float
;; [D = double
;; [C = char
;; [L = any non-primitives(Object)

(defn normalize-array-name [n]
  ({"[F" "float[]"
    "[B" "byte[]"
    "[Z" "boolean[]"
    "[C" "char[]"
    "[D" "double[]"
    "[I" "int[]"
    "[J" "long[]"
    "[S" "short[]"} n n))

(def ignored (atom #{}))
(def unignored (atom #{}))

(defn ignore [{:keys [:tracer :caller_class :function :args] :as _m}]
  (when (= "reflect" tracer)
    (when-let [arg (first args)]
      (let [arg (normalize-array-name arg)]
        (if (and caller_class
                 (or (= "clojure.lang.RT" caller_class)
                     (= "clojure.genclass__init" caller_class)
                     (and (str/starts-with? caller_class "clojure.core$fn")
                          (= "java.sql.Timestamp" arg)))
                 (= "forName" function))
          (swap! ignored conj arg)
          (when (= "clojure.lang.RT" caller_class)
            ;; unignore other reflective calls in clojure.lang.RT
            (swap! unignored conj arg)))))))

(run! ignore trace-json)

;; (prn @ignored)
;; (prn @unignored)

(defn process-1 [{:keys [:name] :as m}]
  (when-not (and (= 1 (count m))
                 (contains? @ignored name)
                 (not (contains? @unignored name)))
    ;; fix bug(?) in automated generated config
    (if (= "java.lang.reflect.Method" name)
      (assoc m :name "java.lang.reflect.AccessibleObject")
      m)))

(def config-json (cheshire/parse-string (slurp "reflect-config.json") true))

(def cleaned (keep process-1 config-json))

(spit "reflect-config-cleaned.json" (cheshire/generate-string cleaned {:pretty true}))
