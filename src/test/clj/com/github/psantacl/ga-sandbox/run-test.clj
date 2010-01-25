
(ns {{project.testScript.className}}
  (:use clojure.contrib.test.junit
        clojure.contrib.test-is)
  (:gen-class))

(def *all-tests*
  '())

(defn main [& args]
  (apply run-test *all-tests*))
