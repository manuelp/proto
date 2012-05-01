;; Clojure provides **protocols** and **data types** as tools to
;; define *abstractions*.
;;
;; - Protocols provide high-performance polymorphic method dispatch
;; - Data types provide a way to create concretions of abstractions
;; defined either with protocols or Java interfaces.
;;
;; They are an idiomatic way to write abstractions and concretions
;; without resorting to Java classes and interfaces.
(ns proto.core
  (:import [java.io FileInputStream InputStreamReader BufferedReader
            FileOutputStream OutputStreamWriter BufferedWriter]))

(defn make-reader [src]
  (-> src FileInputStream. InputStreamReader. BufferedReader.))

(defn make-writer [dst]
  (-> dst FileOutputStream. OutputStreamWriter. BufferedWriter.))

(defn gulp [src]
  (let [sb (StringBuilder.)]
    (with-open [reader (make-reader src)]
      (loop [c (.read reader)]
        (if (neg? c)
          (str sb)
          (do
            (.append sb (char c))
            (recur (.read reader))))))))

(defn expectorate [dst content]
  (with-open [writer (make-writer dst)]
    (.write writer (str content))))
