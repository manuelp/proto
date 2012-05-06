;; Clojure provides **protocols** and **data types** as tools to
;; define *abstractions*.
;;
;; - Protocols provide high-performance polymorphic method dispatch
;; - Data types provide a way to create concretions of abstractions
;; defined either with protocols or Java interfaces.
;;
;; They are an idiomatic way to write abstractions and concretions
;; without resorting to Java classes and interfaces.
;;
;; But these tools also have advantages over classic Java classes and
;; interfaces.
;;
;; ## Protocols ##
;; Protocols have the same advantages of Java's Interfaces:
;;
;; - Data types can implement multiple protocols.
;; - Protocols provides only *specification*, not implementation. This
;; allows a data type to implements multiple interfaces without the
;; problems associated with multiple inheritance.
;;
;; But in contrast to classic interfaces, protocols provides
;; additional benefits:
;;
;; - Existing data types can be extended to implement new interfaces
;; without modification of the said data types.
;; - Protocols are namespaced, so there is no collision even with
;; multiple parties extending the same data type.
(ns proto.core
  (:import [java.io FileInputStream InputStreamReader BufferedReader
            FileOutputStream OutputStreamWriter BufferedWriter]))

;; Here we define a protocol that supports reading and writing from.
(defprotocol IOFactory
  "A protocol for things that can be read from and written to."
  (make-reader [this] "Creates a BufferedReader.")
  (make-writer [this] "Creates a BufferedWriter."))

;; Next we write our functions in terms of the protocol.

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

;; You can extend an interface with `extend` and provide an
;; implentation for every *method* (functions are called methods in
;; this context) of the extending protocol.
;;
;; In this case, since the way to extract a `BufferedReader` and a
;; `BufferedWriter` from an input and output stream is standard, we
;; can extend that interfaces.
(extend InputStream
  IOFactory
  {:make-reader (fn [src]
                  (-> src InputStreamReader. BufferedReader.))
   :make-writer (fn [dst]
                  (throw (IllegalArgumentException.
                          "Can't open as an InputStream.")))})

(extend OutputStream
  IOFactory
  {:make-reader (fn [src]
                  (throw (IllegalArgumentException.
                          "Can't open as an OutputStream.")))
   :make-writer (fn [dst]
                  (-> dst OutputStreamWriter. BufferedWriter.))})

;; We can use `extend-type` to actually extend concrete types.
;;
;; Here we extend the `File` type and recursively call the `IOFactory`
;; protocol's implementations of the `InputStream` and `OutputStream`
;; generated from this type.
(extend-type File
  IOFactory
  (make-reader [src]
    (make-reader (FileInputStream. src)))
  (make-writer [dst]
    (make-writer (FileOutputStream. dst))))

;; There is even a macro called `extend-protocol` to extend a bunch of
;; types with the same protocol in one go.
(extend-protocol IOFactory
  Socket
  (make-reader [src]
    (make-reader (.getInputStream src)))
  (make-writer [dst]
    (make-writer (.getOuputStream dst)))

  URL
  (make-reader [src]
    (make-reader (if (= "file" (.getProtocol src))
                   (-> src .getPath FileInputStream.)
                   (.openStream src))))
  (make-writer [dst]
    (make-writer (if (= "file" (.getProtocol dst))
                   (-> dest .getPath FileInputStream.)
                   (throw (IllegalArgumentException.
                           "Can't write to non-file URL")))))))
