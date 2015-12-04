(ns base64.core
  { :doc "main"
   :author "Michio Nakagawa <michio.nakagawa@gmail.com>"}
  (:require [base64.encode :refer :all]))

(defn basic-tests []
  (let [TEXTS ["a", "ab", "abc", "abcd", "Hello, clojure.", "I love clojure", "Clojureは楽しいね!"]]
    (doseq  [text TEXTS]
      (println (concat "text: " text))
      (println (concat "enc : " (bytes-to-string (encode-string text))))
      (println (concat "dnc : " (bytes-to-string (decode (encode-string text)))))
      (println "==="))))

(defn performance-tests []
  (let [long-string-1 (apply str (repeat 1000 "I love clojure"))
        long-byte-array-1 (string-to-bytes long-string-1)
        long-encoded-array-1 (encode long-byte-array-1)
        long-encoded-string-1 (bytes-to-string long-encoded-array-1)
        long-string-2 (apply str (repeat 1000 "Clojureは楽しいね!"))
        long-byte-array-2 (string-to-bytes long-string-2)
        long-encoded-array-2 (encode long-byte-array-2)
        long-encoded-string-2 (bytes-to-string long-encoded-array-2)]
    (println "encode")
    (dotimes [_ 3]
      (time (encode long-byte-array-1))
      (time (encode long-byte-array-2)))
    (println "encode-string")
    (dotimes [_ 3]
      (time (encode-string long-string-1))
      (time (encode-string long-string-2)))
    (println "decode")
    (dotimes [_ 3]
      (time (decode long-encoded-string-1))
      (time (decode long-encoded-string-2)))
    (println "decode-string")
    (dotimes [_ 3]
      (time (decode-string long-encoded-string-1))
      (time (decode-string long-encoded-string-2)))))

(defn -main
  [& args]
  (if (empty? args)
    (do
      (basic-tests)
      (performance-tests))
    (doseq [arg args]
      (println (bytes-to-string (encode-string arg))))))
