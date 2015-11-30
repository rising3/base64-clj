(ns base64-clj.encode_test
  { :doc "BASE64 encod/decode libraries test."
    :author "Michio Nakagawa <michio.nakagawa@gmail.com>"}
  (:require [clojure.test :refer :all]
            [base64-clj.encode :refer :all]))

(deftest tbl-at-test
  (testing "0 is A"
	  (are [^byte n] (= (byte \A) (tbl-at n))
	       0))
  (testing "63 is /"
	  (are [^byte n] (= (byte \/) (tbl-at n))
	       63)))

(deftest tbl-of-test
  (testing "A is 0"
	  (are [^byte n] (= 0 (tbl-of n))
	       \A))
  (testing "/ is 63"
	  (are [^byte n] (= 63 (tbl-of n))
	       \/)))

(deftest string-to-bytes-test
  (testing "A is 0"
	  (are [n] (= "ABC" (String. (string-to-bytes n)))
	       "ABC")))

(deftest bytes-to-string-test
  (testing "A is 0"
	  (are [^bytes n] (= "ABC" (bytes-to-string n))
	       (.getBytes "ABC"))))

(deftest write-base64-test
  (testing "encode 'A'"
	  (are [n] (= "QQ==") (String. (write-base64 n)))
	       (.getBytes "A"))
  (testing "encode 'AB'"
	  (are [n] (= "QUI=") (String. (write-base64 n)))
	       (.getBytes "AB"))
  (testing "encode 'ABC'"
	  (are [n] (= "QUJD") (String. (write-base64 n)))
	       (.getBytes "ABC")))

(deftest encode-test
  (testing "encode 'Hello, clojure.'"
	  (are [n] (= "SGVsbG8sIGNsb2p1cmUu") (String. (encode n)))
	       (.getBytes "Hello, clojure."))
  (testing "encode 'I love clojure'"
	  (are [n] (= "SSBsb3ZlIGNsb2p1cmU=") (String. (encode n)))
	       (.getBytes "I love clojure")))

(deftest encode-string-test
  (testing "encode 'Hello, clojure.'"
	  (are [n] (= "SGVsbG8sIGNsb2p1cmUu") (String. (encode-string n)))
	       "Hello, clojure.")
  (testing "encode 'I love clojure'"
	  (are [n] (= "SSBsb3ZlIGNsb2p1cmU=") (String. (encode-string n)))
	       "I love clojure"))

(deftest read-base64-test
  (testing "decode 'QQ=='"
	  (are [n] (= "AB") (String. (read-base64 n)))
	       (.getBytes "QQ=="))
  (testing "decode 'QUI='"
	  (are [n] (= "QUI=") (String. (read-base64 n)))
	       (.getBytes ""))
  (testing "decode 'QUJD'"
	  (are [n] (= "ABC") (String. (read-base64 n)))
	       (.getBytes "QUJD")))

(deftest decode-test
  (testing "decode 'SGVsbG8sIGNsb2p1cmUu'"
	  (are [n] (= "Hello, clojure.") (String. (decode n)))
	       (.getBytes "SGVsbG8sIGNsb2p1cmUu"))
  (testing "decode 'SSBsb3ZlIGNsb2p1cmU='"
	  (are [n] (= "I love clojure") (String. (decode n)))
	       (.getBytes "SSBsb3ZlIGNsb2p1cmU=")))

(deftest decode-string-test
  (testing "decode 'SGVsbG8sIGNsb2p1cmUu'"
	  (are [n] (= "Hello, clojure.") (String. (decode-string n)))
	       "SGVsbG8sIGNsb2p1cmUu")
  (testing "decode 'SSBsb3ZlIGNsb2p1cmU='"
	  (are [n] (= "I love clojure") (String. (decode-string n)))
	       "SSBsb3ZlIGNsb2p1cmU="))
