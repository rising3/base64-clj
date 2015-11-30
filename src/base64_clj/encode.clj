(ns base64-clj.encode
  { :doc "BASE64 encod/decode libraries."
    :author "Michio Nakagawa <michio.nakagawa@gmail.com>"})

;;
;; Constants.
;;

(def ^:const ^String BASE64_TBL
  "Base64 Alphabet Table."
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(def ^:const ^String CHARSET 
  "Base64 Charset."
  "UTF-8")

(def ^:const BASE64_PAD
  "Base64 Padding character."
  (byte \=))

;;
;; Common functions.
;;

(defn tbl-at
  "Look up character from BASE64 Table."
  [^Byte x]
  (byte (.charAt BASE64_TBL x)))

(defn tbl-of
  "Look up character index from BASE64 Table."
  [^Byte x]
  (.indexOf BASE64_TBL (String. (byte-array 1 (byte x)))))

(defn string-to-bytes
  "Convert to byte array from String."
  [x]
  (.getBytes x CHARSET))

(defn bytes-to-string
  "Convert to string from byte array."
  [^bytes x]
  (String. x CHARSET))

;;
;; BASE64 encoder.
;;

(defn write-base64
  "This function is an algorithm of BASE64 encoding.Encoding to process by 3 characters unit."
  [^bytes array]
  (let [
        ;; encode char1
        e1 (fn [c1]
             (tbl-at (bit-and (bit-shift-right c1 2) 0x3F)))
        ;; encode char2
        e2 (fn [c1 c2]
             (cond
               (nil? c2) (tbl-at (bit-and (bit-shift-left c1 4) 0x30))
               :else     (tbl-at (bit-or
                            (bit-and (bit-shift-left  c1 4) 0x30)
                            (bit-and (bit-shift-right c2 4) 0x0F)))))
        ;; encode char3
        e3 (fn [c2 c3]
             (cond
               (nil? c2) BASE64_PAD
               (nil? c3) (tbl-at (bit-and (bit-shift-left  c2 2) 0x3C)) 
               :else     (tbl-at (bit-or
                                   (bit-and (bit-shift-left  c2 2) 0x3C)
						                       (bit-and (bit-shift-right c3 6) 0x03)))))
        ;; encode char4
        e4 (fn [c3]
             (cond
               (nil? c3) BASE64_PAD
	             :else (tbl-at (bit-and c3 0x3F))))
        [c1 c2 c3] array]
    ;; writing 4 characters of BASE64.
    (conj [] (e1 c1) (e2 c1 c2) (e3 c2 c3) (e4 c3))))

(defn encode
  "Convert to BASE64(6bit) byte array from string(8bit) byte array."
  [^bytes array]
  (let [nextval (fn [x] (next (next (next x))))]
    (loop [result [] curr array]
      (if (nil? curr)
        (byte-array result)
        (recur (into result (write-base64 curr)) (nextval curr))))))
 
(defn encode-string
  "Convert to BASE64(6bit) byte array from string."
  [x]
  (encode (string-to-bytes x)))

;;
;; BASE64 decoder.
;;

(defn read-base64
"This function is an algorithm of BASE64 decoding.Decoding to process by 4 characters unit."
  [^bytes array]
  (let [
      ;; decode char1
      d1 (fn [c1 c2]
             (bit-or
                 (bit-and (bit-shift-left  c1 2) 0xFC)
                 (bit-and (bit-shift-right c2 4) 0x03)))
        ;; decode char2
      d2 (fn [c2 c3]
             (bit-or
                 (bit-and (bit-shift-left  c2 4) 0xF0)
                 (bit-and (bit-shift-right c3 2) 0x0F)))
        ;; decode char3
      d3 (fn [c3 c4]
             (bit-or
                 (bit-and (bit-shift-left  c3 6) 0xC0)
                 (bit-and c4 0x3F)))
        [c1 c2 c3 c4] array
        _c1 (tbl-of c1)
        _c2 (tbl-of c2)
        _c3 (tbl-of c3)
        _c4 (tbl-of c4)]
  ;; writing 1-3 characters of string.
  (cond
    (and (=    BASE64_PAD c3) (= BASE64_PAD c4)) (conj [] (d1 _c1 _c2))
    (and (not= BASE64_PAD c3) (= BASE64_PAD c4)) (conj [] (d1 _c1 _c2) (d2 _c2 _c3))
    :else                                        (conj [] (d1 _c1 _c2) (d2 _c2 _c3) (d3 _c3 _c4)))))

(defn decode
  "Convert to string(8bit) byte array from BASE64(6bit) byte array."
  [^bytes array]
  (let [nextval (fn [x] (next (next (next (next x)))))]
    (loop [result [] curr array]
      (if (nil? curr)
        (byte-array result)
        (recur (into result (read-base64 curr)) (nextval curr))))))

(defn decode-string
  "Convert to string(8bit) byte array from BASE64(6bit) string."
  [x]
  (decode (string-to-bytes x)))
