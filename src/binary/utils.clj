(ns binary.utils
  (:import java.nio.ByteOrder))

;;
;; various sizes and utilities for sizes
;;

(def bits-in-byte 8)

(def byte-size    (/ java.lang.Byte/SIZE bits-in-byte))
(def char-size    (/ java.lang.Character/SIZE bits-in-byte))
(def short-size   (/ java.lang.Short/SIZE bits-in-byte))
(def int-size     (/ java.lang.Integer/SIZE bits-in-byte))
(def long-size    (/ java.lang.Long/SIZE bits-in-byte))
(def float-size   (/ java.lang.Float/SIZE bits-in-byte))
(def double-size  (/ java.lang.Double/SIZE bits-in-byte))

(def sizes
  {
   :byte    byte-size
   :char    char-size
   :short   short-size
   :int     int-size
   :long    long-size
   :float   float-size
   :double  double-size
   :cstring byte-size
   :vstring byte-size
   })

;;
;; endian byte orders
;;

(def byte-order
  {
   :big    ByteOrder/BIG_ENDIAN
   :little ByteOrder/LITTLE_ENDIAN
   })

;;
;; utilities for building java byte arrays (byte[])

(defn as-byte [n]
  (byte
   (let [n (bit-and 0xFF n)]
     (if (> n 0x7F)
       (- n (inc 0xFF))
       n))))

(defn as-short [n]
  (short
   (let [n (bit-and 0xFFFF n)]
     (if (> n 0x7FFF)
       (- n (inc 0xFFFF))
       n))))

(defn as-int [n]
  (Integer/valueOf
   (let [n (bit-and 0xFFFFFFFF n)]
     (if (> n 0x7FFFFFFF)
       (- n (inc 0xFFFFFFFF))
       n))))

(defn int8 [n]
  (as-byte n))

(defn int16 [n]
  (as-short n))

(defn int32 [n]
  (as-int n))

(defn int64 [l]
  (Long/valueOf l))

(defmulti to-bytes class)

(defmethod to-bytes nil [_]   (byte 0))

(defmethod to-bytes java.lang.Byte [b] [b])

(defmethod to-bytes java.lang.Character [c]
  [(as-byte (int c))])

(defmethod to-bytes java.lang.Integer [i]
  (vec (.array (.flip (.putInt (java.nio.ByteBuffer/allocate int-size) i)))))

(defmethod to-bytes java.lang.Short [i]
  (vec (.array (.flip (.putShort (java.nio.ByteBuffer/allocate short-size) i)))))

(defmethod to-bytes java.lang.Long [l]
  (vec (.array (.flip (.putLong (java.nio.ByteBuffer/allocate long-size) l)))))

(defmethod to-bytes java.lang.Float [f]
  (vec (.array (.flip (.putFloat (java.nio.ByteBuffer/allocate float-size) f)))))

(defmethod to-bytes java.lang.Double [d]
  (vec (.array (.flip (.putDouble (java.nio.ByteBuffer/allocate double-size) d)))))

(defmethod to-bytes java.math.BigInteger [bi]
  (let [bytes (to-bytes (.toByteArray bi))]
    (flatten [(to-bytes (int32 (count bytes))) bytes])))

(defmethod to-bytes java.math.BigDecimal [bd]
  (flatten [(to-bytes (.unscaledValue bd)) (to-bytes (int32 (.scale bd)))]))

(defmethod to-bytes clojure.lang.Ratio [r]
  (flatten [(to-bytes (numerator r)) (to-bytes (denominator r))]))

(defmethod to-bytes java.lang.String [s]
  (vec (map to-bytes s)))

(defmethod to-bytes clojure.lang.IPersistentVector [v]
  (vec (flatten (map #(to-bytes %) v))))

(defn process-persistent-map [data endian]
  (if (> (count data) 1)
    (throw (Exception. "only one data element allowed in map with optional :endian setting ie. {:data <value> <:endian <endian>>}"))
    (let [[_ v] (first data)
          results (flatten (to-bytes v))]
      (if (and (= endian :little)
               (instance? java.lang.Number v)
               (not (or (instance? java.math.BigDecimal v)
                        (instance? java.math.BigInteger v)
                        (instance? clojure.lang.Ratio v))))
        (vec (reverse results))
        (vec results)))))

(defmethod to-bytes clojure.lang.IPersistentMap [{:keys [endian] :or {endian :big} :as data}]
  (process-persistent-map (dissoc data :endian) endian))

(def byte-array-class (class (.getBytes "")))

(defmethod to-bytes byte-array-class [ba] (vec ba))

(defn to-byte-array [xs] (byte-array (flatten (vec (map to-bytes xs)))))

(defn bit-set? [bn n]
  (let [check (bit-shift-left (long 1) bn)]
    (= (bit-and n check) check)))

