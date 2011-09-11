(ns binary.core)

(def bits-in-octet 8)

(def byte-size    (/ java.lang.Byte/SIZE bits-in-octet))
(def char-size    (/ java.lang.Character/SIZE bits-in-octet))
(def short-size   (/ java.lang.Short/SIZE bits-in-octet))
(def int-size     (/ java.lang.Integer/SIZE bits-in-octet))
(def long-size    (/ java.lang.Long/SIZE bits-in-octet))
(def float-size   (/ java.lang.Float/SIZE bits-in-octet))
(def double-size  (/ java.lang.Double/SIZE bits-in-octet))

(defn as-byte [i]
  (byte (let [i (bit-and 0xFF i)]
          (if (> i 127)
            (- i 256)
            i))))

(defmulti to-bytes class)

(defmethod to-bytes nil [_] (byte 0))

(defmethod to-bytes java.lang.Byte [b] [b])

(defmethod to-bytes java.lang.Character [c]
           [(as-byte (int c))])

(defmethod to-bytes java.lang.Integer [i]
           (vec (.array (.flip (.putInt (java.nio.ByteBuffer/allocate int-size) i)))))

(defmethod to-bytes java.lang.Long [l]
           (vec (.array (.flip (.putInt (java.nio.ByteBuffer/allocate long-size) l)))))

(defmethod to-bytes java.lang.Float [f]
           (vec (.array (.flip (.putFloat (java.nio.ByteBuffer/allocate float-size) f)))))

(defmethod to-bytes java.lang.Double [d]
           (vec (.array (.flip (.putDouble (java.nio.ByteBuffer/allocate double-size) d)))))

(defmethod to-bytes java.lang.String [s]
           (vec (map to-bytes s)))

(defmethod to-bytes clojure.lang.PersistentVector [v]
           (vec (map to-bytes v)))

(def byte-array-class (class (.getBytes "")))

(defmethod to-bytes byte-array-class [ba]
           (vec ba))

(defn to-byte-array [xs]
  (byte-array (flatten (map to-bytes xs))))

(defn bit-set? [bn n]
  (let [check (bit-shift-left (long 1) bn)]
    (= (bit-and n check) check)))
