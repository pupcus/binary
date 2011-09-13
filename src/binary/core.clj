(ns binary.core
  (:refer-clojure :exclude [read])
  (:import java.nio.ByteOrder))

;;
;; various sizes and utilities for sizes
;;

(def bits-in-octet 8)

(def byte-size    (/ java.lang.Byte/SIZE bits-in-octet))
(def char-size    (/ java.lang.Character/SIZE bits-in-octet))
(def short-size   (/ java.lang.Short/SIZE bits-in-octet))
(def int-size     (/ java.lang.Integer/SIZE bits-in-octet))
(def long-size    (/ java.lang.Long/SIZE bits-in-octet))
(def float-size   (/ java.lang.Float/SIZE bits-in-octet))
(def double-size  (/ java.lang.Double/SIZE bits-in-octet))

(def sizes
  {
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

;;
;; multimethod for extracting value of a param
;; could be value, keyword reference, or function
;; could be param map of :in and :out values
;;


(defn value-dispatch-fn [target data direction]
  (class target))

(defmulti value value-dispatch-fn)

(defmethod value clojure.lang.PersistentArrayMap [{:keys [out in]} data direction]
  (cond
   (and out (= direction :writing)) (value out data :writing) 
   (and in  (= direction :reading)) (value in data :writing)
   :else data))

(defmethod value :default [target data direction]
  (if (or (fn? target)
          (keyword? target))
    (target data)
    target))


;;
;; methods for reading binary data buffers
;;

(defn read-dispatch [_ {:keys [type]}] type)

(defmulti read read-dispatch)

(defmethod read :int [buf {:keys [endian] :or {endian :big}}]
  (.order buf (endian byte-order))
  (.getInt buf))

(defmethod read :long [buf {:keys [endian] :or {endian :big}}]
  (.order buf (endian byte-order))
  (.getLong buf))

(defmethod read :float [buf {:keys [endian] :or {endian :big}}]
  (.order buf (endian byte-order))
  (.getFloat buf))

(defmethod read :double [buf {:keys [endian] :or {endian :big}}]
  (.order buf (endian byte-order))
  (.getDouble buf))

(defmethod read :cstring [buf {:keys [size]}]
  (if (nil? size) (throw (Exception. "size required for cstring type")))
  (loop [i (dec size)
         s (conj [] (.get buf))]
    (if (or (= (last s) 0)
            (<= i 0))
      (String. (to-byte-array (if (= (last s) 0) (butlast s) s)))
      (recur (dec i) (conj s (.get buf))))))

(defmethod read :vstring [buf {:keys [size]}]
  (if (nil? size) (throw (Exception. "size required for cstring type")))
  (let [size (min (.getInt buf) size)
        result (byte-array size)
        _      (.get buf result 0 size)]
    (String. result)))

(defn decode-buffer [buf {:keys [number] :or {number 1} :as form}]
  (let [vec (into [] (for [_ (range number)] (read buf form)))]
    (if (= number 1)
      (first vec)
      vec)))

(defn process-form-decode [buf {:keys [name number size in] :as form} sofar]
  (let [form (merge form (if number {:number (value number sofar :reading)}) (if size {:size (value size sofar :reading)}))
        data (decode-buffer buf form)]
    (if name
      [name (if in (in (merge sofar {name data})) data)]
      nil)))

(defn decode [buf forms]
  (loop [sofar {}
         forms forms]
    (if (nil? (seq forms))
      sofar
      (recur (conj sofar (process-form-decode buf (first forms) sofar)) (rest forms)))))

;;
;; methods for writing binary data buffers
;;

(defn write-dispatch [_ {:keys [type]} _] type)

(defmulti write write-dispatch)

(defmethod write :int [buf {:keys [endian] :or {endian :big}} data]
  (.order buf (endian byte-order))
  (.putInt buf data))

(defmethod write :long [buf {:keys [endian] :or {endian :big}} data]
  (.order buf (endian byte-order))
  (.putLong buf data))

(defmethod write :float [buf {:keys [endian] :or {endian :big}} data]
  (.order buf (endian byte-order))
  (.putFloat buf data))

(defmethod write :double [buf {:keys [endian] :or {endian :big}} data]
  (.order buf (endian byte-order))
  (.putDouble buf data))

(defmethod write :cstring [buf {:keys [size]} data]
  (let [bytes (.getBytes data)]
    (.put buf bytes 0 (min (count bytes) (dec size)))
    (.put buf (byte 0))))

(defmethod write :vstring [buf {:keys [size]} data]
  (let [bytes (.getBytes data)
        size (min (count bytes) size)]
    (.putInt buf size)
    (.put buf bytes 0 size)))

(defn encode-buffer [buf {:keys [number] :or {number 1} :as form} data]
  (if (> number 1)
    (doseq [i (range number)] (let [piece (nth data i)] (write buf form piece)))
    (write buf form data)))

(defn process-form-encode [buf {:keys [name number size out] :as form} datamap]
  (let [form (merge form (if number {:number (value number datamap :writing)}) (if size {:size (value size datamap :writing)}))
        data (name datamap)
        data (if out (out (merge datamap {name data})) data)]
    (encode-buffer buf form data)
    {name data}))

(defn encode [buf forms datamap]
  (loop [sofar datamap
         forms forms]
    (if (nil? (seq forms))
      buf
      (recur (merge sofar (process-form-encode buf (first forms) sofar)) (rest forms)))))

