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


(defn size-bits [#^java.lang.Number n]
  (eval (list '. (class n) 'SIZE)))

(defn size-bytes [#^java.lang.Number n]
  (/ (size-bits n) bits-in-octet))

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

(defn to-bytes-dispatch
  ([n]   (class n))
  ([n _] (to-bytes-dispatch n)))

(defmulti to-bytes to-bytes-dispatch)

(defmethod to-bytes nil
  ([_]   (byte 0))
  ([_ _] (byte 0)))

(defmethod to-bytes java.lang.Byte
  ([b]   [b])
  ([b _] [b]))

(defmethod to-bytes java.lang.Character
  ([c]   [(as-byte (int c))])
  ([c _] (to-bytes c)))

(defmethod to-bytes java.lang.Integer
  ([i]   (to-bytes i :big))
  ([i e] (vec (.array (.flip (.putInt (.order (java.nio.ByteBuffer/allocate int-size) (byte-order e)) i))))))

(defmethod to-bytes java.lang.Long
  ([l]   (to-bytes l :big))
  ([l e] (vec (.array (.flip (.putLong (.order (java.nio.ByteBuffer/allocate long-size) (byte-order e)) l))))))

(defmethod to-bytes java.lang.Float
  ([f]   (to-bytes f :big))
  ([f e] (vec (.array (.flip (.putFloat (.order (java.nio.ByteBuffer/allocate float-size) (byte-order e)) f))))))

(defmethod to-bytes java.lang.Double
  ([d]   (to-bytes d :big))
  ([d e] (vec (.array (.flip (.putDouble (.order (java.nio.ByteBuffer/allocate double-size) (byte-order e)) d))))))

(defmethod to-bytes java.lang.String
  ([s]   (vec (map to-bytes s)))
  ([s _] (to-bytes s)))

(defmethod to-bytes clojure.lang.IPersistentVector
  ([v]        (to-bytes v :big))
  ([v endian] (vec (flatten (map #(to-bytes % endian) v)))))

(defmethod to-bytes clojure.lang.IPersistentMap
  ([{:keys [endian] :or {endian :big} :as data}] (to-bytes (dissoc data :endian) endian))
  ([data endian]
     (if (> (count data) 1)
       (throw (Exception. "only one data element allowed in map with optional :endian setting ie. {:data <value> <:endian <endian>>}"))
       (vec (flatten (map (fn [[k v]] (to-bytes v endian)) data))))))

(def byte-array-class (class (.getBytes "")))

(defmethod to-bytes byte-array-class
  ([ba]   (vec ba))
  ([ba _] (vec ba)))

(defn to-byte-array [xs]
  (byte-array (flatten (vec (map to-bytes xs)))))

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

(defn safe-write [buf {:keys [name type number size] :or {number 1 size 1}} data]
  (let [bytes-needed (min (* number size (sizes type)) (count (to-byte-array (name data))))]
    (if (< bytes-needed (.remaining buf))
      (.resize buf (+ (.limit buf) bytes-needed)))))

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


;;
;; take any number and encode it to bytes
;;

(defn little-endian-bytes [#^java.lang.Number n]
  (let [size (size-bytes n)]
    (loop [bytes [] count 0]
      (if (< count size)
        (recur (conj bytes (as-byte (bit-shift-right n (* count bits-in-octet)))) (inc count))
        bytes))))

(defn big-endian-bytes [#^java.lang.Number n]
  (let [size (size-bytes n)]
    (loop [bytes [] count size]
      (if (> count 0)
        (recur (conj bytes (as-byte (bit-shift-right n (* (dec count) bits-in-octet)))) (dec count))
        bytes))))
