(ns binary.core
  (:refer-clojure :exclude [read])
  (:import java.nio.ByteOrder)
  (:use binary.utils))


  ;;
  ;; multimethod for extracting value of a param
  ;; could be value, keyword reference, or function
  ;; could be param map of :in and :out values
  ;;
  

(defn callable? [c]
  (or (fn? c)
      (keyword? c)))

(defn validate [r]
  (if (instance? java.lang.Number r)
      r
      (throw (Exception. (str "value of result must evaluate to a number: result =[" r "]")))))

(defn value-dispatch-fn [target data direction]
  (class target))

(defmulti value value-dispatch-fn)

(defmethod value clojure.lang.PersistentArrayMap [{:keys [out in] :as target} data direction]
  (cond
     (and out (= direction :writing))  (validate (if (callable? out) (value out data :writing) out))
     (and in  (= direction :reading))  (validate (if (callable? in) (value in data :writing) in))
     :else (throw (Exception. (format "unable to find in or out for value map [%s]" target)))))

(defmethod value :default [target data direction]
  (validate (if (callable? target) (target data) target)))


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

(defmethod read :cstring [buf {:keys [size endian] :or {endian :big}}]
  (if (nil? size) (throw (Exception. "size required for cstring type")))
  (loop [i (dec size)
         s (conj [] (.get buf))]
    (if (or (= (last s) 0)
            (<= i 0))
      (String. (to-byte-array (if (= (last s) 0) (butlast s) s)))
      (recur (dec i) (conj s (.get buf))))))

(defmethod read :vstring [buf {:keys [size endian] :or {endian :big}}]
  (if (nil? size) (throw (Exception. "size required for vstring type")))
  (let [bsize (.getInt (.order buf (endian byte-order)))
        result (byte-array bsize)
        _      (.get buf result 0 bsize)]
    (String. result 0 (min size bsize))))

(defn decode-buffer [buf {:keys [number] :or {number 1} :as form}]
  (let [result (into [] (for [_ (range number)] (read buf form)))]
    (if (= number 1)
      (first result)
      result)))

(defn process-form-decode [buf {:keys [name number size in] :as form} sofar]
  (let [form (merge form (if number {:number (value number sofar :reading)}) (if size {:size (value size sofar :reading)}))]
    (let [data (decode-buffer buf form)]
      (if name
        [name (if in (in (merge sofar {name data})) data)]
        nil))))

(defn decode-dispatch [buf _]
  (class buf))

(defmulti decode decode-dispatch)

(defmethod decode java.nio.ByteBuffer [buf forms]
  (loop [sofar {}
         forms forms]
    (if (nil? (seq forms))
      sofar
      (recur (conj sofar (process-form-decode buf (first forms) sofar)) (rest forms)))))

(defmethod decode byte-array-class [bytes forms]
  (let [buf (java.nio.ByteBuffer/wrap bytes)]
    (decode buf forms)))


  ;;
  ;; methods for writing binary data buffers
  ;;
  
(defn write-dispatch [_ {:keys [type]} _] type)

(defmulti write write-dispatch)

(defmethod write :int [buf {:keys [endian] :or {endian :big}} data]
  (let [bytes (to-byte-array [{:data (int32 data) :endian endian}])]
    (.write buf bytes 0 (count bytes))))

(defmethod write :long [buf {:keys [endian] :or {endian :big}} data]
  (let [bytes (to-byte-array [{:data data :endian endian}])]
    (.write buf bytes 0 (count bytes))))

(defmethod write :float [buf {:keys [endian] :or {endian :big}} data]
  (let [bytes (to-byte-array [{:data data :endian endian}])]
    (.write buf bytes 0 (count bytes))))

(defmethod write :double [buf {:keys [endian] :or {endian :big}} data]
  (let [bytes (to-byte-array [{:data data :endian endian}])]
    (.write buf bytes 0 (count bytes))))

(defmethod write :cstring [buf {:keys [size]} data]
  (let [data-bytes (.getBytes data)]
    (.write buf data-bytes 0 (min (count data-bytes) (dec size)))
    (.write buf (int 0))))

(defmethod write :vstring [buf {:keys [size endian] :or {endian :big}} data]
  (let [bytes (.getBytes data)
        size (min (count bytes) size) 
        size-bytes (to-byte-array [{:data (int32 size) :endian endian}])]
    (.write buf size-bytes 0 (count size-bytes))
    (.write buf bytes 0 size)))

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

(defn encode [forms datamap]
  (let [buf (java.io.ByteArrayOutputStream. 1024)]
    (loop [sofar datamap
           forms forms]
      (if (nil? (seq forms))
        (.toByteArray buf)
        (recur (merge sofar (process-form-encode buf (first forms) sofar)) (rest forms))))))


