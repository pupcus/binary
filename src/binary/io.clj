(ns binary.io
  (:require [binary.core :as binary])
  (:import java.nio.ByteOrder))

;;
;; endian byte orders
;;

(def byte-order
     {
      :big    ByteOrder/BIG_ENDIAN
      :little ByteOrder/LITTLE_ENDIAN
      })

;;
;; methods for reading binary data buffers
;;

(defn read-dispatch [_ [_ _ type _]] type)

(defmulti bin-read read-dispatch)

(defmethod bin-read :int [buf [_ _ _ endian]]
           (.order buf (endian byte-order))
           (.getInt buf))

(defmethod bin-read :long [buf [_ _ _ endian]]
           (.order buf (endian byte-order))
           (.getLong buf))

(defmethod bin-read :float [buf [_ _ _ endian]]
           (.order buf (endian byte-order))
           (.getFloat buf))

(defmethod bin-read :double [buf [_ _ _ endian]]
           (.order buf (endian byte-order))
           (.getDouble buf))

(defmethod bin-read :cstring [buf [_ n :as form]]
           (loop [i (dec n)
                  s (conj [] (.get buf))]
             (if (or (= (last s) 0)
                     (<= i 0))
               (String. (binary/to-byte-array (if (= (last s) 0) (butlast s) s)))
               (recur (dec i) (conj s (.get buf))))))

(defmethod bin-read :vstring [buf [_ n :as form]]
           (let [size (min (.getInt buf) n)
                 result (byte-array size)
                 _      (.get buf result 0 size)]
             (String. result)))

(defmulti read-buffer read-dispatch)

(defmethod read-buffer :cstring [buf form]
           (bin-read buf form))

(defmethod read-buffer :vstring [buf form]
           (bin-read buf form))

(defmethod read-buffer :default [buf [_ n :as form]]
  (let [vec (into [] (for [_ (range n)] (bin-read buf form)))]
    (if (= n 1)
      (first vec)
      vec)))

(defn parse-buffer [buf forms]
  (apply hash-map
         (mapcat (fn [[name _ _ _ func :as form]]
                   (let [data (read-buffer buf form)]
                     (if (nil? name)
                       nil
                       [name (if func
                                     (func data)
                                     data)])))
                 forms)))

;;
;; methods for writing binary data buffers
;;

(defn write-dispatch [_ [_ _ type _] _] type)

(defmulti bin-write write-dispatch)

(defmethod bin-write :int [buf [_ _ _ endian] data]
  (.order buf (endian byte-order))
  (.putInt buf data))

(defmethod bin-write :long [buf [_ _ _ endian] data]
  (.order buf (endian byte-order))
  (.putLong buf data))

(defmethod bin-write :float [buf [_ _ _ endian] data]
  (.order buf (endian byte-order))
  (.putFloat buf data))

(defmethod bin-write :double [buf [_ _ _ endian] data]
  (.order buf (endian byte-order))
  (.putDouble buf data))

(defmethod bin-write :cstring [buf [_ n :as form] data]
           (let [bytes (.getBytes data)]
             (.put buf bytes 0 (min (count bytes) (dec n)))
             (.put buf (byte 0))))

(defmethod bin-write :vstring [buf [_ n :as form] data]
           (let [bytes (.getBytes data)
                 size (min (count bytes) n)]
             (.putInt buf size)
             (.put buf bytes 0 size)))

(defmulti write-buffer write-dispatch)

(defmethod write-buffer :cstring [buf form data]
           (bin-write buf form data))

(defmethod write-buffer :vstring [buf form data]
           (bin-write buf form data))

(defmethod write-buffer :default [buf [_ n :as form] data]
           (if (> n 1)
             (doseq [i (range n)]
               (let [piece (nth data i)] (bin-write buf form piece)))
             (bin-write buf form data)))

(defn pack-buffer [buf forms datamap]
  (mapcat (fn [[k _ _ _ func :as form]]
            (let [data (k datamap)
                  data (if func (func data) data)]
              (write-buffer buf form data)))
          forms)
  buf)




