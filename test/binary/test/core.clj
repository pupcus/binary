(ns binary.test.core
  (:refer-clojure :exclude [read])
  (:use [clojure.test])
  (:use [binary.core])
  (:require [binary.utils :as utils]))

;;
;; testing the (value) multimethod
;;

(deftest value-simple-number
  (let [target 10
        data   {:data 20}
        direction :reading]
    (is (= 10 (value target data direction)))))

(deftest value-from-keyword-simple-number
  (let [target :data
        data   {:data 20}
        direction :reading]
    (is (= 20 (value target data direction)))))

(deftest value-string-exception
  (let [target "error"
        data   {:data 20}
        direction :reading]
    (is (thrown? Exception (value target data direction)))))

(deftest value-from-keyword-string-exception
  (let [target :data
        data   {:data "error"}
        direction :reading]
    (is (thrown? Exception (value target data direction)))))

(deftest value-in-from-function
  (let [target {:in #(+ (:data %) 5) :out :nothing}
        data   {:data 10}
        direction :reading]
    (is (= 15 (value target data direction)))))

(deftest value-in-from-keyword
  (let [target {:in :data :out :nothing}
        data   {:data 30}
        direction :reading]
    (is (= 30 (value target data direction)))))

(deftest value-in-from-function-exception
  (let [target {:in #(:data %) :out :nothing}
        data   {:data "error"}
        direction :reading]
    (is (thrown? Exception (value target data direction)))))

(deftest value-in-from-keyword-exception
  (let [target {:in :data :out :nothing}
        data   {:data "error"}
        direction :reading]
    (is (thrown? Exception (value target data direction)))))

(deftest value-out-from-function
  (let [target {:out #(+ (:data %) 5) :in :nothing}
        data   {:data 10}
        direction :writing]
    (is (= 15 (value target data direction)))))

(deftest value-out-from-keyword
  (let [target {:out :data :in :nothing}
        data   {:data 30}
        direction :writing]
    (is (= 30 (value target data direction)))))

(deftest value-out-from-function-exception
  (let [target {:out #(:data %) :in :nothing}
        data   {:data "error"}
        direction :writing]
    (is (thrown? Exception (value target data direction)))))

(deftest value-out-from-keyword-exception
  (let [target {:out :data :in :nothing}
        data   {:data "error"}
        direction :writing]
    (is (thrown? Exception (value target data direction)))))

;;
;; testing the read multimethod
;;


(deftest read-integer-form
  (let [val 10
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [val]))
        form {:name :data :type :int}]
    (is (= val (binary.core/read bb form)))))

(deftest read-integer-little-endian-form
  (let [val 10
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [{:data val :endian :little}]))
        form {:name :data :type :int :endian :little}]
    (is (= val (binary.core/read bb form)))))

(deftest read-long-form
  (let [val (long 0x00FFFFFFFFFFFFFF)
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [val]))
        form {:name :data :type :long}]
    (is (= val (binary.core/read bb form)))))

(deftest read-long-little-endian-form
  (let [val (long 0x00FFFFFFFFFFFFFF)
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [{:data val :endian :little}]))
        form {:name :data :type :long :endian :little}]
    (is (= val (binary.core/read bb form)))))

(deftest read-float-form
  (let [val (float 1.1)
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [val]))
        form {:name :data :type :float}]
    (is (= val (binary.core/read bb form)))))

(deftest read-float-little-endian-form
  (let [val (float 1.1)
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [{:data val :endian :little}]))
        form {:name :data :type :float :endian :little}]
    (is (= val (binary.core/read bb form)))))

(deftest read-double-form
  (let [val 1.1
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [val]))
        form {:name :data :type :double}]
    (is (= val (binary.core/read bb form)))))

(deftest read-double-little-endian-form
  (let [val 1.1
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [{:data val :endian :little}]))
        form {:name :data :type :double :endian :little}]
    (is (= val (binary.core/read bb form)))))

(deftest read-cstring-form
  (let [val "cstring"
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [[val nil]]))
        form {:name :data :type :cstring :size 100}]
    (is (= val (binary.core/read bb form)))))

(deftest read-vstring-form
  (let [val "vstring"
        bb (java.nio.ByteBuffer/wrap (utils/to-byte-array [[7 val]]))
        form {:name :data :type :vstring :size 100}]
    (is (= val (binary.core/read bb form)))))

;;
;; test the wrtie multimethod
;;

(deftest write-integer-form
  (let [val 10
        check (utils/to-bytes [val])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :int}
        _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-integer-little-endian-form
  (let [val 10
        check (utils/to-bytes [{:data val :endian :little}])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :int :endian :little}
           _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-long-form
  (let [val (long 20)
        check (utils/to-bytes [val])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :long}
        _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-long-little-endian-form
  (let [val (long 20)
        check (utils/to-bytes [{:data val :endian :little}])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :long :endian :little}
           _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-float-form
  (let [val (float 1.1)
        check (utils/to-bytes [val])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :float}
        _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-float-little-endian-form
  (let [val (float 1.1)
        check (utils/to-bytes [{:data val :endian :little}])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :float :endian :little}
           _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-double-form
  (let [val 1.1
        check (utils/to-bytes [val])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :double}
        _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-double-little-endian-form
  (let [val 1.1
        check (utils/to-bytes [{:data val :endian :little}])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :double :endian :little}
           _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-cstring-form
  (let [val "cstring"
        check (utils/to-bytes [[val nil]])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :cstring :size 100}
        _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

(deftest write-vstring-form
  (let [val "vstring"
        check (utils/to-bytes [[7 val]])
        buf (java.io.ByteArrayOutputStream.)
        form {:name :data :type :vstring :size 100}
        _    (binary.core/write buf form val)]
    (is (= check (vec (.toByteArray buf))))))

;;
;; test encoding and decoding
;;

(def id-to-kw
  {
   0x00000000 :nack
   0x0000000b :out
   })

(def kw-to-id
  {
   :nack 0x00000000
   :out  0x0000000b})

(def expected
  (utils/to-byte-array [16 11 2 {:data 1 :endian :little} 0.9312 10.88745 (float 1.1) [10 20] [4 "text"] ["test string" nil]]))

(def definition [{:name :command-length    :type :int}
                 {:name :command-id        :type :int :out #(kw-to-id (:command-id %)) :in #(id-to-kw (:command-id %))}
                 {:name :command-status    :type :int :out #(count (:points %))}
                 {:name :sequence-number   :type :int :endian :little}
                 {:name :fraction          :type :double}
                 {:name :percentage        :type :double}
                 {:name :average           :type :float}
                 {:name :points            :type :int  :number :command-status}
                 {:name :text              :type :vstring :size 44}
                 {:name :body              :type :cstring :size 12}])

(deftest decoding-test
  (let [results (decode expected definition)]
    (is (= 16             (:command-length results)))
    (is (= :out       (:command-id results)))
    (is (= 2              (:command-status results)))
    (is (= 1              (:sequence-number results)))
    (is (= 0.9312         (:fraction results)))
    (is (= 10.88745       (:percentage results)))
    (is (= (float 1.1)    (:average results)))
    (is (= [10 20]        (:points results)))
    (is (= "text"         (:text results)))
    (is (= "test string"  (:body results)))))

(deftest encoding-test
  (let [data  { :command-length 16
               :command-id :out
               :command-status 2
               :sequence-number 1
               :points  [10 20]
               :fraction 0.9312
               :percentage 10.88745
               :average (float 1.1)
               :text "text"
               :body "test string longer than it needs to be"}
        
        results (encode definition data)]
    (is (= (vec expected) (vec results)))))

(deftest roundtrip-test
  (let [data  { :command-length 16
               :command-id :out
               :command-status 2
               :sequence-number 1
               :points  [10 20]
               :fraction 0.9312
               :percentage 10.88745
               :average (float 1.1)
               :text "text"
               :body "test string"}]
    
    (is (= data (decode (encode definition data) definition)))))
