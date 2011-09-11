(ns binary.test.io
  (:use binary.io)
  (:use clojure.test)
  (:require [binary.core :as core]))

(def definition [[:command-length    1  :int       :big]
                 [:command-id        1  :int       :big]
                 [:command-status    1  :int       :big]
                 [:sequence-number   1  :int       :big]
                 [:fraction          1  :double    :big]
                 [:percentage        1  :double    :big]
                 [:average           1  :float     :big]
                 [:points            4  :int       :big]
                 [:text              44 :vstring   :any]
                 [:body              12 :cstring   :any]])

(def expected (core/to-byte-array [16 2 0 1 0.9312 10.88745 (float 1.1) 10 20 30 40 4 "text" "test string" nil]))

(deftest binary-buffer-read
  (let [data expected
        buff (.flip (.put (java.nio.ByteBuffer/allocate (count data)) data))
        results (parse-buffer buff definition)]
    (is (= 16             (:command-length results)))
    (is (= 2              (:command-id results)))
    (is (= 0              (:command-status results)))
    (is (= 1              (:sequence-number results)))
    (is (= 0.9312         (:fraction results)))
    (is (= 10.88745       (:percentage results)))
    (is (= [10 20 30 40]  (:points results)))
    (is (= "text"         (:text results)))
    (is (= "test string"  (:body results)))))

(deftest binary-buffer-write
  (let [data  { :command-length 16
               :command-id 2
               :command-status 0
               :sequence-number 1
               :points  [10 20 30 40]
               :fraction 0.9312
               :percentage 10.88745
               :average (float 1.1)
               :text "text"
               :body "test string longer than it needs to be"}
        
        buff (java.nio.ByteBuffer/allocate 1000)
        results (pack-buffer buff definition data)
        actual  (byte-array (.limit (.flip buff)))
        _       (.get buff actual (.position results) (.limit results))]
    (is (= (vec expected) (vec actual)))))
