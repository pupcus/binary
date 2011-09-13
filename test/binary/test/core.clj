(ns binary.test.core
  (:refer-clojure :exclude [read])
  (:use [binary.core])
  (:use [clojure.test]))

;;
;; testing making byte array
;;

(deftest to-byte-array-test
  (let [ba (to-byte-array [(byte 1) "test" \a \b \c nil 0x7FFFFFFF])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [1 116 101 115 116 97 98 99 0 127 -1 -1 -1]))))

;;
;; testing bit-set?
;;

(deftest bit-set?-test
  (let [n 0x800000000000000000000000000000008000000000000001]
    (is (true? (bit-set? 191 n)))
    (is (true? (bit-set? 63 n)))
    (is (false? (bit-set? 1 n)))
    (is (true? (bit-set? 0 n)))))

;;
;; test encoding and decoding
;;

(def id-to-kw
  {
   0x00000000 :generic_nack
   0x0000000b :outbind
   })

(def kw-to-id
  {
   :generic_nack 0x00000000
   :outbind      0x0000000b})

(def expected (to-byte-array [16 11 2 1 0.9312 10.88745 (float 1.1) [10 20] [4 "text"] ["test string" nil]]))

(def definition [{:name :command-length    :type :int}
                 {:name :command-id        :type :int :out #(kw-to-id (:command-id %)) :in #(id-to-kw (:command-id %))}
                 {:name :command-status    :type :int :out #(count (:points %))}
                 {:name :sequence-number   :type :int}
                 {:name :fraction          :type :double}
                 {:name :percentage        :type :double}
                 {:name :average           :type :float}
                 {:name :points            :type :int  :number :command-status}
                 {:name :text              :type :vstring :size 44}
                 {:name :body              :type :cstring :size 12}])

(deftest encoding-test
  (let [data expected
        buff (.flip (.put (java.nio.ByteBuffer/allocate 1000) data))
        results (decode buff definition)]
    (is (= 16             (:command-length results)))
    (is (= :outbind       (:command-id results)))
    (is (= 2              (:command-status results)))
    (is (= 1              (:sequence-number results)))
    (is (= 0.9312         (:fraction results)))
    (is (= 10.88745       (:percentage results)))
    (is (= (float 1.1)    (:average results)))
    (is (= [10 20]        (:points results)))
    (is (= "text"         (:text results)))
    (is (= "test string"  (:body results)))))

(deftest decoding-test
  (let [data  { :command-length 16
               :command-id :outbind
               :command-status nil
               :sequence-number 1
               :points  [10 20]
               :fraction 0.9312
               :percentage 10.88745
               :average (float 1.1)
               :text "text"
               :body "test string longer than it needs to be"}
        
        buff (java.nio.ByteBuffer/allocate 1000)
        results (encode buff definition data)
        actual  (byte-array (.limit (.flip buff)))
        _       (.get buff actual (.position results) (.limit results))]
    (is (= (vec expected) (vec actual)))))
