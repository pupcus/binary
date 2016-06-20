(ns binary.utils-test
  (:require [binary.utils :refer :all]
            [clojure.test :refer :all]))

;;
;; testing making various byte arrays
;;

(deftest test-char-byte-array
  (let [ba (to-byte-array [\a \b \c \d])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [97 98 99 100]))))

(deftest test-string-byte-array
  (let [ba (to-byte-array ["test"])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [116 101 115 116]))))

(deftest test-cstring-byte-array
  (let [ba (to-byte-array [["test" nil]])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [116 101 115 116 0]))))

(deftest test-vstring-byte-array
  (let [ba (to-byte-array [[(int32 4) "test"]])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 4 116 101 115 116]))))

(deftest test-byte-byte-array
  (let [ba (to-byte-array [(int8 1)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [1]))))

(deftest test-short-byte-array
  (let [ba (to-byte-array [(int16 1)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 1]))))

(deftest test-int-byte-array
  (let [ba (to-byte-array [(int32 0x01020304)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [1 2 3 4]))))

(deftest test-long-byte-array
  (let [ba (to-byte-array [(int64 0x01020304)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 0 1 2 3 4]))))

(deftest test-bigint-byte-array
  (let [ba (to-byte-array [(java.math.BigInteger/valueOf 10)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 1 10]))))

(deftest test-bigdecimal-byte-array
  (let [ba (to-byte-array [(java.math.BigDecimal. 1.1)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 22 2 -16 -90 -119 -15 -71 74 120 -15 29 49 -73 -85 -128 109 64 -79 1 77 63 109 89 0 0 0 51]))))

(deftest test-ratio-byte-array
  (let [ba (to-byte-array [(rationalize 1.1)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 1 11 0 0 0 1 10]))))

(deftest test-vector-byte-array
  (let [ba (to-byte-array [[10 20 30 40]])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 30 0 0 0 0 0 0 0 40]))))

(deftest test-mixed-byte-array
  (let [ba (to-byte-array [nil 10 (int32 0x01020304) (short 20) (byte 10) "test" \a])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 0 0 0 0 0 10 1 2 3 4 0 20 10 116 101 115 116 97]))))

(deftest test-map-byte-array
  (let [ba (to-byte-array [{:somekey 10} {:somekey (int32 10)} {:somekey (int32 10) :endian :little} {:somekey 10 :endian :little}])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [0 0 0 0 0 0 0 10 0 0 0 10 10 0 0 0 10 0 0 0 0 0 0 0]))))

(deftest test-map-with-string-byte-array
  (let [ba (to-byte-array [{:somekey "test" :endian :little} {:somekey "test"}])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [116 101 115 116 116 101 115 116]))))

(deftest test-mixes-byte-array
  (let [ba (to-byte-array ["test" nil (vec (map int32 [10 20 30 40])) [\a [\b "test" (long 10)]] {:data 0xFF :endian :big} (byte 3)])]
    (is (= byte-array-class (class ba)))
    (is (= (vec ba) [116 101 115 116 0 0 0 0 10 0 0 0 20 0 0 0 30 0 0 0 40 97 98 116 101 115 116 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 -1 3]))))

;;
;; testing bit-set?
;;

(deftest bit-set?-test
  (let [n 0x7000000000000001]
    (is (true? (bit-set? 62 n)))
    (is (false? (bit-set? 1 n)))
    (is (true? (bit-set? 0 n)))))

