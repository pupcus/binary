(ns binary.test.core
  (:use [binary.core])
  (:use [clojure.test]))

(deftest byte-array-generation
  (let [ba (to-byte-array [(byte 1) "test" \a \b \c nil 0x7FFFFFFF])]
    (is (= binary.core/byte-array-class (class ba)))
    (is (= (vec ba) [1 116 101 115 116 97 98 99 0 127 -1 -1 -1]))))

(deftest checking-bits
  (let [n 0x800000000000000000000000000000008000000000000001]
    (is (true? (bit-set? 191 n)))
    (is (true? (bit-set? 63 n)))
    (is (false? (bit-set? 1 n)))
    (is (true? (bit-set? 0 n)))))
