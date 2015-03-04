(ns miner.test-roman
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pprint]
            [miner.roman :refer :all]))


;; clojure.pprint/cl-format works but is slow
(defn roman-cl [n]
  (pprint/cl-format nil "~@R" n))

(deftest roman-4k
  (doseq [n (range 1 4000)]
    (is (= (roman-cl n) (roman-numeral n)))))

(deftest sample-roman-numerals
  (testing "Roman Numberal Conversion" 
    (are [rn n] (= rn (roman-numeral n)) 
         "I" 1 
         "II" 2
         "IV" 4
         "V"  5
         "VI" 6
         "IX" 9
         "X" 10
         "XVII" 17
         "XXXVIII" 38
         "CCCXCIX" 399
         "MCMXLIX" 1949 )))

(deftest parse-roman-test
  (doseq [n (range 1 4000)]
    (is (= n (parse-roman (roman-numeral n))))))

;; Some malformed Roman numerals are not worth the effort to reject, but we try to catch
;; a few non-standard forms.
(deftest parse-errors
  (testing "Throws on parsing illegal Roman numerals"
    (are [err msg rn] (thrown-with-msg? err msg (parse-roman rn))
         IllegalArgumentException #".* an illegal Roman numeral.*"  "VV"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "CMCVVI"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "IVI"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "IXXI"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "IXCXI"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "XCX"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "VCI"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "CCMIII"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "LCIV"
         IllegalArgumentException #".* an illegal Roman numeral.*"  "IC")))

