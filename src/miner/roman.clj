(ns miner.roman
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; Originally inspired by this blog post:
;; http://www.jayway.com/2012/08/04/a-decimal-to-roman-numeral-converter-in-just-a-few-lines/

(defn range-down
  "Returns a seq of integers from HIGH (exclusive) down to LOW (inclusive).
   LOW defaults to 0. STEP is a positve decrement, defaults to 1.  Like
   `(reverse (range low high step))' but a bit faster."
  ([high] (range (dec high) -1 -1))
  ([high low] (range (dec high) (dec low) -1))
  ([high low step]
     ;; calculate nearest multiple of step + offset using mod
     (let [hi (dec high)
           top (- hi (mod (- hi low) step))]
       (range top (dec low) (- step)))))

(defn roman-lookup [c pow10]
  (nth (case c
         \1 ["I" "X" "C" "M"]
         \2 ["II" "XX" "CC" "MM"]
         \3 ["III" "XXX" "CCC" "MMM"]
         \4 ["IV" "XL" "CD"]
         \5 ["V" "L" "D"]
         \6 ["VI" "LX" "DC"]
         \7 ["VII" "LXX" "DCC"]
         \8 ["VIII" "LXXX" "DCCC"]
         \9 ["IX" "XC" "CM"]
         \0 ["" "" "" ""])
       pow10))

(defn roman-numeral [n]
  {:pre [(< 0 n 4000)]}
  (let [digits (seq (str n))]
    (apply str (map roman-lookup digits (range-down (count digits))))))


(def roman-map {1000 "M" 900 "CM" 500 "D" 400 "CD"
                100 "C" 90 "XC" 50 "L" 40 "XL"
                10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"})

(def roman-char-map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1})

(def roman-negs {1000 100, 500 100, 100 10, 50 10, 10 1, 5 1})

(defn roman-sign [v prev]
  ;; return string to indicate error
  (cond (>= v prev) v
        (not= v (get roman-negs prev)) (str (roman-map v) (roman-map prev))
        :else  (- v)))

(defn roman-values-seq [rn]
  (let [vs (rseq (conj (mapv #(get roman-char-map % (str %)) (str/upper-case rn)) 0))]
    ;; going in reverse order is easier,
    ;; padded zero to simplify prev
    (if (seq (filter string? vs))
      ;; return vs with strings marking errors
      vs
      (map roman-sign (rest vs) vs))))


(defn has-double? [fiver vs]
  (> (count (filter #{fiver} vs)) 1))

(defn has-double-fiver? [vs]
  ;; you only want one of each five-ish numeral
  (or (has-double? 5 vs)
      (has-double? 50 vs)
      (has-double? 500 vs)))

(defn remove-adjacent-dups [vs]
  ;; vs is a sequense of roman addends (ints)
  ;; returns seq with at most one in a row of any number
  (loop [vs vs rs [] prev 0]
    (if-let [v (first vs)]
      (if (== v prev)
        (recur (rest vs) rs prev)
        (recur (rest vs) (conj rs v) (long v)))
      (seq rs))))

(defn has-bad-neg? [vs]
  ;; idea is to catch things like IXXI or XCX which are non-standard
  (let [vs (remove-adjacent-dups vs)]
    (and (seq (filter (fn [[a _ b]] (== a (- b))) (partition 3 1 vs))) true)))

(defn monotonic? [vs]
  ;; drops all negs first
  (apply <= (filter pos? vs)))

(defn roman-values-error [roman-values]
  ;; returns falsey if no errors, true for an invalid roman numeral,
  ;; or a specific string if the error is isolated.
  ;; roman-values as given by roman-values-seq;
  ;; numbers are OK, strings are error components;
  ;; reports only first suspicious string.
  (or (first (rseq (filterv string? roman-values)))
      ;; no parse errors, so check for illegal forms
      (not (monotonic? roman-values))
      (has-double-fiver? roman-values)
      (has-bad-neg? roman-values)))

(defn parse-roman [rn]
  "Parse the string rn as a Roman numeral.  Throws IllegalArgumentException
   if rn does not parse as a Roman numeral."
  (let [vs (roman-values-seq rn)]
    (when-let [error (roman-values-error vs)]
      (if (true? error)
        (throw (IllegalArgumentException. (str "'" rn "' is an illegal Roman numeral")))
        (throw (IllegalArgumentException. 
                (str "'" rn "' is an illegal Roman numeral, problem near '" error "'")))))
    (reduce + vs)))

(defn parse-roman-soft [rn]
  "Parse the string rn as a Roman numeral.  Returns original rn
   string if rn does not look like a Roman numeral.  Use parse-roman if
   you want to throw an exception on a parse error."
  (let [vs (roman-values-seq rn)]
    (if (or (seq (filter string? vs))
            (not (monotonic? vs)))
      ;; return orginal string for failure
      rn
      (reduce + vs))))

(defn parse-roman-no-check [rn]
  "Parse the string rn as a Roman numeral without error checking.  The result is undefined
   if rn is not a valid Roman numeral.  Use parse-roman if you want to throw an exception 
   on a parse error."
  (reduce + (roman-values-seq rn)))

;; suitable for a tagged literal (mostly just for fun)
(defn roman-numeral-reader [s]
  "Data-reader for Roman numerals.  Takes a symbol or string s as the argument 
   and returns a Long.  Throws IllegalArgumentException if the argument cannot 
   be parsed as Roman numeral."
  (parse-roman (name s)))

;; For example as a macro, but probably not a good idea because it gives a non-standard look.
(defmacro roman2 [s]
  (let [v (parse-roman (name s))]
    `~v))

;; Bad to break lexical interpretation of the symbol
(defmacro roman1 [s]
  (if (symbol? s)
    `(parse-roman (name '~s))
    `(parse-roman ~s)))

;; (let [mx "XIV"] (roman1 mx))
;; =>  1010


