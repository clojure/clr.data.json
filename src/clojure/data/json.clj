;; Copyright (c) Stuart Sierra, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;;  Modified to run under ClojureCLR by David Miller
;;  January 1, 2012
;;  Changes are
;; Copyright (c) David Miller, 2012. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; Changes to Stuart Sierra's code are clearly marked.
;; An end-of-line comment start with  ;DM:  indicates a change on that line.
;; The commented material indicates what was in the original code or indicates a new line inserted.
;; For example, a comment such as the following
;;    (defn- read-json-array [^PushbackTextReader stream keywordize?]  ;DM: ^PushbackReader
;; indicates that the type hint needed to be replaced
;; More substantial changes are commented more substantially.


(ns ^{:author "Stuart Sierra, modifed for ClojureCLR by David Miller"
      :doc "JavaScript Object Notation (JSON) parser/generator.
  See http://www.json.org/"}
  clojure.data.json
  (:require [clojure.pprint :as pprint])
  ;DM: (:import (java.io PrintWriter PushbackReader StringWriter
  ;DM:                   StringReader Reader EOFException)))
  (:import (System.IO EndOfStreamException StreamWriter StringReader         ;DM: Added
                      StringWriter TextReader TextWriter)                    ;DM: Added
           (clojure.lang PushbackTextReader)                                 ;DM: Added
		   (System.Globalization NumberStyles StringInfo)                    ;DM: Added
		   ))                                                                ;DM: Added
 
(set! *warn-on-reflection* true)

;;; JSON READER

(def ^:dynamic ^:private *keywordize*)

(declare do-parse)

(defmacro ^:private codepoint [c]
  (int c))

(defn- codepoint-clause [[test result]]
  (cond (list? test)
        [(map int test) result]
        (= test :whitespace)
        ['(9 10 13 32) result]
        (= test :simple-ascii)
        [(remove #{(codepoint \") (codepoint \\) (codepoint \/)}
                 (range 32 127))
         result]
        :else
        [(int test) result]))

(defmacro ^:private codepoint-case [e & clauses]
  `(case ~e
     ~@(mapcat codepoint-clause (partition 2 clauses))
     ~@(when (odd? (count clauses))
         [(last clauses)])))

(defn- parse-array [^PushbackTextReader stream]                                               ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  (loop [c (.Read stream), result (transient [])]                                             ;DM: .read
    (when (neg? c) 
	  (throw (EndOfStreamException. "JSON error (end-of-file inside array)")))   ;DM: EOFException.
    (codepoint-case c
      :whitespace (recur (.Read stream) result)                                               ;DM: .read 
      \, (recur (.Read stream) result)                                                        ;DM: .read 
      \] (persistent! result)                                                                 ;DM: .unread 
      (do (.Unread stream c)
          (let [element (do-parse stream true nil)]
            (recur (.Read stream) (conj! result element)))))))	                              ;DM: .read 
	
(defn- parse-object [^PushbackTextReader stream]                                              ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  (loop [key nil, result (transient {})] 
    (let [c (.Read stream)]                                                                   ;DM: .read 
      (when (neg? c)
        (throw (EndOfStreamException. "JSON error (end-of-file inside array)")))              ;DM: EOFException.
      (codepoint-case c
        :whitespace (recur key result)  

        \, (recur nil result) 

        \: (recur key result) 

        \} (if (nil? key)
              (persistent! result)
              (throw (Exception. "JSON error (key missing value in object)")))

       (do (.Unread stream c)                                                                 ;DM: .unread 
           (let [element (do-parse stream true nil)]
             (if (nil? key)
               (if (string? element)
                 (recur element result)                                                       ;DM: .read 
                 (throw (Exception. "JSON error (non-string key in object)")))
               (recur nil 
                      (assoc! result (if *keywordize* (keyword key) key)
                              element)))))))))
							
(defn- parse-hex-char [^PushbackTextReader stream]                                   ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial "\u".  Reads the next four characters from the stream.
  (let [a (.Read stream)                                                             ;DM: .read
        b (.Read stream)                                                             ;DM: .read
        c (.Read stream)                                                             ;DM: .read
        d (.Read stream)]                                                            ;DM: .read
    (when (or (neg? a) (neg? b) (neg? c) (neg? d))
      (throw (EndOfStreamException.                                                  ;DM: EOFException.
	          "JSON error (end-of-file inside Unicode character escape)")))
    (let [s (str (char a) (char b) (char c) (char d))]
      (char (Int32/Parse s NumberStyles/HexNumber)))))                               ;DM: (Integer/parseInt s 16)

(defn- parse-escaped-char [^PushbackTextReader stream]                               ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial backslash.
  (let [c (.Read stream)]                                                            ;DM: .read
    (codepoint-case c
      (\" \\ \/) (char c)
      \b \backspace
      \f \formfeed
      \n \newline
      \r \return
      \t \tab
      \u (parse-hex-char stream))))  

(defn- parse-quoted-string [^PushbackTextReader stream]                             ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening quotation mark.
  (let [buffer (StringBuilder.)]
    (loop []
      (let [c (.Read stream)]                                                       ;DM: .read
        (when (neg? c)
          (throw (EndOfStreamException. "JSON error (end-of-file inside array)")))  ;DM: EOFException.
        (codepoint-case c  
          \" (str buffer)
          \\ (do (.Append buffer (parse-escaped-char stream))                       ;DM: .append
                 (recur))
          (do (.Append buffer (char c))                                             ;DM: .append
              (recur)))))))

(defn- parse-number [^PushbackTextReader stream]                               ;DM: ^PushbackReader
  (let [buffer (StringBuilder.)
        floating-point?
        (loop [float? false]
          (let [c (.Read stream)]                                              ;DM: .read
            (codepoint-case c
              (\- \+ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
              (do (.Append buffer (char c))                                    ;DM: .append
                  (recur float?))
              (\e \E \.)
              (do (.Append buffer (char c))                                    ;DM: .append
                  (recur true))
              (do (.Unread stream c)                                           ;DM: .unread
                  float?))))]
    (if floating-point?
      (System.Double/Parse (str buffer))                                       ;DM: Double/valueOf 
      (System.Int64/Parse (str buffer)))))                                     ;DM: Long/valueOf

(defn- do-parse
  [^PushbackTextReader stream eof-error? eof-value]                            ;DM: ^PushbackReader
  (loop []
    (let [c (.Read stream)]                  ;DM: .read
      (if (neg? c) ;; Handle end-of-stream
        (if eof-error?
          (throw (EndOfStreamException. "JSON error (end-of-file)"))           ;DM: EOFException.
          eof-value)
        (codepoint-case
          c
          :whitespace (recur)

          ;; Read numbers
          (\- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
          (do (.Unread stream c)                                               ;DM: .unread
              (parse-number stream))

          ;; Read strings
          \" (parse-quoted-string stream)

          ;; Read null as nil
          \n (if (and (= (codepoint \u) (.Read stream))                       ;DM: .read
                      (= (codepoint \l) (.Read stream))                       ;DM: .read
                      (= (codepoint \l) (.Read stream)))                      ;DM: .read
               nil
               (throw (Exception. (str "JSON error (expected null)"))))

          ;; Read true
          \t (if (and (= (codepoint \r) (.Read stream))                       ;DM: .read
                      (= (codepoint \u) (.Read stream))                       ;DM: .read
                      (= (codepoint \e) (.Read stream)))                      ;DM: .read
               true
               (throw (Exception. (str "JSON error (expected true)"))))

          ;; Read false
          \f (if (and (= (codepoint \a) (.Read stream))                       ;DM: .read
                      (= (codepoint \l) (.Read stream))                       ;DM: .read
                      (= (codepoint \s) (.Read stream))                       ;DM: .read
                      (= (codepoint \e) (.Read stream)))                      ;DM: .read
               false
               (throw (Exception. (str "JSON error (expected false)"))))

          ;; Read JSON objects
          \{ (parse-object stream)

          ;; Read JSON arrays
          \[ (parse-array stream)

          (throw (Exception. 
		          (str "JSON error (unexpected character): " (char c)))))))))

(defn parse [rdr & options]
  (let [{:keys [keywordize eof-error? eof-value]
         :or {keywordize false
              eof-error? true}} options]
    (binding [*keywordize* keywordize]
      (do-parse rdr eof-error? eof-value))))

(defn parse-string
  "Reads one JSON value from input String."
  ([string & options]
     (apply parse (PushbackTextReader. (StringReader. string)) options)))        ;DM: PushbackReader.

;;; JSON WRITER

(def ^:dynamic ^:private *escape-unicode*)
(def ^:dynamic ^:private *escape-slash*)

(defprotocol JSONWriter
  (-write-json [object out]
    "Print object to PrintWriter out as JSON"))

(defn- write-json-string [^String s ^TextWriter out]                                   ;DM: ^CharSequence  ^PrintWriter
  (let [sb (StringBuilder. ^Int32 (count s))]                                          ;DM: ^Integer
    (.Append sb \")                                                                    ;DM: .append
    (dotimes [i (count s)]
      (let [cp (int (.get_Chars s i))]                                                 ;DM: (Character/codePointAt s i)
        (codepoint-case cp
          ;; Printable JSON escapes
          \" (.Append sb "\\\"")                                                       ;DM: .append
          \\ (.Append sb "\\\\")                                                       ;DM: .append
          \/ (.Append sb (if *escape-slash* "\\/" "/"))                                ;DM: .append
          ;; Simple ASCII characters
          :simple-ascii (.Append sb (.get_Chars s i))                                  ;DM: .append  .charAt
          ;; JSON escapes
          \backspace (.Append sb "\\b")                                                ;DM: .append
          \formfeed  (.Append sb "\\f")                                                ;DM: .append
          \newline   (.Append sb "\\n")                                                ;DM: .append
          \return    (.Append sb "\\r")                                                ;DM: .append
          \tab       (.Append sb "\\t")                                                ;DM: .append
          ;; Any other character is Unicode
          (if *escape-unicode*
            (.Append sb (format "\\u%04x" cp)) ; Hexadecimal-escaped                   ;DM: .append
            (.Append sb (.get_Chars s i))))))                                          ;DM: (.appendCodePoint sb cp)
    (.Append sb \")                                                                    ;DM: .append
    (.Write out (str sb))))                                                            ;DM: .print

(defn- as-str
  [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn- write-json-object [m ^TextWriter out]                               ;DM: ^PrintWriter
  (.Write out \{)                                                          ;DM: .print
  (loop [x m]
    (when (seq m)
      (let [[k v] (first x)]
        (when (nil? k)
          (throw (Exception. "JSON object keys cannot be nil/null")))
	(write-json-string (as-str k) out)
        (.Write out \:)                                                               ;DM: .print
        (-write-json v out))
      (let [nxt (next x)]
        (when (seq nxt)
          (.Write out \,)                                                             ;DM: .print
          (recur nxt)))))
  (.Write out \}))                                                                    ;DM: .print

(defn- write-json-array [s ^TextWriter out]                                           ;DM: ^PrintWriter
  (.Write out \[)                                                                     ;DM: .print
  (loop [x s]
    (when (seq x)
      (let [fst (first x)
            nxt (next x)]
        (-write-json fst out)
        (when (seq nxt)
          (.Write out \,)                                                             ;DM: .print
          (recur nxt)))))
  (.Write out \]))                                                                    ;DM: .print

(defn- write-json-bignum [x ^TextWriter out]                                          ;DM: ^PrintWriter
  (.Write out (str x)))                                                               ;DM: .print

(defn- write-json-plain [x ^TextWriter out]                                           ;DM: ^PrintWriter
  (.Write out x))                                                                     ;DM: .print

(defn- write-json-null [x ^TextWriter out]                                            ;DM: ^PrintWriter
  (.Write out "null"))                                                                ;DM: .print

(defn- write-json-named [x ^TextWriter out]                                          ;DM: ^PrintWriter
  (write-json-string (name x) out))

(defn- write-json-generic [x out]
  (if (.IsArray (class x))                                                            ;DM: isArray
    (-write-json (seq x) out)
    (throw (Exception. (str "Don't know how to write JSON of " (class x))))))

(defn- write-json-ratio [x out]
  (-write-json (double x) out))

;;DM: Added write-json-float
(defn- write-json-float [x ^TextWriter out] 
  (.Write out (fp-str x)))                                  

  
;;DM: (extend nil JSONWriter
;;DM:         {:-write-json write-json-null})
;;DM: (extend clojure.lang.Named JSONWriter
;;DM:         {:-write-json write-json-named})
;;DM: (extend java.lang.Boolean JSONWriter
;;DM:         {:-write-json write-json-plain})
;;DM: (extend java.lang.Number JSONWriter
;;DM:         {:-write-json write-json-plain})
;;DM: (extend java.math.BigInteger JSONWriter
;;DM:         {:-write-json write-json-bignum})
;;DM: (extend java.math.BigDecimal JSONWriter
;;DM:         {:-write-json write-json-bignum})
;;DM: (extend clojure.lang.Ratio JSONWriter
;;DM:         {:-write-json write-json-ratio})
;;DM: (extend java.lang.CharSequence JSONWriter
;;DM:         {:-write-json write-json-string})
;;DM: (extend java.util.Map JSONWriter
;;DM:         {:-write-json write-json-object})
;;DM: (extend java.util.Collection JSONWriter
;;DM:         {:-write-json write-json-array})
;;DM: (extend clojure.lang.ISeq JSONWriter
;;DM:         {:-write-json write-json-array})
;;DM: (extend java.lang.Object JSONWriter
;;DM:         {:-write-json write-json-generic})

;;DM: Following added
;;DM: Following added
(extend nil JSONWriter
        {:-write-json write-json-null})
(extend clojure.lang.Named JSONWriter
        {:-write-json write-json-named})
(extend System.Boolean JSONWriter
        {:-write-json write-json-plain})
;;; no equivalent to java.lang.Number.  Sigh.
(extend System.Byte JSONWriter {:-write-json write-json-plain})
(extend System.SByte JSONWriter {:-write-json write-json-plain})
(extend System.Int16 JSONWriter {:-write-json write-json-plain})
(extend System.Int32 JSONWriter {:-write-json write-json-plain})
(extend System.Int64 JSONWriter {:-write-json write-json-plain})
(extend System.UInt16 JSONWriter {:-write-json write-json-plain})
(extend System.UInt32 JSONWriter {:-write-json write-json-plain})
(extend System.UInt64 JSONWriter {:-write-json write-json-plain})
(extend System.Double JSONWriter {:-write-json write-json-float})
(extend System.Single JSONWriter {:-write-json write-json-float})
(extend System.Decimal JSONWriter {:-write-json write-json-plain})
(extend clojure.lang.BigInt JSONWriter
        {:-write-json write-json-bignum})
(extend clojure.lang.BigInteger JSONWriter
        {:-write-json write-json-bignum})
(extend clojure.lang.BigDecimal JSONWriter
        {:-write-json write-json-bignum})
(extend clojure.lang.Ratio JSONWriter
        {:-write-json write-json-ratio})
(extend System.String JSONWriter
        {:-write-json write-json-string})
(extend clojure.lang.IPersistentMap JSONWriter
        {:-write-json write-json-object})
(extend System.Collections.IDictionary JSONWriter
        {:-write-json write-json-object})
;;; Cannot handle generic types!!!!
(extend System.Collections.ICollection JSONWriter
        {:-write-json write-json-array})
(extend clojure.lang.ISeq JSONWriter
        {:-write-json write-json-array})
(extend System.Object JSONWriter
        {:-write-json write-json-generic})
;;DM: End addition

(defn write-json
  "Write JSON-formatted output to a java.io.Writer.
   Options are key-value pairs, valid options are:

    :escape-unicode false
        to turn off \\uXXXX escapes of Unicode characters

    :escape-slash false
        to turn of escaping / as \\/"
  [x writer & options]
  (let [{:keys [escape-unicode escape-slash]
         :or {escape-unicode true
              escape-slash true}} options]
    (binding [*escape-unicode* escape-unicode
              *escape-slash* escape-slash]
	  (-write-json x (if (instance? TextWriter writer) writer (StreamWriter. writer))))))    ;DM: (-write-json x (PrintWriter. writer))
	
(defn json-str
  "Converts x to a JSON-formatted string. Options are the same as
  write-json."
  [x & options]
  (let [sw (StringWriter.)]
    (apply write-json x sw options)
    (.ToString sw)))                                                  ;DM: .toString

;;; JSON PRETTY-PRINTER

;; Based on code by Tom Faulhaber

(defn- pprint-json-array [s escape-unicode] 
  ((pprint/formatter-out "~<[~;~@{~w~^, ~:_~}~;]~:>") s))

(defn- pprint-json-object [m escape-unicode]
  ((pprint/formatter-out "~<{~;~@{~<~w:~_~w~:>~^, ~_~}~;}~:>") 
   (for [[k v] m] [(as-str k) v])))

(defn- pprint-json-generic [x escape-unicode]
  (if (.IsArray (class x))                                                  ;DM: isArray
    (pprint-json-array (seq x) escape-unicode)
    (print (json-str x :escape-unicode escape-unicode))))

(defn- pprint-json-dispatch [x escape-unicode]
  (cond (nil? x) (print "null")
        (true? x) (print "true")                                                                ;DM: Added
		(false? x) (print "false")                                                              ;DM: Added
        (instance? System.Collections.IDictionary x) (pprint-json-object x escape-unicode)      ;DM: java.util.Map
        (instance? System.Collections.ICollection x) (pprint-json-array x escape-unicode)       ;DM: java.util.Collection
        (instance? clojure.lang.ISeq x) (pprint-json-array x escape-unicode)
        :else (pprint-json-generic x escape-unicode)))

(defn pprint-json
  "Pretty-prints JSON representation of x to *out*.

  Valid options are:
    :escape-unicode false
        to turn off \\uXXXX escapes of Unicode characters."
  [x & options]
  (let [{:keys [escape-unicode] :or {escape-unicode true}} options]
    (pprint/write x :dispatch #(pprint-json-dispatch % escape-unicode))))
	
;; Local Variables:
;; mode: clojure
;; eval: (define-clojure-indent (codepoint-case (quote defun)))
;; End:	