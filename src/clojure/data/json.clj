;;; json.clj: JavaScript Object Notation (JSON) parser/writer

;; by Stuart Sierra, http://stuartsierra.com/
;; January 30, 2010

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
       :doc "JavaScript Object Notation (JSON) parser/writer.
  See http://www.json.org/
  To write JSON, use json-str, write-json, or print-json.
  To read JSON, use read-json."}
  clojure.data.json
  (:use [clojure.pprint :only (write formatter-out)])
  ;DM: (:import (java.io PrintWriter PushbackReader StringWriter
  ;DM:                   StringReader Reader EOFException)))
  (:import (System.IO EndOfStreamException StreamWriter StringReader         ;DM: Added
                      StringWriter TextReader TextWriter)                    ;DM: Added
           (clojure.lang PushbackTextReader)                                 ;DM: Added
		   (System.Globalization NumberStyles StringInfo)                    ;DM: Added
		   ))                                                                ;DM: Added
  
;;; JSON READER

(set! *warn-on-reflection* true)

(defmacro ^:private codepoint [c]
  (int c))

(declare read-json-reader)

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

(defn- read-json-array [^PushbackTextReader stream keywordize?]                       ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  (loop [c (.Read stream), result (transient [])]                                             ;DM: .read
    (when (neg? c) (throw (EndOfStreamException. "JSON error (end-of-file inside array)")))   ;DM: EOFException.
    (codepoint-case c
      :whitespace (recur (.Read stream) result)                                               ;DM: .read 
      \, (recur (.Read stream) result)                                                        ;DM: .read 
      \] (persistent! result)                                                                 ;DM: .unread 
      (do (.Unread stream c)
          (let [element (read-json-reader stream keywordize? true nil)]
            (recur (.Read stream) (conj! result element)))))))	                              ;DM: .read 
	
(defn- read-json-object [^PushbackTextReader stream keywordize?]                      ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  (loop [c (.Read stream), key nil, result (transient {})]                                    ;DM: .read 
    (when (neg? c) (throw (EndOfStreamException. "JSON error (end-of-file inside array)")))   ;DM: EOFException.
    (codepoint-case c
     :whitespace (recur (.Read stream) key result)                                            ;DM: .read 

     \, (recur (.Read stream) nil result)                                                     ;DM: .read 

     \: (recur (.Read stream) key result)                                                     ;DM: .read 

     \} (if (nil? key)
           (persistent! result)
           (throw (Exception. "JSON error (key missing value in object)")))

     (do (.Unread stream c)                                                                   ;DM: .unread 
         (let [element (read-json-reader stream keywordize? true nil)]
           (if (nil? key)
             (if (string? element)
               (recur (.Read stream) element result)                                          ;DM: .read 
               (throw (Exception. "JSON error (non-string key in object)")))
             (recur (.Read stream) nil                                                        ;DM: .read 
                    (assoc! result (if keywordize? (keyword key) key)
                            element))))))))
							
(defn- read-json-hex-character [^PushbackTextReader stream]                          ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial "\u".  Reads the next four characters from the stream.
  (let [a (.Read stream)                                                             ;DM: .read
        b (.Read stream)                                                             ;DM: .read
        c (.Read stream)                                                             ;DM: .read
        d (.Read stream)]                                                            ;DM: .read
    (when (or (neg? a) (neg? b) (neg? c) (neg? d))
      (throw (EndOfStreamException. "JSON error (end-of-file inside Unicode character escape)")))    ;DM: EOFException.
    (let [s (str (char a) (char b) (char c) (char d))]
      (char (Int32/Parse s NumberStyles/HexNumber)))))                                               ;DM: (Integer/parseInt s 16)

(defn- read-json-escaped-character [^PushbackTextReader stream]                      ;DM: ^PushbackReader
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
      \u (read-json-hex-character stream))))  

(defn- read-json-quoted-string [^PushbackTextReader stream]                          ;DM: ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening quotation mark.
  (let [buffer (StringBuilder.)]
    (loop [c (.Read stream)]                                                                     ;DM: .read
      (when (neg? c) (throw (EndOfStreamException. "JSON error (end-of-file inside array)")))    ;DM: EOFException.
      (codepoint-case c
          \" (str buffer)
          \\ (do (.Append buffer (read-json-escaped-character stream))                           ;DM: .append
                 (recur (.Read stream)))                                                         ;DM: .read
          (do (.Append buffer (char c))                                                          ;DM: .append
              (recur (.Read stream)))))))	                                                     ;DM: .read

(defn- read-json-number [^PushbackTextReader stream]                            ;DM: ^PushbackReader
  (let [buffer (StringBuilder.)
        floating-point?
          (loop [float? false]
            (let [c (.Read stream)]                                            ;DM: .read
              (codepoint-case c
                (\- \+ \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
                (do (.Append buffer (char c))                                  ;DM: .append
                    (recur float?))
                (\e \E \.)
                (do (.Append buffer (char c))                                  ;DM: .append
                    (recur true))
                (do (.Unread stream c)                                         ;DM: .unread
                    float?))))]
    (if floating-point?
      (System.Double/Parse (str buffer))                                       ;DM: Double/valueOf 
      (System.Int64/Parse (str buffer)))))                                     ;DM: Long/valueOf
			  
(defn- read-json-reader
  ([^PushbackTextReader stream keywordize? eof-error? eof-value]                     ;DM: ^PushbackReader
     (loop [c (.Read stream)]                                                        ;DM: .read
       (if (neg? c) ;; Handle end-of-stream
	 (if eof-error?
	   (throw (EndOfStreamException. "JSON error (end-of-file)"))                    ;DM: EOFException.
	   eof-value)
	 (codepoint-case c
           :whitespace (recur (.Read stream))                                        ;DM: .read

           ;; Read numbers
           (\- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
           (do (.Unread stream c)                                                    ;DM: .unread
               (read-json-number stream))

           ;; Read strings
           \" (read-json-quoted-string stream)

           ;; Read null as nil
           \n (if (and (= (codepoint \u) (.Read stream))                            ;DM: .read
                       (= (codepoint \l) (.Read stream))                            ;DM: .read
                       (= (codepoint \l) (.Read stream)))                           ;DM: .read
                nil
                (throw (Exception. (str "JSON error (expected null)"))))

           ;; Read true
           \t (if (and (= (codepoint \r) (.Read stream))                            ;DM: .read
                       (= (codepoint \u) (.Read stream))                            ;DM: .read
                       (= (codepoint \e) (.Read stream)))                           ;DM: .read
                true
                (throw (Exception. (str "JSON error (expected true)"))))

           ;; Read false
           \f (if (and (= (codepoint \a) (.Read stream))                            ;DM: .read
                       (= (codepoint \l) (.Read stream))                            ;DM: .read
                       (= (codepoint \s) (.Read stream))                            ;DM: .read
                       (= (codepoint \e) (.Read stream)))                           ;DM: .read
                false
                (throw (Exception. (str "JSON error (expected false)"))))

           ;; Read JSON objects
           \{ (read-json-object stream keywordize?)

           ;; Read JSON arrays
           \[ (read-json-array stream keywordize?)

           (throw (Exception. (str "JSON error (unexpected character): " (char c)))))))))

(defn read-json-string
  "Reads one JSON value from input String.
  If keywordize? is true (default), object keys will be converted to
  keywords.  If eof-error? is true (default), empty input will throw
  an EOFException; if false EOF will return eof-value. "
  ([input]
     (read-json-string input true true nil))
  ([input keywordize?]
     (read-json-string input keywordize? true nil))
  ([input keywordize? eof-error? eof-value]
     (read-json-reader (PushbackTextReader. (StringReader. input)) keywordize? eof-error? eof-value)))       ;DM: PushbackReader.


;;; JSON PRINTER

(defprotocol Write-JSON
  (write-json [object out escape-unicode?]
              "Print object to PrintWriter out as JSON"))

(defn- write-json-string [^String s ^TextWriter out escape-unicode?]                   ;DM: ^CharSequence  ^PrintWriter
  (let [sb (StringBuilder. ^Int32 (count s))]                                          ;DM: ^Integer
    (.Append sb \")                                                                    ;DM: .append
    (dotimes [i (count s)]
      (let [cp (int (.get_Chars s i))]                                                 ;DM: (Character/codePointAt s i)
        (codepoint-case cp
          ;; Handle printable JSON escapes before ASCII
          \" (.Append sb "\\\"")                                                       ;DM: .append
          \\ (.Append sb "\\\\")                                                       ;DM: .append
          \/ (.Append sb "\\/")                                                        ;DM: .append
          ;; Print simple ASCII characters
          :simple-ascii (.Append sb (.get_Chars s i))                                  ;DM: .append  .charAt
          ;; Handle JSON escapes
          \backspace (.Append sb "\\b")                                                ;DM: .append
          \formfeed  (.Append sb "\\f")                                                ;DM: .append
          \newline   (.Append sb "\\n")                                                ;DM: .append
          \return    (.Append sb "\\r")                                                ;DM: .append
          \tab       (.Append sb "\\t")                                                ;DM: .append
          ;; Any other character is Unicode
          (if escape-unicode?
            ;; Hexadecimal-escaped
            (.Append sb (format "\\u%04x" cp))                                         ;DM: .append
            (.Append sb (.get_Chars s i))))))                                          ;DM: (.appendCodePoint sb cp)
    (.Append sb \")                                                                    ;DM: .append
    (.Write out (str sb))))                                                            ;DM: .print

(defn- as-str
  [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn- write-json-object [m ^TextWriter out escape-unicode?]                           ;DM: ^PrintWriter
  (.Write out \{)                               ;DM: .print
  (loop [x m]
    (when (seq m)
      (let [[k v] (first x)]
        (when (nil? k)
          (throw (Exception. "JSON object keys cannot be nil/null")))
	(write-json-string (as-str k) out escape-unicode?)
        (.Write out \:)                                                               ;DM: .print
        (write-json v out escape-unicode?))
      (let [nxt (next x)]
        (when (seq nxt)
          (.Write out \,)                                                             ;DM: .print
          (recur nxt)))))
  (.Write out \}))                                                                    ;DM: .print

(defn- write-json-array [s ^TextWriter out escape-unicode?]                           ;DM: ^PrintWriter
  (.Write out \[)                                                                     ;DM: .print
  (loop [x s]
    (when (seq x)
      (let [fst (first x)
            nxt (next x)]
        (write-json fst out escape-unicode?)
        (when (seq nxt)
          (.Write out \,)                                                             ;DM: .print
          (recur nxt)))))
  (.Write out \]))                                                                    ;DM: .print

(defn- write-json-bignum [x ^TextWriter out escape-unicode]                           ;DM: ^PrintWriter
  (.Write out (str x)))                                                               ;DM: .print

(defn- write-json-plain [x ^TextWriter out escape-unicode?]                           ;DM: ^PrintWriter
  (.Write out x))                                                                     ;DM: .print

(defn- write-json-null [x ^TextWriter out escape-unicode?]                            ;DM: ^PrintWriter
  (.Write out "null"))                                                                ;DM: .print

(defn- write-json-named [x ^TextWriter out escape-unicode?]                           ;DM: ^PrintWriter
  (write-json-string (name x) out escape-unicode?))

(defn- write-json-generic [x out escape-unicode?]
  (if (.IsArray (class x))                                                            ;DM: isArray
    (write-json (seq x) out escape-unicode?)
    (throw (Exception. (str "Don't know how to write JSON of " (class x))))))

(defn- write-json-ratio [x out escape-unicode?]
  (write-json (double x) out escape-unicode?))

;;DM: Added write-json-float
(defn- write-json-float [x ^TextWriter out escape-unicode?] 
  (.Write out (fp-str x)))                                  

  
;;DM: (extend nil Write-JSON
;;DM:         {:write-json write-json-null})
;;DM: (extend clojure.lang.Named Write-JSON
;;DM:         {:write-json write-json-named})
;;DM: (extend java.lang.Boolean Write-JSON
;;DM:         {:write-json write-json-plain})
;;DM: (extend java.lang.Number Write-JSON
;;DM:         {:write-json write-json-plain})
;;DM: (extend java.math.BigInteger Write-JSON
;;DM:         {:write-json write-json-bignum})
;;DM: (extend java.math.BigDecimal Write-JSON
;;DM:         {:write-json write-json-bignum})
;;DM: (extend clojure.lang.Ratio Write-JSON
;;DM:         {:write-json write-json-ratio})
;;DM: (extend java.lang.CharSequence Write-JSON
;;DM:         {:write-json write-json-string})
;;DM: (extend java.util.Map Write-JSON
;;DM:         {:write-json write-json-object})
;;DM: (extend java.util.Collection Write-JSON
;;DM:         {:write-json write-json-array})
;;DM: (extend clojure.lang.ISeq Write-JSON
;;DM:         {:write-json write-json-array})
;;DM: (extend java.lang.Object Write-JSON
;;DM:         {:write-json write-json-generic})

;;DM: Following added
;;DM: Following added
(extend nil Write-JSON
        {:write-json write-json-null})
(extend clojure.lang.Named Write-JSON
        {:write-json write-json-named})
(extend System.Boolean Write-JSON
        {:write-json write-json-plain})
;;; no equivalent to java.lang.Number.  Sigh.
(extend System.Byte Write-JSON {:write-json write-json-plain})
(extend System.SByte Write-JSON {:write-json write-json-plain})
(extend System.Int16 Write-JSON {:write-json write-json-plain})
(extend System.Int32 Write-JSON {:write-json write-json-plain})
(extend System.Int64 Write-JSON {:write-json write-json-plain})
(extend System.UInt16 Write-JSON {:write-json write-json-plain})
(extend System.UInt32 Write-JSON {:write-json write-json-plain})
(extend System.UInt64 Write-JSON {:write-json write-json-plain})
(extend System.Double Write-JSON {:write-json write-json-float})
(extend System.Single Write-JSON {:write-json write-json-float})
(extend System.Decimal Write-JSON {:write-json write-json-plain})
(extend clojure.lang.BigInt Write-JSON
        {:write-json write-json-bignum})
(extend clojure.lang.BigInteger Write-JSON
        {:write-json write-json-bignum})
(extend clojure.lang.BigDecimal Write-JSON
        {:write-json write-json-bignum})
(extend clojure.lang.Ratio Write-JSON
        {:write-json write-json-ratio})
(extend System.String Write-JSON
        {:write-json write-json-string})
(extend clojure.lang.IPersistentMap Write-JSON
        {:write-json write-json-object})
(extend System.Collections.IDictionary Write-JSON
        {:write-json write-json-object})
;;; Cannot handle generic types!!!!
(extend System.Collections.ICollection Write-JSON
        {:write-json write-json-array})
(extend clojure.lang.ISeq Write-JSON
        {:write-json write-json-array})
(extend System.Object Write-JSON
        {:write-json write-json-generic})
;;DM: End addition

(defn json-str
  "Converts x to a JSON-formatted string.

  Valid options are:
    :escape-unicode false
        to turn of \\uXXXX escapes of Unicode characters."
  [x & options]
  (let [{:keys [escape-unicode] :or {escape-unicode true}} options
	sw (StringWriter.)
        ]                                                                   ;DM: out (PrintWriter. sw)]
    (write-json x sw escape-unicode)                                        ;DM:  out => sw
    (.ToString sw)))                                                        ;DM: .toString

(defn print-json
  "Write JSON-formatted output to *out*.

  Valid options are:
    :escape-unicode false
        to turn off \\uXXXX escapes of Unicode characters."
  [x & options]
  (let [{:keys [escape-unicode] :or {escape-unicode true}} options]
    (write-json x (StreamWriter. *out*) escape-unicode)))                     ;DM: PrintWriter.  


;;; JSON PRETTY-PRINTER

;; Based on code by Tom Faulhaber

(defn- pprint-json-array [s escape-unicode] 
  ((formatter-out "~<[~;~@{~w~^, ~:_~}~;]~:>") s))

(defn- pprint-json-object [m escape-unicode]
  ((formatter-out "~<{~;~@{~<~w:~_~w~:>~^, ~_~}~;}~:>") 
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
    (write x :dispatch #(pprint-json-dispatch % escape-unicode))))