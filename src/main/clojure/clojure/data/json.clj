;; Copyright (c) Stuart Sierra, 2012. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Stuart Sierra, modifed for ClojureCLR by David Miller"
      :doc "JavaScript Object Notation (JSON) parser/generator.
  See http://www.json.org/"}
  clojure.data.json
  (:refer-clojure :exclude (read))
  (:require [clojure.pprint :as pprint] [clojure.data.json.impl.appendable :as a])
  (:import (System.IO EndOfStreamException TextWriter StreamWriter StringWriter StringReader)      ;;; (java.io PrintWriter PushbackReader StringWriter
           (clojure.data.json.impl.appendable Appendable)                                          ;;;          Writer StringReader EOFException)
           (clojure.lang PushbackTextReader) (clojure.lang PushbackTextReader)))                   ;;; DM: Added
 ;;; JSON READER

(set! *warn-on-reflection* true)

(defn- default-write-key-fn
  [x]
  (cond (instance? clojure.lang.Named x)
        (name x)
        (nil? x)
        (throw (Exception. "JSON object properties may not be nil"))
        :else (str x)))

(defn- default-value-fn [k v] v)

(declare -read)

(defmacro ^:private codepoint [c]
  (int c))

(defn- codepoint-clause [[test result]]
  (cond (list? test)
        [(map int test) result]
        (= test :whitespace)
        ['(9 10 13 32) result]
        (= test :js-separators)
        ['(16r2028 16r2029) result]
        :else
        [(int test) result]))

(defmacro ^:private codepoint-case [e & clauses]
  `(case ~e
     ~@(mapcat codepoint-clause (partition 2 clauses))
     ~@(when (odd? (count clauses))
         [(last clauses)])))

(defn- read-hex-char [^PushbackTextReader stream]                                       ;;; ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial "\u".  Reads the next four characters from the stream.
  (let [a (.Read stream)                                                                ;;; .read
        b (.Read stream)                                                                ;;; .read
        c (.Read stream)                                                                ;;; .read
        d (.Read stream)]                                                               ;;; .read
    (when (or (neg? a) (neg? b) (neg? c) (neg? d))
      (throw (EndOfStreamException.                                                     ;;; EOFException.
	          "JSON error (end-of-file inside Unicode character escape)")))
    (let [s (str (char a) (char b) (char c) (char d))]
      (char (Int32/Parse s System.Globalization.NumberStyles/HexNumber)))))             ;;; (Integer/parseInt s 16)

(defn- read-escaped-char [^PushbackTextReader stream]                                   ;;; ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; initial backslash.
  (let [c (.Read stream)]                                                               ;;; .read
    (when (neg? c)
      (throw (EndOfStreamException. "JSON error (end-of-file inside escaped char)")))   ;;; EOFException.
    (codepoint-case c
      (\" \\ \/) (char c)
      \b \backspace
      \f \formfeed
      \n \newline
      \r \return
      \t \tab
      \u (read-hex-char stream))))  

(defn- slow-read-string [^PushbackTextReader stream ^String already-read]            ;;; ^PushbackReader
  (let [buffer (StringBuilder. already-read)]
    (loop []
      (let [c (.Read stream)]                                                        ;;; .read 
        (when (neg? c)
          (throw (EndOfStreamException. "JSON error (end-of-file inside string)")))  ;;; EOFException
        (codepoint-case c
          \" (str buffer)
          \\ (do (.Append buffer (read-escaped-char stream))                         ;;; .append
                 (recur))
          (do (.Append buffer (char c))                                              ;;; .append
              (recur)))))))
			  
(defn- read-quoted-string [^PushbackTextReader stream]                               ;;; ^PushbackReader  ;;; cannot implement with chunking as done here because we don't have multi-char unread
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening quotation mark.
  (slow-read-string stream ""))                                                      ;;; (let [buffer ^chars (char-array 64)
                                                                                     ;;;         read (.read stream buffer 0 64)
                                                                                     ;;;         end-index (unchecked-dec-int read)]
                                                                                     ;;;     (when (neg? read)
                                                                                     ;;;       (throw (EOFException. "JSON error (end-of-file inside string)")))
                                                                                     ;;;     (loop [i (int 0)]
                                                                                     ;;;       (let [c (int (aget buffer i))]
                                                                                     ;;;         (codepoint-case c
                                                                                     ;;;           \" (let [off (unchecked-inc-int i)
                                                                                     ;;;                    len (unchecked-subtract-int read off)]
                                                                                     ;;;                (.unread stream buffer off len)
                                                                                     ;;;                (String. buffer 0 i))
                                                                                     ;;;           \\ (let [off i
                                                                                     ;;;                    len (unchecked-subtract-int read off)]
                                                                                     ;;;                (.unread stream buffer off len)
                                                                                     ;;;                (slow-read-string stream (String. buffer 0 i)))
                                                                                     ;;;           (if (= i end-index)
                                                                                     ;;;             (do (.unread stream c)
                                                                                     ;;;                 (slow-read-string stream (String. buffer 0 i)))
                                                                                     ;;;             (recur (unchecked-inc-int i))))))))

(defn- read-integer [^String string]
  (if (< (count string) 18)  ; definitely fits in a Long
    (Int64/Parse string)                                                          ;;; Long/valueOf
    (or (try (Int64/Parse string)                                                 ;;; Long/valueOf
	         (catch OverflowException e nil) (catch FormatException e nil))       ;;; NumberFormatException
        (bigint string))))
		
(defn- read-decimal [^String string bigdec?]
  (if bigdec?
    (bigdec string)
    (Double/Parse string)))                                                               ;;; Double/valueOf

(defn- read-number [^PushbackTextReader stream bigdec?]                                   ;;; ^PushbackReader
  (let [buffer (StringBuilder.)
        decimal? (loop [stage :minus]
                   (let [c (.Read stream)]                                                ;;; .read 
                     (case stage
                       :minus
                       (codepoint-case c
                         \-
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :int-zero))
                         \0
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :frac-point))
                         (\1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :int-digit))
                         (throw (Exception. "JSON error (invalid number literal)")))
                       ;; Number must either be a single 0 or 1-9 followed by 0-9
                       :int-zero
                       (codepoint-case c
                         \0
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :frac-point))
                         (\1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :int-digit))
                         (throw (Exception. "JSON error (invalid number literal)")))
                       ;; at this point, there is at least one digit
                       :int-digit
                       (codepoint-case c
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :int-digit))
                         \.
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :frac-first))
                         (\e \E)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :exp-symbol))
                         ;; early exit
                         :whitespace
                         (do (.Unread stream c)                                           ;;; .unread
                             false)
                         (\, \] \} -1)
                         (do (.Unread stream c)                                           ;;; .unread
                             false)
                         (throw (Exception. "JSON error (invalid number literal)")))
                       ;; previous character is a "0"
                       :frac-point
                       (codepoint-case c
                         \.
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :frac-first))
                         (\e \E)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :exp-symbol))
                         ;; early exit
                         :whitespace
                         (do (.Unread stream c)                                           ;;; .unread
                             false)
                         (\, \] \} -1)
                         (do (.Unread stream c)                                           ;;; .unread
                             false)
                         ;; Disallow zero-padded numbers or invalid characters
                         (throw (Exception. "JSON error (invalid number literal)")))
                       ;; previous character is a "."
                       :frac-first
                       (codepoint-case c
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :frac-digit))
                         (throw (Exception. "JSON error (invalid number literal)")))
                       ;; any number of following digits
                       :frac-digit
                       (codepoint-case c
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :frac-digit))
                         (\e \E)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :exp-symbol))
                         ;; early exit
                         :whitespace
                         (do (.Unread stream c)                                           ;;; .unread
                             true)
                         (\, \] \} -1)
                         (do (.Unread stream c)                                           ;;; .unread
                             true)
                         (throw (Exception. "JSON error (invalid number literal)")))
                       ;; previous character is a "e" or "E"
                       :exp-symbol
                       (codepoint-case c
                         (\- \+)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :exp-first))
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :exp-digit)))
                       ;; previous character is a "-" or "+"
                       ;; must have at least one digit
                       :exp-first
                       (codepoint-case c
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :exp-digit))
                         (throw (Exception. "JSON error (invalid number literal)")))
                       ;; any number of following digits
                       :exp-digit
                       (codepoint-case c
                         (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
                         (do (.Append buffer (char c))                                    ;;; .append
                             (recur :exp-digit))
                         :whitespace
                         (do (.Unread stream c)                                           ;;; .unread
                             true)
                         (\, \] \} -1)
                         (do (.Unread stream c)                                           ;;; .unread
                             true)
                         (throw (Exception. "JSON error (invalid number literal)"))))))]
    (if decimal?
      (read-decimal (str buffer) bigdec?)
      (read-integer (str buffer)))))  

(defn- next-token [^PushbackTextReader stream]                                 ;;; ^PushbackReader
  (loop [c (.Read stream)]                                                     ;;; .read
    (if (< 32 c)
      (int c)
      (codepoint-case (int c)
        :whitespace (recur (.Read stream))                                     ;;; .read
        -1 -1))))

(defn invalid-array-exception []
  (Exception. "JSON error (invalid array)"))
  
(defn- read-array* [^PushbackTextReader stream options]                        ;;; ^PushbackReader
  ;; Handles all array values after the first.
  (loop [result (transient [])]
    (let [r (conj! result (-read stream true nil options))]
      (codepoint-case (int (next-token stream))
        \] (persistent! r)
        \, (recur r)
        (throw (invalid-array-exception))))))
  
 (defn- read-array [^PushbackTextReader stream options]                                               ;;; ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  ;; Only handles array value.  
  (let [c (int (next-token stream))]
    (codepoint-case c
      \] []
      \, (throw (invalid-array-exception))
      (do (.Unread stream c)                                                                           ;;; .unread
          (read-array* stream options)))))

(defn- read-key [^PushbackTextReader stream]                                                           ;;; ^PushbackReader
  (let [c (int (next-token stream))]
    (if (= c (codepoint \"))
      (let [key (read-quoted-string stream)]
        (if (= (codepoint \:) (int (next-token stream)))
          key
          (throw (Exception. "JSON error (missing `:` in object)"))))
      (if (= c (codepoint \}))
        nil
        (throw (Exception. (str "JSON error (non-string key in object), found `" (char c) "`, expected `\"`"))))))) 
  
(defn- read-object [^PushbackTextReader stream options]                                              ;;; ^PushbackReader
  ;; Expects to be called with the head of the stream AFTER the
  ;; opening bracket.
  (let [key-fn (get options :key-fn)
        value-fn (get options :value-fn)]
    (loop [result (transient {})]
      (if-let [key (read-key stream)]
        (let [key (cond-> key key-fn key-fn)
              value (-read stream true nil options)
              r (if value-fn
                  (let [out-value (value-fn key value)]
                    (if-not (= value-fn out-value)
                      (assoc! result key out-value)
                      result))
                  (assoc! result key value))]
          (codepoint-case (int (next-token stream))
            \, (recur r)
            \} (persistent! r)
            (throw (Exception. "JSON error (missing entry in object)"))))
        (let [r (persistent! result)]
          (if (empty? r)
            r
            (throw (Exception. "JSON error empty entry in object is not allowed"))))))))
  
(defn- -read
  [^PushbackTextReader stream eof-error? eof-value options]                         ;;; ^PushbackReader
  (let [c (int (next-token stream))]
    (codepoint-case c
        ;; Read numbers
        (\- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
        (do (.Unread stream c)                                                      ;;; .unread
            (read-number stream (:bigdec options)))

        ;; Read strings
        \" (read-quoted-string stream)

        ;; Read null as nil
        \n (if (and (= (codepoint \u) (.Read stream))                               ;;; .read
                    (= (codepoint \l) (.Read stream))                               ;;; .read
                    (= (codepoint \l) (.Read stream)))                              ;;; .read
             nil
             (throw (Exception. "JSON error (expected null)")))

        ;; Read true
        \t (if (and (= (codepoint \r) (.Read stream))                               ;;; .read
                    (= (codepoint \u) (.Read stream))                               ;;; .read
                    (= (codepoint \e) (.Read stream)))                              ;;; .read
             true
             (throw (Exception. "JSON error (expected true)")))

        ;; Read false
        \f (if (and (= (codepoint \a) (.Read stream))                               ;;; .read
                    (= (codepoint \l) (.Read stream))                               ;;; .read
                    (= (codepoint \s) (.Read stream))                               ;;; .read
                    (= (codepoint \e) (.Read stream)))                              ;;; .read
             false
             (throw (Exception. "JSON error (expected false)")))

        ;; Read JSON objects
        \{ (read-object stream options)

        ;; Read JSON arrays
        \[ (read-array stream options)

        (if (neg? c) ;; Handle end-of-stream
          (if eof-error?
            (throw (EndOfStreamException. "JSON error (end-of-file)"))              ;;; EOFException
            eof-value)
          (throw (Exception.
                  (str "JSON error (unexpected character): " (char c))))))))

(defn- -read1
  [^PushbackTextReader stream eof-error? eof-value options]                         ;;; PushbackReader
  (let [val (-read stream eof-error? eof-value options)]
    (if-let [extra-data-fn (:extra-data-fn options)]
      (if (or eof-error? (not (identical? eof-value val)))
        (let [c (.Read stream)]                                                     ;;; .read
          (if (neg? c)
            val
            (do
              (.Unread stream c)                                                    ;;; .unread 
              (extra-data-fn val stream))))
        val)
      val)))

(defn on-extra-throw
  "Pass as :extra-data-fn to `read` or `read-str` to throw if data is found
  after the first object."
  [val rdr]
  (throw (ex-info "Found extra data after json object" {:val val})))

(defn on-extra-throw-remaining
  "Pass as :extra-data-fn to `read` or `read-str` to throw if data is found
  after the first object and return the remaining data in ex-data :remaining."
  [val ^clojure.lang.PushbackTextReader rdr]                                       ;;; java.io.PushbackReader
  (let [remaining (.ReadToEnd rdr)]                                                ;;; (slurp rdr) -- does not work on a reader
    (throw (ex-info (str "Found extra data after json object: " remaining)
             {:val val, :remaining remaining}))))
			 
(def default-read-options {:bigdec false
                           :key-fn nil
                           :value-fn nil})						   
(defn read
  "Reads a single item of JSON data from a java.io.Reader.
  
  If you wish to repeatedly read items from the same reader, you must
  supply a PushbackReader and reuse it on subsequent calls.
  
  Options are key-value pairs, valid options are:
  
  :eof-error? boolean

        If true (default) will throw exception if the stream is empty.

     :eof-value Object

        Object to return if the stream is empty and eof-error? is
        false. Default is nil.

     :bigdec boolean

        If true use BigDecimal for decimal numbers instead of Double.
        Default is false.

     :key-fn function

        Single-argument function called on JSON property names; return
        value will replace the property names in the output. Default
        is clojure.core/identity, use clojure.core/keyword to get
        keyword properties.

     :value-fn function

        Function to transform values in maps (\"objects\" in JSON) in
        the output. For each JSON property, value-fn is called with
        two arguments: the property name (transformed by key-fn) and
        the value. The return value of value-fn will replace the value
        in the output. If value-fn returns itself, the property will
        be omitted from the output. The default value-fn returns the
        value unchanged. This option does not apply to non-map
        collections.
		
     :extra-data-fn function
	 
       If :extra-data-fn is not nil, then the reader will be checked
       for extra data after the read. If found, the extra-data-fn will
       be invoked with the read value and the reader. The result of
       the extra-data-fn will be returned."
  [reader & {:as options}]
  (let [{:keys [eof-error? eof-value]
         :or {eof-error? true}} options
        pbr (if (instance? PushbackTextReader reader)                                     ;;; PushbackReader
              reader
              (PushbackTextReader. reader))]                                              ;;; PushbackReader.  --  64  (can't specify size of pushback buffer)
    (->> options
         (merge default-read-options)
         (-read1 pbr eof-error? eof-value))))
		 
(defn read-str
  "Reads one JSON value from input String. Options are the same as for
  read."
  [string & {:as options}]
  (let [{:keys [eof-error? eof-value]
         :or {eof-error? true}} options]
    (->> options
         (merge default-read-options)
         (-read1 (PushbackTextReader. (StringReader. string)) eof-error? eof-value))))    ;;; PushbackReader.  --  64  (can't specify size of pushback buffer)

;;; JSON WRITER


(defprotocol JSONWriter
  (-write [object out options]
    "Print object to Appendable out as JSON"))

(defn- ->hex-string [^Appendable out cp]
  (let [cpl (long cp)]
    (a/append-str out "\\u")                                                           ;;; .append
                                                                                       ;;; (cond
                                                                                       ;;;   (< cpl 16)
                                                                                       ;;;   (.append out "000")
                                                                                       ;;;   (< cpl 256)
                                                                                       ;;;   (.append out "00")
                                                                                       ;;;   (< cpl 4096)
                                                                                       ;;;   (.append out "0"))
    (a/append-str out (.ToString cpl "x4"))))                                          ;;; .append Integer/toHexString cp)

(def ^{:tag |System.Int16[]|} codepoint-decoder                                        ;;; "[S"
  (let [shorts (short-array 128)]
    (dotimes [i 128]
      (codepoint-case i
        \" (aset shorts i (short 1))
        \\ (aset shorts i (short 1))
        \/ (aset shorts i (short 2))
        \backspace (aset shorts i (short 3))
        \formfeed  (aset shorts i (short 4))
        \newline   (aset shorts i (short 5))
        \return    (aset shorts i (short 6))
        \tab       (aset shorts i (short 7))
        (if (< i 32)
          (aset shorts i (short 8))
          (aset shorts i (short 0)))))
    shorts))
	
(defn- slow-write-string [^String s ^Appendable out options]                                  ;;; ^CharSequence
  (let [decoder codepoint-decoder]
    (dotimes [i (.Length s)]                                                             ;;; .length
      (let [cp (int (.get_Chars s i))]                                                   ;;; .charAt
        (if (< cp 128)
          (case (aget decoder cp)
            0 (a/append-char out (char cp))                                                  ;;; .append
            1 (do (a/append-char out (char (codepoint \\))) (a/append-char out (char cp)))   ;;; .append  .append
            2 (a/append-str out (if (get options :escape-slash) "\\/" "/"))                  ;;; .append
            3 (a/append-str out "\\b")                                                   ;;; .append
            4 (a/append-str out "\\f")                                                   ;;; .append
            5 (a/append-str out "\\n")                                                   ;;; .append
            6 (a/append-str out "\\r")                                                   ;;; .append
            7 (a/append-str out "\\t")                                                   ;;; .append
            8 (->hex-string out cp))
          (codepoint-case cp
            :js-separators (if (get options :escape-js-separators)
                             (->hex-string out cp)
                             (a/append-char out (char cp)))
            (if (get options :escape-unicode)
              (->hex-string out cp) ; Hexadecimal-escaped
              (a/append-char out (char cp)))))))))	                                     ;;; .append
	

(defn- write-string [^String s ^Appendable out options]                                  ;;; ^CharSequence
  (let [decoder codepoint-decoder
        l (.Length s)]                                                                   ;;; .length
    (a/append-char out \")                                                               ;;; .append
    (loop [i 0]
	  (if (= i l)
	    (a/append-str out s)                                                             ;;; .append
        (let [cp (int (.get_Chars s i))]                                                 ;;; .charAt
          (if (and (< cp 128)
		           (zero? (aget decoder cp)))
		    (recur (unchecked-inc i))
			(do 
			   (a/append-str out s 0 (dec i))                                            ;;; .append  i (instead of (dec i))
			   (slow-write-string (.Substring s i) out options))))))                     ;;; (.subSequence s i l)
    (a/append-char out \")))	
	
(defn- write-indent [^Appendable out options]
  (let [indent-depth (:indent-depth options)]
    (a/append-char out \newline)                                                         ;;; .append
    (loop [i indent-depth]
      (when (pos? i)
        (a/append-str out "  ")                                                          ;;; .append
        (recur (dec i))))))	

(defn- write-object [m ^Appendable out options]
  (let [key-fn (get options :key-fn)
        value-fn (get options :value-fn)
        indent (get options :indent)
        opts (cond-> options
               indent (update :indent-depth inc))]
    (a/append-char out \{)                                                               ;;; .append
    (when (and indent (seq m))
      (write-indent out opts))
    (loop [x m, have-printed-kv false]
      (when (seq x)
        (let [[k v] (first x)
              out-key (key-fn k)
              out-value (value-fn k v)
              nxt (next x)]
          (when-not (string? out-key)
            (throw (Exception. "JSON object keys must be strings")))
          (if-not (= value-fn out-value)
            (do
              (when have-printed-kv
                (a/append-char out \,)                                                    ;;; .append
                (when indent
                  (write-indent out opts)))
              (write-string out-key out opts)
              (a/append-char out \:)                                                     ;;; .append
              (when indent
                (a/append-char out \space))                                             ;;; .append
              (-write out-value out opts)
              (when (seq nxt)
                (recur nxt true)))
            (when (seq nxt)
              (recur nxt have-printed-kv))))))
    (when (and indent (seq m))
      (write-indent out options)))
  (a/append-char out \}))                                                                ;;; .append

(defn- write-array [s ^Appendable out options]
  (let [indent (get options :indent)
        opts (cond-> options
                  indent (update :indent-depth inc))]
    (a/append-char out \[)                                                               ;;; .append
    (when (and indent (seq s))
      (write-indent out opts))
    (loop [x s]
      (when (seq x)
        (let [fst (first x)
              nxt (next x)]
          (-write fst out opts)
          (when (seq nxt)
            (a/append-char out \,)                                                       ;;; .append
            (when indent
              (write-indent out opts))
            (recur nxt)))))
    (when (and indent (seq s))
      (write-indent out options)))
  (a/append-char out \]))                                                                ;;; .append
  
(defn- write-bignum [x ^Appendable out options]
  (a/append-str out (str x)))                                                            ;;; .append
  
(defn- write-float [x ^Appendable out options]                                           ;;;  ^Float  -- can't type-hint float
  (cond (Single/IsInfinity (float x))                                                    ;;; (.isInfinite x)
        (throw (Exception. "JSON error: cannot write infinite Float"))
        (Single/IsNaN (float x))                                                         ;;; (.isNaN x)
        (throw (Exception. "JSON error: cannot write Float NaN"))
        :else
        (a/append-str out (fp-str x))))                                                  ;;; .append   str -> fp-str because ToString omits ".0" on integer-valued floats

(defn- write-double [^Double x ^Appendable out options]
  (cond (Double/IsInfinity (double x))                                                   ;;; (.isInfinite x)
        (throw (Exception. "JSON error: cannot write infinite Double"))
        (Double/IsNaN (double x))                                                        ;;; (.isNaN x)
        (throw (Exception. "JSON error: cannot write Double NaN"))
        :else
        (a/append-str out (fp-str x))))                                                  ;;; .append   str -> fp-str because ToString omits ".0" on integer-valued floats

(defn- write-plain [x ^Appendable out options]
  (a/append-str out (str x)))

(defn- write-boolean [x ^Appendable out options]                                         ;;; ADDED
  (a/append-str out (if x "true" "false")))                                              ;;; ADDED              

(defn- write-uuid [^System.Guid x ^Appendable out options]                               ;;; ^java.util.UUID
  (a/append-char out \")                                                                 ;;; .append
  (a/append-str out (.ToString x))                                                       ;;; .append .toString
  (a/append-char out \"))                                                                ;;; .append

;;;(defn- write-instant [^java.time.Instant x ^Appendable out options]
;;;  (let [formatter ^java.time.format.DateTimeFormatter (:date-formatter options)]
;;;    (.append out \")
;;;    (.append out (.format formatter x))
;;;    (.append out \")))

(defn- write-date [ ^DateTime x ^Appendable out options]                                                        ;;; ^java.util.Date
  (a/append-char out \")(a/append-str out (.ToString x ^String (:date-formatter options))) (a/append-char out \"))    ;;;  (write-instant (.toInstant x) out options)

;;;(defn- default-sql-date->instant-fn [^java.sql.Date d]
;;;  (.toInstant (.atStartOfDay (.toLocalDate d) (java.time.ZoneId/systemDefault))))
  
;;;(defn- write-sql-date [^java.sql.Date x ^Appendable out options]
;;;  (let [->instant (:sql-date-converter options)]
;;;    (write-instant (->instant x) out options)))  
  
(defn- write-null [x ^Appendable out options]
  (a/append-str out "null"))                                                             ;;; .append
  
(defn- write-named [x out options]
  (write-string (name x) out options))

(defn- write-generic [x out options]
  (if (.IsArray (class x))                                                               ;;; isArray
    (-write (seq x) out options)
    ((:default-write-fn options) x out options)))
	
(defn- write-ratio [x out options]
  (-write (double x) out options))
     
;; nil, true, false
(extend nil                    JSONWriter {:-write write-null})
(extend System.Boolean         JSONWriter {:-write write-boolean})                     ;;; java.lang.Boolean write-plain

;; Numbers
(extend System.Byte            JSONWriter {:-write write-plain})                     ;;; java.lang.Byte
(extend System.Int16           JSONWriter {:-write write-plain})                     ;;; java.lang.Short  
(extend System.Int32           JSONWriter {:-write write-plain})                     ;;; java.lang.Integer 
(extend System.Int64           JSONWriter {:-write write-plain})                     ;;; java.lang.Long
(extend System.Single          JSONWriter {:-write write-float})                     ;;; java.lang.Float 
(extend System.Double          JSONWriter {:-write write-double})                    ;;; java.lang.Double 
(extend clojure.lang.Ratio     JSONWriter {:-write write-ratio})
(extend clojure.lang.BigInteger    JSONWriter {:-write write-bignum})                ;;; java.math.BigInteger 
(extend clojure.lang.BigDecimal    JSONWriter {:-write write-bignum})                ;;; java.math.BigDecimal 
(extend clojure.lang.AtomicInteger JSONWriter {:-write write-plain})                 ;;; java.util.concurrent.atomic.AtomicInteger 
(extend clojure.lang.AtomicLong    JSONWriter {:-write write-plain})                 ;;; java.util.concurrent.atomic.AtomicLong
(extend System.Guid            JSONWriter {:-write write-uuid})                      ;;; java.util.UUID
                                                                                     ;;;(extend java.time.Instant      JSONWriter {:-write write-instant})
(extend System.DateTime        JSONWriter {:-write write-date})                      ;;; java.util.Date 
                                                                                     ;;;(extend java.sql.Date          JSONWriter {:-write write-sql-date})
(extend clojure.lang.BigInt    JSONWriter {:-write write-bignum})

;; Symbols, Keywords, and Strings
(extend clojure.lang.Named     JSONWriter {:-write write-named})
(extend System.String          JSONWriter {:-write write-string})                    ;;; java.lang.CharSequence

;; Collections
(extend System.Collections.IDictionary  JSONWriter {:-write write-object})           ;;; java.util.Map 
(extend System.Collections.ICollection  JSONWriter {:-write write-array})            ;;; java.util.Collection

;; Maybe a Java array, otherwise fail
(extend System.Object          JSONWriter {:-write write-generic})                   ;;; java.lang.Object

;;; Following added
(extend System.SByte                    JSONWriter {:-write write-plain})
(extend System.UInt16                   JSONWriter {:-write write-plain})
(extend System.UInt32                   JSONWriter {:-write write-plain})
(extend System.UInt64                   JSONWriter {:-write write-plain})
(extend System.Decimal                  JSONWriter {:-write write-plain})
;;;; End addition

(defn- default-write-fn [x out options]
  (throw (Exception. (str "Don't know how to write JSON of " (class x)))))

(def default-write-options {:escape-unicode true
                            :escape-js-separators true
                            :escape-slash true
                                                                                ;;; :sql-date-converter default-sql-date->instant-fn
                            :date-formatter "G"                                 ;;; java.time.format.DateTimeFormatter/ISO_INSTANT
                            :key-fn default-write-key-fn 
                            :value-fn default-value-fn
							:default-write-fn default-write-fn
                            :indent false
                            :indent-depth 0  ;; internal, to track nesting depth
							})
							

(defn write
  "Write JSON-formatted output to a java.io.Writer. Options are
   key-value pairs, valid options are:

    :escape-unicode boolean

       If true (default) non-ASCII characters are escaped as \\uXXXX

    :escape-js-separators boolean

       If true (default) the Unicode characters U+2028 and U+2029 will
       be escaped as \\u2028 and \\u2029 even if :escape-unicode is
       false. (These two characters are valid in pure JSON but are not
       valid in JavaScript strings.)

    :escape-slash boolean

       If true (default) the slash / is escaped as \\/

    :sql-date-converter function

       Single-argument function used to convert a java.sql.Date to
       a java.time.Instant. As java.sql.Date does not have a
       time-component (which is required by java.time.Instant), it needs
       to be computed. The default implementation, `default-sql-date->instant-fn`
       uses
       ```
          (.toInstant (.atStartOfDay (.toLocalDate sql-date) (java.time.ZoneId/systemDefault)))
       ```

    :date-formatter

        A java.time.DateTimeFormatter instance, defaults to DateTimeFormatter/ISO_INSTANT

    :key-fn function

        Single-argument function called on map keys; return value will
        replace the property names in the output. Must return a
        string. Default calls clojure.core/name on symbols and
        keywords and clojure.core/str on everything else.

    :value-fn function

        Function to transform values in maps before writing. For each
        key-value pair in an input map, called with two arguments: the
        key (BEFORE transformation by key-fn) and the value. The
        return value of value-fn will replace the value in the output.
        If the return value is a number, boolean, string, or nil it
        will be included literally in the output. If the return value
        is a non-map collection, it will be processed recursively. If
        the return value is a map, it will be processed recursively,
        calling value-fn again on its key-value pairs. If value-fn
        returns itself, the key-value pair will be omitted from the
        output. This option does not apply to non-map collections.
		
     :default-write-fn function
	 
        Function to handle types which are unknown to data.json. Defaults
        to a function which throws an exception. Expects to be called with
        three args, the value to be serialized, the output stream, and the
        options map.		

    :indent boolean

        If true, indent json while writing (default = false)."
  [x writer & {:as options}]                                        ;;; ^Writer  -- can't do. we might get a TextWriter or a Stream
  (-write x writer (merge default-write-options options)))
  
(defn write-str
  "Converts x to a JSON-formatted string. Options are the same as
  write."
  ^String [x & {:as options}]
  (let [sw (StringWriter.)]
    (-write x sw (merge default-write-options options))
    (.ToString sw)))                                                        ;;; .toString 
  
;;; JSON PRETTY-PRINTER

;; Based on code by Tom Faulhaber

(defn- pprint-array [s] 
  ((pprint/formatter-out "~<[~;~@{~w~^, ~:_~}~;]~:>") s))

(defn- pprint-object [m options]
  (let [key-fn (:key-fn options)]
    ((pprint/formatter-out "~<{~;~@{~<~w:~_~w~:>~^, ~_~}~;}~:>")
     (for [[k v] m] [(key-fn k) v]))))

(defn- pprint-generic [x options]
  (if (.IsArray (class x))                                                   ;;; .isArray
    (pprint-array (seq x))
    ;; pprint proxies Writer, so we can't just wrap it
    (print (with-out-str (-write x (if (instance? TextWriter *out*) *out* (StreamWriter. ^System.IO.Stream *out*)) options)))))     ;;; (PrintWriter. *out*)

(defn- pprint-dispatch [x options]
  (cond (nil? x) (print "null")
        (true? x) (print "true")                                                ;;; Added
		(false? x) (print "false")                                              ;;; Added
        (instance? System.Collections.IDictionary x) (pprint-object x options)          ;;; java.util.Map
        (instance? System.Collections.ICollection x) (pprint-array x)           ;;; java.util.Collection
        (instance? clojure.lang.ISeq x) (pprint-array x)
        :else (pprint-generic x options)))

(defn pprint
  "Pretty-prints JSON representation of x to *out*. Options are the same
  as for write except :value-fn and :indent, which are not supported."
  [x & {:as options}]
  (let [opts (merge default-write-options options)]
    (pprint/with-pprint-dispatch #(pprint-dispatch % opts)
      (pprint/pprint x))))
