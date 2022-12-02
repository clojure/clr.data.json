(ns clojure.data.json.impl.appendable
   (:import [System.IO TextWriter]))

;; Defines a protocol to mimic the interface java.lang.Appendable.
;;
;; java.lang.Appendable has this signature:
;;
;;    Appendable append(char c)
;;    Appendable append(CharSequence csq)
;;    Appendable append(CharSequence csq, int start, int end)
;;
;; We make a few adjustments.
;; 
;; First, a straightforward implementation would have a single append method, 
;;   given that defprotocol does not allow overloading and Clojure does not allow ^Char as a type hint.
;;   This would lead to a reflection at runtime.  Nope.
;;   So we define the protocol with separate entries append-char and append-str.
;;
;; Second, there is no CharSequence, so we will use String instead.  
;;  And we will add versions for Char[], just for fun.

(set! *warn-on-reflection* true)

(defprotocol Appendable
   "A replacement for java.lang.Appendable"
   (append-char [this c])
   (append-chars [this ^chars cs] [this ^chars cs start end])   
   (append-str [this ^String s] [this ^String s start end]))
 
;;; The Java interface uses start/end.
;;; The CLR methods use start/count
(defn- append-count ^long [^long start ^long end]
  (+ 1 (- end start)))

   
(extend-protocol Appendable
  
  StringBuilder
    (append-char [this c] (.Append this (char c)))
	(append-chars 
	  ([this ^chars cs] (.Append this cs))
	  ([this ^chars cs start end] (.Append this cs (int start) (int (append-count start end)))))	
	(append-str 
	  ([this ^String s] (.Append this s))
	  ([this ^String s start end]  (.Append this s (int start) (int (append-count start end)))))

  TextWriter
	(append-char [this c] (.Write this (char c)) this)
	(append-chars 
	  ([this ^chars cs] (.Write this cs) this)
	  ([this ^chars cs  start  end] (.Write this cs (int start) (int (append-count start end))) this))
	(append-str 
	 ([this ^String s] (.Write this s) this)
	 ([this ^String s  start end] (.Write this (.ToCharArray s (int start) (int (append-count start end)))) this)))
	
