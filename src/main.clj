;; Trying to implement a parser using Clojure.

(import '(java.io FileReader BufferedReader))

;; The file handler. 
(def *reader* (BufferedReader. (FileReader. (first *command-line-args*))))

;; The collection that will hold all the tokens.
(def *token-list* [])

;; Java returns a -1 for the EOF.
(def *EOF* -1)


;; note: this program's behavior is undefined when the user inputs a dos style text file. this program has been tested for correctness only with unix files.

;;TODO[]  test to make sure that the input file given by the user points to a real file. Give an error message if the file doesn't exist. I dont want them to see an exception because they'll get frustrated and think the program doesn't work.

;;TODO[] create a jar file, and a readme with directions on how to run the jar file.

;; The data structure that will represent an individual token.
(defstruct token :type :value)

;; Helper functions that I'll later use to identify the recently read input
(defn is-whitespace?    [c] (Character/isWhitespace (char c)))
(defn is-letter?        [c] (Character/isLetter (char c)))
(defn is-digit?         [c] (Character/isDigit (char c)))
(defn is-asterisk?       [c] (= c \*))
(defn is-dash?          [c] (= c \-))
(defn is-colon?         [c] (= c \:))
(defn is-left-caret?   [c] (= c \<))
(defn is-right-caret?  [c] (= c \>))
(defn is-comma?         [c] (= c \,))
(defn is-semicolon?     [c] (= c \;))
(defn is-left-paren?    [c] (= c \())
(defn is-right-paren?   [c] (= c \)))
(defn is-period?        [c] (= c \.))
(defn is-forward-slash? [c] (= c \/))
(defn is-dollar-sign?   [c] (= c \$))
(defn is-plus?          [c] (= c \+))
(defn is-vertical-bar?  [c] (= c \|))
(defn is-ampersand?     [c] (= c \&))
(defn is-equal-sign?    [c] (= c \-))
(defn is-eof?           [c] (= c *EOF*))

(defn get-char
  "Fetches the next character from *reader*."
  []
  (. *reader* mark 1)
  (let [in-char (. *reader* read)]
    ;; BufferedReader returns an integer with each call to read(), hence
    ;; we'll need to coerce it into an integer using the CHAR function.
    (if-not (= in-char *EOF*) (char in-char) *EOF*)))


(defn unget-char 
  "Backtracks one character."
  []
  (. *reader* reset))

(defn get-token 
  "Returns the next token."
  []
  (loop [current-state :START token-value ""]
    (let [in-char (get-char)]
      (cond
       (= current-state :START)
       (cond 
        (is-eof?           in-char) (recur :EOF (str *EOF*))
        (is-whitespace?    in-char) (recur :START "")
        (is-letter?        in-char) (recur :INID (str token-value in-char))
        (is-digit?         in-char) (recur :INDIGIT (str in-char))
	(is-asterisk?       in-char) (recur :INMULOP (str in-char token-value))
	(is-dash?          in-char) (recur :INMINUS   (str in-char token-value))
	(is-colon?         in-char) (recur :INASSIGNOP (str in-char token-value))
	(is-right-caret?  in-char) (recur :INGRELOP (str in-char))
	(is-left-caret?   in-char) (recur :INLRELOP (str in-char))
	(is-comma?         in-char) (struct token "Comma"     (str in-char))
	(is-semicolon?     in-char) (struct token "SemiColon" (str in-char))
	(is-left-paren?    in-char) (struct token "LParen"    (str in-char))
	(is-right-paren?   in-char) (struct token "RParen"    (str in-char))
	(is-period?        in-char) (struct token "Period"    (str in-char))
	(is-forward-slash? in-char) (struct token "DivOp"     (str in-char))
	(is-dollar-sign?   in-char) (struct token "ModOp"     (str in-char))
	(is-plus?          in-char) (struct token "Plus"      (str in-char))
	(is-vertical-bar?  in-char) (struct token "LogOp"     (str in-char))
	(is-ampersand?     in-char) (struct token "LogOp"     (str in-char))
	(is-equal-sigh?    in-char) (struct token "LogOp"     (str in-char))
	
	:else (struct token "Error" "Unexpected input while in START state."))

       (= current-state :INMULOP)
       (if (= in-char \*) 
	 ;; We're seeing a second '*'. Return the 'ExpOp' token.  
	 (struct token "ExpOp" (str in-char token-value)) 
	 
	 ;; Put back char and send a MulOp token.
	 ;; If it's the EOF, go to the EOF state.
	 (do (unget-char) (struct token "MulOp" token-value)))
       
       (= current-state :INMINUS)
       (if (= in-char \-)
	 ;; If true, then we're seeing a second '-'. Change state to INCOMMENT.
	 (recur :INCOMMENT "")

	 ;; If false, then put back char and send a Minus token.
	 (do (unget-char) (struct token "Minus" "-")))
       
       (= current-state :INCOMMENT)
       (if (= in-char \newline)
	 ;; If true, then we're done throwing away chars. Go back to the START state.
	 (recur :START "")
	 
	 ;; As long as we don't see an EOF, keep consuming. 
	 (if-not (= in-char *EOF*)
	   (recur :INCOMMENT "")
	   (recur :EOF (str *EOF*))))
       
       (= current-state :EOF)
       (struct token "SCANEOF" *EOF*)
       
       (= current-state :INID)
       (if (Character/isLetterOrDigit (char in-char))
	 (recur :INID (str token-value in-char))
	 (do (unget-char) (struct token "Id" token-value)))
       
       (= current-state :INASSIGNOP)
       (if (= in-char \=)
	 ;; We're seeing '=' as expected. Return the 'AssignOp' token.
	 (struct token "AssignOp" (str token-value in-char))

	 ;; Return an error, because we're unexpectedly seeing something other than '='. 
	 (struct token "Error" "Unexpected input while in ASSIGNOP state")
					)

       (= current-state :INGRELOP)
       (if (= in-char \=)
	 ;; We're seeing a '>=' token.
	 (struct token "RelOp" (str token-value in-char))

	 ;; It's a '>' token.
	 (do (unget-char) (struct token "RelOp" token-value)))
       
       (= current-state :INLRELOP)
       (if (or (= in-char \>) (= in-char \=))
	 ;; We're seeing a '<>' or '<=' token.
	 (struct token "RelOp" (str token-value in-char))
	 
	 ;; It's a '<' token.
	 (do (unget-char) (struct token "RelOp" token-value)))
       
       (= current-state :INDIGIT)
       (if (Character/isDigit (char in-char))
	 (recur :INDIGIT (str token-value in-char))
	 (do (unget-char) (struct token "IntLiteral" token-value)))))))

(defn print-token 
  "Pretty print the token to screen."
  [token]
  (println (str "<" (:type token) ", " (:value token) ">")))

(defn print-total
  "Pretty print the total to screen."
  [token-type total] 
  (println (str 
	    token-type 
	    (apply str (interpose "" (repeat (- 10 (count token-type)) "."))) 
	    "...." 
	    (if (< total 10) ".") 
	    (str total))))

(println (str "\n*** Input File: " (first *command-line-args*) " ***\n"))

(loop []
  (let [token (get-token)]
    (cond
     (= (:type token) "Error") (println "\nError: Unable to continue scanning source file.\n")
     (= (:type token) "SCANEOF")
     (do
       (def *token-list* (conj *token-list* token))
       (print-token token))
     (= (:type token) "Id") 
     (if (> (count (:value token)) 8) 
       (do
	 (println "Error: Identifier is more than 8 characters long.")
	 (println (str "The following Id is greater than 8 characters long: "
		       (:value token))))
       (do
	 (def *token-list* (conj *token-list* token))
	 (print-token token)
	 (recur)))
     :else 
     (do 
       (def *token-list* (conj *token-list* token))
       (print-token token)
       (recur)))))

(println)
(print-total "Id"         (count (filter #(= (:type %) "Id")         *token-list*)))
(print-total "IntLiteral" (count (filter #(= (:type %) "IntLiteral") *token-list*)))
(print-total "LParen"     (count (filter #(= (:type %) "LParen")     *token-list*)))
(print-total "RParen"     (count (filter #(= (:type %) "RParen")     *token-list*)))
(print-total "SemiColon"  (count (filter #(= (:type %) "SemiColon")  *token-list*)))
(print-total "Comma"      (count (filter #(= (:type %) "Comma")      *token-list*)))
(print-total "Period"     (count (filter #(= (:type %) "Period")     *token-list*)))
(print-total "AssignOp"   (count (filter #(= (:type %) "AssignOp")   *token-list*)))
(print-total "Plus"       (count (filter #(= (:type %) "Plus")       *token-list*)))
(print-total "Minus"      (count (filter #(= (:type %) "Minus")      *token-list*)))
(print-total "MulOp"      (count (filter #(= (:type %) "MulOp")      *token-list*)))
(print-total "DivOp"      (count (filter #(= (:type %) "DivOp")      *token-list*)))
(print-total "ModOp"      (count (filter #(= (:type %) "ModOp")      *token-list*)))
(print-total "ExpOp"      (count (filter #(= (:type %) "ExpOp")      *token-list*)))
(print-total "RelOp"      (count (filter #(= (:type %) "RelOp")      *token-list*)))
(print-total "LogOp"      (count (filter #(= (:type %) "LogOp")      *token-list*)))
(print-total "SCANEOF"    (count (filter #(= (:type %) "SCANEOF")    *token-list*)))
(println)
(print-total "Total" (count *token-list*))

