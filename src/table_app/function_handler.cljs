(ns table-app.function-handler
  (:require [clojure.string :as str]))

;; TODO: Why do I have trouble getting vars to show up in figwheel repl.
;; Sometimes have to do a 'lein clean'.

(def operator-strings (set ["+" "-" "*" "/"]))

(defn tokenize
  ([infix-string]
   (tokenize infix-string []))
  ([infix-string variable-symbols]
   (let [variable-names (set (map name variable-symbols))
         atoms (str/split infix-string #" ")
         tag-fn (fn [atom]
                 (cond
                   (operator-strings atom) ['operator atom]
                   (variable-names atom) ['variable atom]
                   (= "(" atom) ['l-paren nil]
                   (= ")" atom) ['r-paren nil]

                   ;; TODO likely buggy, fix later
                   (not (js/isNaN (js/parseFloat atom))) ['number atom]
                   :else ['error atom]))]
        (map tag-fn atoms))))

;; token type identifier functions
;; -------------------------------

(defn variable? [[tag value :as token]]
  (= tag 'variable))

(defn error? [[tag value :as token]]
  (= tag 'error))

(defn operator? [[tag value :as token]]
  (= tag 'operator))

(defn is-number? [[tag value :as token]]
  (= tag 'number))

(defn function? [[tag value :as token]]
   (= tag 'function))

(defn left-paren? [[tag value :as token]]
  (= tag 'l-paren))

(defn right-paren? [[tag value :as token]]
  (= tag 'r-paren))

;; -------------------------------

(def operator-precedences {"+" 0 "-" 0 "*" 1 "/" 1})
(def operator-associativity {"+" 'left "-" 'left "*" 'left' "/" 'left})

(defn left-assoc? [operator-string]
  (= (operator-associativity operator-string) 'left))

(defn precedence-dominates? [[tag-1 value-1 :as token-1] [tag-2 value-2 :as token-2]]
  (let [prec-1 (operator-precedences value-1)
        prec-2 (operator-precedences value-2)]
   (or (> prec-1 prec-2)
       (and (= prec-1 prec-2) (left-assoc? prec-1)))))

(defn should-buffer-last-stacked? [last-stacked-token token-to-compare]
  (and (not (left-paren? last-stacked-token))
   (or (function? last-stacked-token)
    (and (operator? last-stacked-token)
       (precedence-dominates? last-stacked-token token-to-compare)))))

(defn shunting-yard [token-seq]
  (loop [[current-token & tokens-remaining :as cur-and-rem-tokens] token-seq
         [last-in-stack & rest-of-stack :as stack] '()
         output-buffer '()]
    (if-not (nil? current-token)

     ;; current-token != nil
     (cond
       (is-number? current-token) (recur tokens-remaining stack (cons current-token output-buffer))
       (variable? current-token) (recur tokens-remaining stack (cons current-token output-buffer))

       (operator? current-token)
       (if (should-buffer-last-stacked? last-in-stack current-token)
        (recur cur-and-rem-tokens rest-of-stack (cons last-in-stack output-buffer))
        (recur tokens-remaining (cons current-token stack) output-buffer))

       (left-paren? current-token)
       (recur tokens-remaining (cons current-token stack) output-buffer)

       (right-paren? current-token)
       (if (left-paren? last-in-stack)
         ;; This takes everything on the stack except the left-paren,
         ;; and dumps it all onto the output buffer.
        (recur tokens-remaining '() (apply (partial conj output-buffer) rest-of-stack))
        ;; This pops one thing off  the stack and adds it to the output buffer.
        ;; Leaves the right-paren on the list of remaining tokens, however.
        (recur cur-and-rem-tokens rest-of-stack (cons last-in-stack output-buffer)))

       (error? current-token)
       nil)

     ;; current-token != nil
     (if-not (nil? last-in-stack)
      ;; start emptying the stack onto output-buffer recursively
      (recur tokens-remaining rest-of-stack (cons last-in-stack output-buffer))
      ;; return everything to the caller upon return
      [tokens-remaining stack output-buffer]))))

;----------------------------
; RPN interperter

(defn evaluate-token [token context]
  (let [[tag value] token
        op-map {"+" + "-" - "*" * "/" /}]
     (cond
       (is-number? token) (js/parseFloat value)
       (operator? token) (op-map value)
       (variable? token) (context (keyword value))
       (function? token) (context (keyword value))
       :else nil)))

(defn interpret-rpn [rpn-token-seq context]
  (loop [[s1 s2 & rest-of-stack :as stack] '()
         [current-token & tokens-remaining] rpn-token-seq]
   (if-not (nil? current-token)

  ;; current-token != nil
    (let [evaled-token (evaluate-token current-token context)]
     (if (operator? current-token)
      (let [new-value (evaled-token s2 s1)]
       (recur (cons new-value rest-of-stack) tokens-remaining))
      (recur (cons evaled-token stack) tokens-remaining)))

  ;; current-token == nil
    s1)))

(defn evaluate-expr [expression-string context]
  (let [tokens (tokenize expression-string (keys context))
        rpn-seq (reverse (last (shunting-yard tokens)))
        final-value (interpret-rpn rpn-seq context)]
   final-value))

;---------------------------------------------------
; Checking to see if the above code works as expected

(def tok (tokenize "( 3 + 4 ) * 5 * foo" [:foo :bar]))
(def rpn (reverse (last (shunting-yard tok))))
(def test-interpret (interpret-rpn rpn {:foo 8}))

(def expr "( 3 + 4 ) * 5 * foo")
(def context {:foo 8})
(def test-run (evaluate-expr expr context))

; Scratch space for things to use at the repl

(comment
 (in-ns 'table-app.function-handler))
