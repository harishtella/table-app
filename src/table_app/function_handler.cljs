(ns table-app.function-handler
  (:require
    [clojure.string :as str]))

(def operator-strings (set ["+" "-" "*" "/"]))

(defn tokenize [infix-string]
  (let [atoms (str/split infix-string #" ")
        tag-fn (fn [atom]
                (cond
                  (operator-strings atom) ['operator atom]
                 (= "(" atom) ['l-paren nil]
                 (= ")" atom) ['r-paren nil]
                 :else ['number atom]))]
      (map tag-fn atoms)))

(defn operator? [[tag value :as token]]
  (= tag 'operator))

(defn is-number? [[tag value :as token]]
  (= tag 'number))

(defn function? [[tag value :as token]]
   (= tag 'function))

(def operator-precedences {"+" 0 "-" 0 "*" 1 "/" 1})
(def operator-associativity {"+" 'left "-" 'left "*" 'left' "/" 'left})
(defn left-assoc? [operator-string]
  (= (operator-associativity operator-string) 'left))

(defn left-paren? [[tag value :as token]]
  (= tag 'l-paren))

(defn right-paren? [[tag value :as token]]
  (= tag 'r-paren))

(defn precedence-dominates? [[tag-1 value-1 :as token-1] [tag-2 value-2 :as token-2]]
  (let [prec-1 (operator-precedences value-1)
        prec-2 (operator-precedences value-2)]
   (or (> prec-1 prec-2)
       (and (= prec-1 prec-2) (left-assoc? prec-1)))))

(defn buffer-last-stacked? [last-stacked-token token-to-compare]
  (and (not (left-paren? last-stacked-token))
   (or (function? last-stacked-token)
    (and (operator? last-stacked-token)
       (precedence-dominates? last-stacked-token token-to-compare)))))

(defn shunting-yard [token-seq]
  (loop [[current-token & tokens-remaining :as cur-and-rem-tokens] token-seq
         [last-in-stack & rest-of-stack :as stack] '()
         output-buffer '()]
    (if-not (nil? current-token)
     (cond
       (is-number? current-token) (recur tokens-remaining stack (cons current-token output-buffer))

       (operator? current-token)
       (if (buffer-last-stacked? last-in-stack current-token)
        (recur cur-and-rem-tokens rest-of-stack (cons last-in-stack output-buffer))
        (recur tokens-remaining (cons current-token stack) output-buffer))

       (left-paren? current-token)
       (recur tokens-remaining (cons current-token stack) output-buffer)

       (right-paren? current-token)
       (if (left-paren? last-in-stack)
         ;; may need to add an other loop here clean up the stack
        (recur tokens-remaining '() (apply (partial conj output-buffer) rest-of-stack))
        (recur cur-and-rem-tokens rest-of-stack (cons last-in-stack output-buffer))))
     (if-not (nil? last-in-stack)
      (recur tokens-remaining rest-of-stack (cons last-in-stack output-buffer))
      [tokens-remaining stack output-buffer]))))
