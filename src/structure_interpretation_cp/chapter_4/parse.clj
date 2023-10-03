(ns structure-interpretation-cp.chapter-4.parse)

;; https://gist.github.com/ks888/feead4815770d45c5433eaccb898d2a3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokenizer/Parser (NA)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tokenize [input]
  (let [pos (atom 0)]
    (letfn [(peek-ch [] (if (nil? (get input @pos)) nil (str (get input @pos))))
            (next-ch [] (let [ch (peek-ch)]
                          (reset! pos (+ @pos 1))
                          ch))]
      (letfn [(next-chunk [] (let [ch (peek-ch)]
                               (cond
                                 (nil? ch) nil
                                 (or (= ch "(") (= ch ")") (= ch "\"") (= ch " ")) nil
                                 :else (do
                                         (next-ch)
                                         (str ch (next-chunk))))))
              (next-token [] (let [ch (next-ch)]
                               (cond
                                 (nil? ch) nil
                                 (or (= ch "(") (= ch ")") (= ch "\"") (= ch "'")) ch
                                 (= ch " ") (next-token)
                                 :else (str ch (next-chunk)))))]
        next-token))))

(defn parse [input]
  (let [tokens (tokenize input)]
    (letfn [(parse-string []
              (let [next (tokens)]
                (if (= next "\"")
                  nil
                  (str next (parse-string)))))
            (parse-symbol []
              (let [next (tokens)]
                (if (= next "(")
                  (list :quote (parse-main))
                  (list :quote (parse-num-or-kw next)))))
            (parse-num-or-kw [token]
              (if (number? (read-string token))
                (read-string token)
                (keyword token)))
            (parse-main []
              (let [token (tokens)]
                (cond
                  (nil? token) '()
                  (= token "(") (cons (parse-main) (parse-main))
                  (= token ")") '()
                  (= token "\"") (cons (parse-string) (parse-main))
                  (= token "'") (cons (parse-symbol) (parse-main))
                  :else (cons (parse-num-or-kw token) (parse-main)))))]
      (first (parse-main)))))

