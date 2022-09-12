(import ./freja-test :as t)

(defmacro dbg
  [& exprs]
  ~(when (dyn :debug)
     (xprintf stdout ,;exprs)))

(defn string-mode
  [state]
  (dbg "string mode")
  (update state :index inc))

(defn number-char?
  [c]
  (and (>= c (chr "0")) (<= c (chr "9"))))

(defn number-pred
  [state]
  (let [c (in (state :code) (state :index))]
    (or (number-char? c)
        (= c (chr ".")))))

(defn comment-pred
  [state]
  (let [c (in (state :code) (state :index))]
    (not= c (chr "\n"))))

(def delimiter?
  {(chr " ") true
   (chr "(") true
   (chr ")") true
   (chr "[") true
   (chr "]") true
   (chr "{") true
   (chr "}") true
   (chr "\n") true
   (chr "\r") true})

(defn symbol-pred
  [state]
  (let [c (in (state :code) (state :index))]
    (not (delimiter? c))))

(defn finalize
  [state which]
  (def pending (get state which))
  (assert pending)
  (-> state
      (update :tokens array/push [which pending (state :index)])
      (put which nil)))

(defn pred-mode
  [state kind pred]

  (dbg (string kind " mode"))

  (update state kind |(or $ (state :index)))

  (while (< (state :index) (length (state :code)))
    (if (pred state)
      (update state :index inc)
      (do (finalize state kind)
        (break))))

  state)

(def whitespace?
  {(chr " ") true})

(defn whitespace-pred
  [state]
  (let [c (in (state :code) (state :index))]
    (whitespace? c)))

(def newline?
  {(chr "\n") true
   (chr "\r") true})

(defn newline-mode
  [state]
  (dbg "newline mode")

  (update state :newline |(or $ (state :index)))

  (while (< (state :index) (length (state :code)))
    (def c (in (state :code) (state :index)))
    (case c
      (chr "\n")
      (do
        (update state :index inc)
        (finalize state :newline)
        (break))

      (chr "\r")
      (update state :index inc)

      (do (finalize state :newline)
        (break))))

  state)

(defn branch
  [state]
  (dbg "default-mode %p" state)

  (def c (in (state :code) (state :index)))

  (cond
    ###< continue non-complete symbols etc

    (state :newline)
    (newline-mode state)

    (state :comment)
    (pred-mode state :comment comment-pred)

    (state :number)
    (pred-mode state :number number-pred)

    (state :whitespace)
    (pred-mode state :whitespace whitespace-pred)

    (state :symbol)
    (pred-mode state :symbol symbol-pred)

    ###>

    (= c (chr `"`))
    (string-mode state)

    (= c (chr "("))
    (do
      (update state :tokens array/push [:open-parens
                                        (state :index)
                                        (+ (length "(") (state :index))])
      (update state :index inc))

    (= c (chr ")"))
    (do
      (update state :tokens array/push [:close-parens
                                        (state :index)
                                        (+ (length ")") (state :index))])
      (update state :index inc))

    (= c (chr "["))
    (do
      (update state :tokens array/push [:open-square-brackets
                                        (state :index)
                                        (+ (length "[") (state :index))])
      (update state :index inc))

    (= c (chr "]"))
    (do
      (update state :tokens array/push [:close-square-brackets
                                        (state :index)
                                        (+ (length "]") (state :index))])
      (update state :index inc))

    (= c (chr "{"))
    (do
      (update state :tokens array/push [:open-curly-brackets
                                        (state :index)
                                        (+ (length "{") (state :index))])
      (update state :index inc))

    (= c (chr "}"))
    (do
      (update state :tokens array/push [:close-curly-brackets
                                        (state :index)
                                        (+ (length "}") (state :index))])
      (update state :index inc))

    (newline? c)
    (newline-mode state)

    (= c (chr "#"))
    (pred-mode state :comment comment-pred)

    (number-char? c)
    (pred-mode state :number number-pred)

    (whitespace? c)
    (pred-mode state :whitespace whitespace-pred)

    # else
    (pred-mode state :symbol symbol-pred)))

(defn default-mode
  [state]
  (while (< (state :index) (length (state :code)))
    (branch state))

  state)

(defn finalize-pending
  [state]
  (def variants [:symbol :whitespace :number :comment :newline])
  (loop [v :in variants
         :when (get state v)]
    (finalize state v))
  state)

(defn tokenize
  [state]
  (default-mode state)
  (dbg "done %p" state)
  state)

(defn default-tokenize-state
  [code]
  @{:tokens @[]
    :index 0
    :code code})

(defn tokenize-finalize
  [code]
  (def state (default-tokenize-state code))
  (let [tokens (-> state
                   tokenize
                   finalize-pending
                   (get :tokens))]
    (tuple ;tokens)))


#
##
###
####
############## tests

(t/test= [[:open-parens 0 1]] (tokenize-finalize "("))
(t/test= [[:close-parens 0 1]] (tokenize-finalize ")"))

(t/test= [[:symbol 0 1]]
         (tokenize-finalize "a"))

(t/test= [[:symbol 0 4]]
         (tokenize-finalize "abcd"))

(t/test= [[:symbol 0 2]
          [:whitespace 2 3]
          [:symbol 3 5]]
         (tokenize-finalize "ab cd"))

(t/test= [[:symbol 0 2]
          [:whitespace 2 3]
          [:newline 3 4]
          [:symbol 4 6]]
         (tokenize-finalize "ab \ncd"))

(t/test= [[:number 0 1]]
         (tokenize-finalize "1"))

(t/test= [[:open-parens 0 1]
          [:symbol 1 2]
          [:whitespace 2 3]
          [:number 3 4]
          [:whitespace 4 5]
          [:number 5 6]
          [:close-parens 6 7]]
         (tokenize-finalize "(+ 1 1)"))

(t/test= [[:open-parens 0 1]
          [:symbol 1 4]
          [:whitespace 4 5]
          [:symbol 5 15]
          [:close-parens 15 16]]
         (tokenize-finalize "(use freja/flow)"))

(defn tokens->string
  [code tokens]
  (def s @"")

  (loop [[kind start stop] :in tokens
         :let [slice (string/slice code start stop)]]
    (buffer/push-string s slice))

  (string s))

(defn append-string-to-tokens
  [code tokens]
  (seq [t :in tokens
        :let [slice (string/slice code (t 1) (t 2))]]
    [;t slice]))

# check if slices are correct
(let [code "(use freja/flow)"]
  (t/test=
    code
    (tokens->string code (tokenize-finalize code))))

(t/test= [[:comment 0 38]]
         (tokenize-finalize "# type: has :attack -> has :hp -> void"))

# not using multiline string because on windows newlines will be \r\n
(let [code "(defn attack\n  [o1 o2]\n    (update o2 :hp - (o1 :attack)))"]
  (t/test= '((:open-parens 0 1)
              (:symbol 1 5)
              (:whitespace 5 6)
              (:symbol 6 12)
              (:newline 12 13)
              (:whitespace 13 15)
              (:open-square-brackets 15 16)
              (:symbol 16 18)
              (:whitespace 18 19)
              (:symbol 19 21)
              (:close-square-brackets 21 22)
              (:newline 22 23)
              (:whitespace 23 27)
              (:open-parens 27 28)
              (:symbol 28 34)
              (:whitespace 34 35)
              (:symbol 35 37)
              (:whitespace 37 38)
              (:symbol 38 41)
              (:whitespace 41 42)
              (:symbol 42 43)
              (:whitespace 43 44)
              (:open-parens 44 45)
              (:symbol 45 47)
              (:whitespace 47 48)
              (:symbol 48 55)
              (:close-parens 55 56)
              (:close-parens 56 57)
              (:close-parens 57 58))
           (tokenize-finalize code)))

(t/test= [[:symbol 0 1]
          [:open-square-brackets 1 2]
          [:open-curly-brackets 2 3]
          [:symbol 3 6]
          [:whitespace 6 7]
          [:number 7 8]]
         (tokenize-finalize "@[{:hp 3"))

(comment
  (printf "%s"
          # a way to see tokens and code next to each other
          (let [code "(defn attack\n  [o1 o2]\n    (update o2 :hp - (o1 :attack)))"]
            (string/format "%p"
                           (append-string-to-tokens
                             code
                             (tokenize-finalize code)))))
  #
)

# should be equal even if code is sliced in different ways
(t/test-deep=
  (let [state (default-tokenize-state @"ab ")]
    (tokenize state)
    (update state :code buffer/push-string "c d")
    (tokenize state))

  (let [state (default-tokenize-state @"ab c d")]
    (tokenize state)
    (tracev state)))

(loop [i :range [0 100]]
  (let [rng (math/rng)
        code (string (math/rng-buffer rng 1000))]
    (t/test=
      code
      (tokens->string
        code
        (tokenize-finalize code))

      (string "rng buffer with seed " rng " did not tokenize properly"))))

(t/print-result)
