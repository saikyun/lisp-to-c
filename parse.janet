(import ./freja-test :as t)

(defmacro dbg
  [& exprs]
  ~(when (dyn :debug)
     (xprintf stdout ,;exprs)))

###
# tokens with length
###

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
  [state]
  (def {:kind kind :start start} (state :current))
  (dbg "%p" (state :index))
  (-> state
      (update :tokens array/push [kind (- (state :index) start)])
      (put :current nil)))

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

  (def c (in (state :code) (state :index)))
  (case c
    (chr "\n")
    (do
      (update state :index inc)
      (finalize state))

    (chr "\r")
    (update state :index inc)

    (finalize state)))

(defn string-mode
  [state]
  (dbg "string mode")

  (def c (in (state :code) (state :index)))

  (cond
    (= c (chr `"`))
    (do (finalize state)
      (update state :tokens array/push :close-string)
      (update state :index inc))

    :else
    (update state :index inc)))

(defn pred-mode
  [state kind pred]
  (dbg (string kind " mode"))

  (if (pred state)
    (update state :index inc)
    (finalize state)))

(defn choose-pred-mode
  [state]
  (case (in (state :current) :kind)
    :comment
    (pred-mode state :comment comment-pred)

    :number
    (pred-mode state :number number-pred)

    :whitespace
    (pred-mode state :whitespace whitespace-pred)

    :symbol
    (pred-mode state :symbol symbol-pred)

    :string
    (string-mode state)

    :newline
    (newline-mode state)

    # else
    (assert false (string/format "no matching pred mode for state: %.40m" state))))


###
# main mode
###

(defn branch
  [state]
  (dbg "default-mode %p" state)

  (def c (in (state :code) (state :index)))

  (cond
    ###< continue non-complete symbols etc

    (state :current)
    (choose-pred-mode state)

    ###>

    (= c (chr `"`))
    (do (update state :tokens array/push :open-string)
      (update state :index inc)
      (put state :current {:kind :string :start (state :index)}))

    (= c (chr "("))
    (do
      (update state :tokens array/push :open-parens)
      (update state :index inc))

    (= c (chr ")"))
    (do
      (update state :tokens array/push :close-parens)
      (update state :index inc))

    (= c (chr "["))
    (do
      (update state :tokens array/push :open-square-brackets)
      (update state :index inc))

    (= c (chr "]"))
    (do
      (update state :tokens array/push :close-square-brackets)
      (update state :index inc))

    (= c (chr "{"))
    (do
      (update state :tokens array/push :open-curly-brackets)
      (update state :index inc))

    (= c (chr "}"))
    (do
      (update state :tokens array/push :close-curly-brackets)
      (update state :index inc))

    (newline? c)
    (put state :current {:kind :newline :start (state :index)})

    (= c (chr "#"))
    (put state :current {:kind :comment :start (state :index)})

    (number-char? c)
    (put state :current {:kind :number :start (state :index)})

    (whitespace? c)
    (put state :current {:kind :whitespace :start (state :index)})

    # else
    (put state :current {:kind :symbol :start (state :index)})))

(defn default-mode
  [state]
  (while (< (state :index) (length (state :code)))
    (branch state))

  state)

(defn finalize-pending
  [state]
  (when (state :current)
    (finalize state))
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
  [code &opt state]
  (default state (default-tokenize-state code))
  (let [tokens (-> state
                   tokenize
                   finalize-pending
                   (get :tokens))]
    (tuple ;tokens)))

##
# query functions
##

(def single-length
  {:open-parens 1
   :close-parens 1
   :open-square-brackets 1
   :close-square-brackets 1
   :open-curly-brackets 1
   :close-curly-brackets 1
   :open-string 1
   :close-string 1})

(def token-has-length?
  {:symbol 1
   :comment 1
   :newline 1
   :number 1
   :whitespace 1
   :string 1})

(defn token-length
  [token]
  (match token
    ([x len] (token-has-length? x))
    len

    (_ (single-length token))
    1

    x
    (errorf "token %p has no token-length" x)))

(defn token->slice
  [tokens token-i]

  (var code-i 0)

  (loop [i :range [0 token-i]
         :let [token (in tokens i)]]
    (+= code-i (token-length token)))

  [code-i (+ code-i (token-length (tokens token-i)))])

#
##
###
####
############## tests

(t/test= [:open-parens] (tokenize-finalize "("))
(t/test= [:close-parens] (tokenize-finalize ")"))

(t/test= [:open-string [:string 3] :close-string]
         (tokenize-finalize `"hej"`))

(t/test= [:open-string [:string 3]] (tokenize-finalize `"hej`))

(t/test= [[:symbol 1]]
         (tokenize-finalize "a"))

(t/test= [[:symbol 4]]
         (tokenize-finalize "abcd"))

(t/test= [[:symbol 2]
          [:whitespace 1]
          [:symbol 2]]
         (tokenize-finalize "ab cd"))

(t/test= [[:symbol 2]
          [:whitespace 1]
          [:newline 1]
          [:symbol 2]]
         (tokenize-finalize "ab \ncd"))

(t/test= [[:number 1]]
         (tokenize-finalize "1"))

(t/test= [:open-parens
          [:symbol 1]
          [:whitespace 1]
          [:number 1]
          [:whitespace 1]
          [:number 1]
          :close-parens]
         (tokenize-finalize "(+ 1 1)"))

(let [code "(use freja/flow)"
      tokens (tokenize-finalize code)]
  (t/test= [:open-parens [:symbol 3] [:whitespace 1] [:symbol 10] :close-parens]
           tokens)
  # we can go from tokens back to code
  (t/test= "freja/flow" (string/slice code ;(token->slice tokens 3))))

(defn tokens->string
  [code tokens]
  (def s @"")

  (loop [i :range [0 (length tokens)]
         :let [slice (string/slice code ;(token->slice tokens i))]]
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

(t/test= [[:comment 38]]
         (tokenize-finalize "# type: has :attack -> has :hp -> void"))

# not using multiline string because on windows newlines will be \r\n
(let [code "(defn attack\n  [o1 o2]\n    (update o2 :hp - (o1 :attack)))"]
  (t/test= '(:open-parens
              (:symbol 4)
              (:whitespace 1)
              (:symbol 6)
              (:newline 1)
              (:whitespace 2)
              :open-square-brackets
              (:symbol 2)
              (:whitespace 1)
              (:symbol 2)
              :close-square-brackets
              (:newline 1)
              (:whitespace 4)
              :open-parens
              (:symbol 6)
              (:whitespace 1)
              (:symbol 2)
              (:whitespace 1)
              (:symbol 3)
              (:whitespace 1)
              (:symbol 1)
              (:whitespace 1)
              :open-parens
              (:symbol 2)
              (:whitespace 1)
              (:symbol 7)
              :close-parens
              :close-parens
              :close-parens)
           (tokenize-finalize code)))

(t/test= [[:symbol 1]
          :open-square-brackets
          :open-curly-brackets
          [:symbol 3]
          [:whitespace 1]
          [:number 1]]
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
    state))

# should be equal even if code is sliced in different ways

(defn randomized-slicing
  [rng code]
  (let [state (default-tokenize-state @"")]
    (var i 0)
    (while (< i (length code))
      (def max-len (- (length code) i))
      (def len (math/rng-int rng (inc max-len)))
      (update state :code buffer/push-string (string/slice code i (+ i len)))
      (tokenize state)
      (+= i len))
    state))

(let [code @"ab c d"
      rng (math/rng)]
  (t/test-deep=
    (randomized-slicing rng code)
    (let [state (default-tokenize-state code)]
      (tokenize state)
      state)
    (string "rng slicing with seed " rng " did not tokenize properly")))

(defn rng-string
  "Used instead of math/rng-buffer for more readable strings."
  [rng len]
  (var buf (buffer/new len))

  (for i 0 len
    (buffer/push-byte buf (+
                            (chr " ")
                            (math/rng-int rng (- 126 (chr " "))))))

  buf)

(let [seed (os/time)
      rng (math/rng seed)]
  (loop [i :range [0 100]]
    (let [code (string (math/rng-buffer rng 100))
          state (default-tokenize-state code)
          token-code (tokens->string
                       code
                       (tokenize-finalize code state))]
      (t/test=
        code
        token-code

        (string/format
          ```
                rng buffer with seed %d and i %d did not tokenize properly
                code: %s
                tokenized code: %s
                state: %.40m
                ```
          seed
          i
          code
          token-code
          state)))))

(let [seed (os/time)
      rng (math/rng seed)]
  (loop [i :range [0 100]]
    (let [code (math/rng-buffer rng 100)]
      (t/test-deep=
        (randomized-slicing rng code)
        (let [state (default-tokenize-state code)]
          (tokenize state)
          state)
        (string "rng slicing with seed " seed " and i " i " did not tokenize properly, code: " code)))))


###
# query functions
###

(t/test= (token-length [:symbol 3]) 3)
(t/test= (token-length :open-parens) 1)
(t/test= [1 4] (token->slice [:open-parens [:symbol 3]] 1))

(t/print-result)
