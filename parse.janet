(import ./freja-test :as t)

(defmacro dbg
  [& exprs]
  ~(when (dyn :debug)
     (xprintf stdout ,;exprs)))

(defn string-mode
  [state]
  (dbg "string mode")
  (update state :index inc))

(def whitespace?
  {(chr " ") true
   (chr "\r") true
   (chr "\n") true})

(defn whitespace-mode
  [state]
  (dbg "whitespace mode")

  (def whitespace @"")

  (while (< (state :index) (length (state :code)))
    (def c (in (state :code) (state :index)))

    (if (whitespace? c)
      (do (buffer/push-byte whitespace c)
        (update state :index inc))
      (break)))

  (update state :tokens array/push [:whitespace whitespace]))

(defn number-char?
  [c]
  (and (>= c (chr "0")) (<= c (chr "9"))))

(defn number-mode
  [state]
  (dbg "number mode")

  (def number @"")

  (while (< (state :index) (length (state :code)))
    (def c (in (state :code) (state :index)))

    (if (or (number-char? c) (= c (chr ".")))
      (do (buffer/push-byte number c)
        (update state :index inc))
      (break)))

  (update state :tokens array/push [:number number]))

(defn symbol-mode
  [state]
  (dbg "symbol mode")

  (def symbol @"")

  (while (< (state :index) (length (state :code)))

    (def c (in (state :code) (state :index)))

    (if (= c (chr " "))
      (break)
      (do (buffer/push-byte symbol c)
        (update state :index inc))))

  (update state :tokens array/push [:symbol symbol]))

(defn default-mode
  [state]

  (while (< (state :index) (length (state :code)))
    (dbg "default-mode %p" state)

    (def c (in (state :code) (state :index)))

    (cond
      (= c (chr `"`))
      (string-mode state)

      (= c (chr "("))
      (do
        (update state :tokens array/push [:delim @"("])
        (update state :index inc))

      (= c (chr ")"))
      (do
        (update state :tokens array/push [:delim @")"])
        (update state :index inc))

      (number-char? c)
      (number-mode state)

      (whitespace? c)
      (whitespace-mode state)

      # else
      (symbol-mode state))))

(defn tokenize
  [code]
  (def state @{:tokens @[]
               :index 0
               :code code})
  (default-mode state)
  (state :tokens))

(t/test-deep= @[[:delim @"("]] (tokenize "("))

(t/test-deep= @[[:delim @")"]] (tokenize ")"))

(t/test-deep= @[[:symbol @"a"]]
              (tokenize "a"))

(t/test-deep= @[[:symbol @"abcd"]]
              (tokenize "abcd"))

(t/test-deep= @[[:symbol @"ab"]
                [:whitespace @" "]
                [:symbol @"cd"]]
              (tokenize "ab cd"))

(t/test-deep= @[[:symbol @"ab"]
                [:whitespace @" \n"]
                [:symbol @"cd"]]
              (tokenize "ab \ncd"))

(t/test-deep= @[[:delim @"("]
                [:symbol @"+"]
                [:whitespace @" "]
                [:number @"1"]
                [:whitespace @" "]
                [:number @"1"]
                [:delim @")"]]
              (with-dyns [:debug true]
                (tokenize "(+ 1 1)")))

(t/print-result)
