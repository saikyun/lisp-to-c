(import ./freja-test :as t)
(import ./tokenize :as tok)

(varfn my-parse [state])

(varfn my-parse
  [state]
  (def {:tokens tokens
        :index index
        :code code
        :stack stack} state)

  (def t (in tokens index))

  (match t
    :open-parens
    (do (update state :index inc)
      (array/push stack @[])
      nil)

    :close-parens
    (do
      (update state :index inc)
      (let [l (last stack)]
        (assert l)
        (array/pop stack)
        l))

    :open-square-brackets
    (do (update state :index inc)
      (array/push stack @[])
      nil)

    :close-square-brackets
    (do
      (update state :index inc)
      (let [l (last stack)]
        (assert l)
        (array/pop stack)
        l))

    [:symbol _]
    (let [sym (symbol (string/slice code ;(tok/token->slice tokens (state :index))))]
      (update state :index inc)
      sym)

    [:whitespace _]
    (do (update state :index inc)
      nil)

    [:number x]

    (let [num (scan-number (string/slice code ;(tok/token->slice tokens (state :index))))]
      (update state :index inc)
      num)

    _ (do (update state :index inc)
        nil)))

(defn parse-all
  [tokens code]
  (def state @{:code code
               :tokens tokens
               :index 0
               :stack @[@[]]})

  (while (< (state :index) (length tokens))
    (def t (in tokens (state :index)))

    (when-let [node (my-parse state)]
      (array/push (last (state :stack)) node))
    #
)

  # in the end, the final list on the stack should be the root
  (first (state :stack)))

(def code "(print (+ 5 5))")
(def tokens (tok/tokenize-finalize code))

(parse-all tokens code)

(let [code "5"
      tokens (tok/tokenize-finalize code)
      ast (parse-all tokens code)]
  (t/test-deep= @[5] ast))


(comment
  (let [code "(use freja/flow)"
        tokens [:open-parens [:symbol 3] [:whitespace 1] [:symbol 10] :close-parens]]
    (t/test-deep=
      {:kind :call
       :children @[1 3]}
      (my-parse tokens 0)))
  #
)

(t/print-result)
