(print (+ 5 5))

(defmacro macro-doubler
  [x]
  (tuple (quote +) x x))

(print (macro-doubler 5))

(comment
  # ast

  [['print ['+ 5 5]]

   ['defmacro 'macro-doubler
    ['x]
    ['tuple ['quote '+] 'x 'x]]

   ['print
    ['macro-doubler 5]]]

  # "compile"

  ['print ['+ 5 5]]
  #=> emits code? no
  # else
  #=> eval
  (print (+ 5 5))

  ['defmacro 'macro-doubler
   ['x]
   ['tuple ['quote '+] 'x 'x]]
  #=> emits code? no
  # else
  #=> eval
  (put-in env [:macros 'macro-doubler]
       {:args ['x]
        :body ['do ['tuple ['quote '+] 'x 'x]]})

  ['print ['macro-doubler 5]]
  #=> emits code? no
  # else
  #=> eval
  (print (call-macro 'macro-doubler 5))

  call-macro means
  (eval ['do ['tuple ['quote '+] 'x 'x]] {'x 5})

  #

  
  )
