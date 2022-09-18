(import jpm/cgen)
(import ./tokenize :as tok)
(import ./parse :as p)

(import ./unification/unify/unify :as u)
(import ./unification/unify/parse :as up)

(comment
  (cgen/ir
    (defn add
      [[int x] [int y]] int
      (return (+ x y))))
  #
)

(defn codegen
  [env]
  (cgen/print-ir
    (seq [[k {:func func :code code}] :pairs env
          :when func]
      # (printf "k: %s v: %.40m" k code)
      (tracev code))

    #
))


(varfn codegen [ast])

(def standard-lib
  @{'print
    {:func (fn [& args]
             ['printf "%d" ;args])}

    '+
    {:func
     (fn [& args]
       ['+ ;args])}

    'tuple
    {:func (fn [& args] (tuple ;args))}})

(defn macro-definition
  [params & body]
  (fn [& args]
    (def env (table/setproto @{} (dyn :env)))
    (loop [i :range [0 (length params)]
           :let [p (in params i)
                 a (in args i)]]
      (put env p a))

    (let [res (with-dyns [:env env]
                (map codegen body))]
      (codegen ['do ;res]))))

(defn eval-macros
  [form]
  (match form
    ([x & args] (get-in (dyn :env) [x :macro]))
    ((get-in (dyn :env) [x :macro]) ;args)

    [& args]
    (tuple ;(map eval-macros args))

    x
    x))

(defn function-definition
  [name params & body]

  (def type "haha")

  (def form ~(defn ,name ,params ,;body))
  (def parsed (up/parse-defn form))
  (def type (up/type-of-defn parsed))
  (printf "%.40m" parsed)
  (printf "%.40m" type)

  (def env @{'?+ '(int int => int)
             '?print '(:call => void)})

  (def unified (u/unify-kvs (first type) env))
  (printf "unified: %.40m" unified)

  (def f-type (get unified (last (get unified (symbol "?" name)))))

  (def params-with-type (map (fn [p] [p (get unified (symbol "?" p))]) params))

  (def new-body (map codegen (map eval-macros body)))

  {:code ['defn name params-with-type f-type
          ;(if (= 'void f-type)
             new-body
             [(reverse (drop 1 (reverse new-body)))
              ['return (last new-body)]])
          ]})

(defn call
  [fcall]
  (match fcall
    ['quote & args]
    (do (assert (= 1 (length args)))
      (first args))

    ['do & args]
    (do (var res nil)
      (loop [node :in args]
        (set res (codegen node)))
      res)

    ['defmacro name args & body]
    (put (dyn :env) name {:macro (macro-definition args ;body)})

    ['defn name args & body]
    (put (dyn :env) name (function-definition name args ;body))

    [f-sym & args]
    (let [f (codegen f-sym)]
      (if-let [{:func func
                :macro macro} f]
        (cond func
          (func ;(map codegen args))

          macro
          (macro ;args)

          :else
          [f-sym ;args])
        (errorf "could not find function %.40m" f-sym)))

    x
    (errorf "unhandled call %.40m" x)))

(varfn codegen
  [ast]

  (def env (or (dyn :env)
               (let [env @{}]
                 (table/setproto env standard-lib)
                 env)))

  (with-dyns [:env env]
    (match ast
      [f & _]
      (call ast)

      (x (number? x))
      x

      (x (symbol? x))
      (get env x x)

      x
      (errorf "unhandled eval %.40m" x))))


(let [code ```
           (defn adder [x y] (+ x y))

           (defn main []
             (print (adder 5 15)))
           ```
      tokens (tok/tokenize-finalize code)

      ast ['do ;(p/parse-all tokens code)]

      _ (def env (table/setproto @{} standard-lib))

      # _ (printf "ast: %.40m" ast)

      code (with-dyns [:env env]
             (seq [[k {:code code}] :pairs (codegen ast)
                   :when code]
               code))

      # _ (printf "my env: %.40m" env)
]

  (print "c code")
  (cgen/print-ir (tracev code))

  (def res-code (with-dyns [:out @""]
                  (cgen/print-ir code)
                  (dyn :out)))

  (spit "c/test.c" res-code)

  (os/execute ["tcc" "c/test.c"] :p)
  (os/execute ["./test.exe"] :p))

(comment
  (def code ```
(print (+ 5 5))

(defmacro macro-doubler
  [x]
  (tuple (quote +) x x))

(pp (macro-doubler 5))
```)

  (def code
    (string
      ```
    #include <stdio.h>
    ```
      (with-dyns [:out @""]
        (cgen/ir
          (defn doubler [[x int]] int (+ x x))
          (defn main
            [] void
            (printf "%d\n" (doubler 12))

            (printf "%d\n" (+ 5 5))
            #
))
        (dyn :out))

      #
))

  (spit "c/test.c" code)

  (os/execute ["tcc" "c/test.c"] :p)
  (os/execute ["./test.exe"] :p))
