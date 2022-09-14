(import ./tokenize :as tok)
(import ./parse :as p)

(varfn my-eval [ast])

(def standard-lib
  @{'print {:func (fn [& args] (print ;args))}

    'pp {:func (fn [& args] (pp ;args))}

    '+
    {:func
     (fn [& args]
       (+ ;args))}

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
                (map my-eval body))]
      (my-eval ['do ;res]))))

(defn call
  [fcall]
  (match fcall
    ['quote & args]
    (do (assert (= 1 (length args)))
      (first args))

    ['do & args]
    (do (var res nil)
      (loop [node :in args]
        (set res (my-eval node)))
      res)

    ['defmacro name args & body]
    (put (dyn :env) name {:macro (macro-definition args ;body)})

    [f-sym & args]
    (let [f (my-eval f-sym)]
      (if-let [{:func func :macro macro} f]
        (cond func
          (func ;(map my-eval args))

          macro
          (macro ;args))
        (errorf "could not find function %.40m" f-sym)))

    x
    (errorf "unhandled call %.40m" x)))

(varfn my-eval
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
      (get env x)

      x
      (errorf "unhandled eval %.40m" x))))


(def code ```
(print (+ 5 5))

(defmacro macro-doubler
  [x]
  (tuple (quote +) x x))

(pp (macro-doubler 5))
```)
(def tokens (tok/tokenize-finalize code))

(def ast ['do ;(p/parse-all tokens code)])

(printf "ast: %.40m" ast)

(my-eval ast)
