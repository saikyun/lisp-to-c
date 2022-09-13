(def all-suites
  @{})

(defn print-result*
  "Called internally by print-result."
  [{:name name
    :nof-tests nof-tests
    :failed-tests failed-tests}]
  (if
    (empty? failed-tests)
    (print name ": All tests successful!")

    (do (eprint name ": " (- nof-tests (length failed-tests)) "/" nof-tests " passed")
      (loop [[line column s] :in failed-tests]
        (eprint "\e[31mX\e[0m " "\e[35mline " line "\e[0m" " " s)))))

(defn get-suite
  []
  (or (get all-suites (dyn :current-file))
      (let [new-suite @{:name (dyn :current-file)
                        :nof-tests 0
                        :failed-tests @[]}]
        (put all-suites (dyn :current-file) new-suite)
        new-suite)))

(defn test-failure
  [line column message]
  (update (get-suite) :failed-tests array/push [line column message]))

(defmacro test
  "Register a test failure if x is not truthy. Will not evaluate `err` if x is truthy."
  [x &opt err line column]
  (def [l c] (tuple/sourcemap (dyn *macro-form* ())))
  (default line l)
  (default column c)
  (update (get-suite) :nof-tests inc)
  (def v (gensym))
  ~(do
     (def ,v ,x)
     (if ,v
       ,v
       (,test-failure ,line ,column ,(if err err (string/format "%j" x))))))

(defmacro test-deep=
  [form1 form2 &opt err]
  (def [l c] (tuple/sourcemap (dyn *macro-form* ())))

  (def v1 (gensym))
  (def v2 (gensym))

  (default err ~(string/format "\n  %.40m\n  \e[31m!=\e[0m\n  %.40m" ,v1 ,v2))

  ~(do
     (def ,v1 ,form1)
     (def ,v2 ,form2)
     (as-macro ,test
               (deep= ,v1 ,v2)
               ,err
               ,l ,c)))

(defmacro test=
  [form1 form2 &opt err]
  (def [l c] (tuple/sourcemap (dyn *macro-form* ())))

  (def v1 (gensym))
  (def v2 (gensym))

  (default err ~(string/format "\n  %.40m\n  \e[31m!=\e[0m\n  %.40m" ,v1 ,v2))

  ~(do
     (def ,v1 ,form1)
     (def ,v2 ,form2)
     (as-macro ,test
               (= ,v1 ,v2)
               ,err
               ,l ,c)))

(defn print-result
  "Run after all `test` calls."
  []
  (print-result* (get-suite))
  (put (get-suite) :nof-tests 0)
  (put (get-suite) :failed-tests @[]))

(comment
  # example usage

  (test (= 1 2))
  (test (= 1 1))
  (test (= "i am a big cat" "yeah so am i"))
  (test-deep= 10 20)
  (print-result)
  #
)
