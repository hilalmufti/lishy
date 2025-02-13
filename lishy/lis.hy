(require
 hyrule [loop -> some->])
(import
 math
 operator :as op
 toolz [first drop cons count last]
 hyrule [assoc coll? recur])


(defn rest [xs]
  (list (drop 1 xs)))


; TODO
(defn list-cons [x xs]
  (+ [x] xs))


(defn null? [x]
  (= x []))


(setv eq? op.is_)


(defn positive? [x]
  (> x 0))


(defn nonempty? [x]
  (positive? (count x)))


(defn if? [x]
  (and (nonempty? x) (= (first x) 'if)))

(defn define? [x]
  (and (nonempty? x) (= (first x) 'define)))


(defn quote? [x]
  (and (nonempty? x) (= (first x) 'quote)))

(defn lambda? [x]
  (and (nonempty? x) (= (first x) 'lambda)))

(defn assignment? [x]
  (and (nonempty? x) (= (first x) 'set!)))


(defn llist [#* xs]
  (list xs))


(defn list? [x]
  (isinstance x list))


(setv number #(int float))

(defn number? [x]
  (isinstance x number))


(setv procedure? callable)


(setv append op.add)


(defn second [xs]
  (get xs 1))

(defn third [xs]
  (get xs 2))

(defn fourth [xs]
  (get xs 3))


(defmacro reverse [xs]
  `(list (reversed ~xs)))

(defn symbol? [x]
  (isinstance x hy.models.Symbol))


(defn tokenize [s]
  (->
   s
   (.replace "(" " ( ")
   (.replace ")" " ) ")
   .split))

(defn parse [s]
  (-> s tokenize make-ast))


; TODO: make tail recursive with loop/recur
(defn make-ast [ts]
  (defn go [xs acc]
    (match xs
      [] (list (if acc (first acc) []))
      ["(" #* xs] (let [[ast rest] (go xs [])]
                    (go rest (cons ast acc)))
      [")" #* xs] [(reverse (list acc)) xs]
      [x #* xs] (go xs (cons (atom x) acc))))
  (go ts []))


(defn safer [f]
  (fn [#* a #** kw]
    (try
      (f #* a #** kw)
      (except [Exception]
              None))))


(setv make-int (safer int))
(setv make-float (safer float))
(setv make-symbol hy.models.Symbol)

(defn until-> [x #* fs]
  (match fs
    [] None
    [f #* fs] (match (f x)
                None (until-> x #* fs)
                _ :as y y)))

(defn atom [t]
  (until-> t make-int make-float make-symbol))

(defn make-env [[symbols []] [values []] [outer []]]
  (let [env {}]
    (do
      (.update env (zip symbols values))
      [env outer])))

(defn find-env [env symbol]
  (match env
    [] None
    [xs xss] :if (in symbol xs) xs
    [xs xss] (find-env xss symbol)))


(defn env-update [env kvs]
  (.update (first env) kvs))

(defn env-assoc [env k v]
  (assoc (first env) k v))

(defn env-get [env k]
  (get (find-env env k) k))

(defn make-procedure [params body env]
  ['procedure params body env])

(defn params [proc]
  (second proc))

(defn body [proc]
  (third proc))

(defn get-env [proc]
  (fourth proc))

; (body proc) = ['* 'pi ['* 'r 'r]]
; args = [3]
(defn apply [proc args]
  (if (procedure? proc)
    (proc #* args)
    (let [[_ params body env] proc]
      (eval body (make-env params args env)))))

(defn get-symbols [m]
  (dfor [k v] (.items (vars m)) (make-symbol k) v))

(defn make-std-env []
  (setv env (make-env))
  (env-update env (get-symbols math))
  (env-update env
              {'+ op.add
               '- op.sub
               '* op.mul
               '/ op.truediv
               '> op.gt
               '< op.lt
               '>= op.ge
               '<= op.le
               '= op.eq
               'abs abs
               'append op.add
               'apply (fn [f xs] (f #* xs))
               'begin (fn [#* xs] (last xs)) ; TODO: Huh?  
               'car first
               'cdr rest
               'cons list-cons
               'eq? eq?
               'expt pow
               'equal? op.eq
               'length count
               'list llist
               'list? list?
               'map map
               'max max
               'min min
               'not op.not_
               'null? null?
               'number? number?
               'print print
               'procedure? procedure?
               'round round
               'symbol? symbol?})
  env)


(setv program "(begin (define r 10) (* pi (* r r)))")
(setv list-program "(list (+ 1 1) (+ 2 2) (* 2 3) (expt 2 3))")
(setv ts (tokenize program))
(setv ast (parse program))

(setv global-env (make-std-env))




(defn eval [x [env global-env]]
  (cond
    (symbol? x) (get (find-env env x) x)
    (not (list? x)) x
    (quote? x) (first (rest x))
    (if? x) (let [[_ test conseq alt] x
                  exp (if (eval test env) conseq alt)]
              (eval exp env))
    (define? x) (let [[_ symbol exp] x]
                  (env-assoc env symbol (eval exp env)))
    (assignment? x) (let [[_ symbol exp] x]
                      (env-assoc (find-env env symbol) symbol (eval exp env)))
    (lambda? x) (let [[_ params body] x]
                  (make-procedure params body env))
    :else (let [proc (eval (first x) env)
                args (lfor arg (rest x) (eval arg env))]
            (apply proc args))))

; TODO: proper error handling
(defn repl [[prompt "lis.hy> "]]
  (loop []
    (let [v (-> prompt input parse eval)]
      (do
        (if v (print (schemestr v)) v)
        (recur)))))

(defn schemestr [exp]
  (if (list? exp)
    (+ "(" (.join " " (map schemestr exp)) ")")
    (str exp)))

