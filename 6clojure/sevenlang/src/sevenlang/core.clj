(ns sevenlang.core)

; Day 1:
(defn big 
    "Return true if string st has more than n characters."
    [st n]
    (> (count st) n))

(defn collection-type
    "Return the collection type of col (:vector, :list, :map"
    [col]
    (if (= (class col) clojure.lang.PersistentList) :list
        (if (= (class col) clojure.lang.PersistentVector) :vector
            (if (= (class col) clojure.lang.PersistentArrayMap) :map))))

; Day 2:

; Unless macro implementing an else body
(defmacro unless [test body else_body] (list 'if (list 'not test) body else_body))

; Define a type that implements a protocol
; the protocol:
(defprotocol Animal
    (speak [c]))

(defrecord Cat [] 
    Animal
    (speak [_] (println "Meow!")))

(defrecord Dog []
    Animal
    (speak [_] (println "Bark!")))

(defrecord Python []
    Animal
    (speak [_] (println "Spam spam spam!")))

(defn animal_farm
  "Tests the Animal type."
  []
  (def c (Cat.))
  (def d (Dog.))
  (def p (Python.))
  (speak c)
  (speak d)
  (speak p))