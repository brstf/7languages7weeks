(ns sevenlang.core)

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