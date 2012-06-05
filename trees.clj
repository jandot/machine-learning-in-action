(ns trees)
(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/math.numeric-tower "0.0.1"]])
(use '[clojure.math.numeric-tower])

(def data [
	{:not-surfacing 1 :flippers 1 :fish "yes"}
	{:not-surfacing 1 :flippers 1 :fish "yes"}
	{:not-surfacing 1 :flippers 0 :fish "no"}
	{:not-surfacing 0 :flippers 1 :fish "no"}
	{:not-surfacing 0 :flippers 1 :fish "no"}])

(defn count-occurences [v]
	"Counts occurences in a vector"
	(partition 2 (interleave  (set v) (map #(count (filter #{%} v)) (set v)))))

(defn log2 [n]
	(/ (Math/log n) (Math/log 2)))
	
(defn entropy [m]
	"Calculates Shannon entropy in dataset m"
	(let [c (count m)
		  occurences (count-occurences m)]
		(reduce - (cons 0 (map #(* (/ (second %) c) (log2 (/ (second %) c))) occurences)))))

(defn split-dataset [m l v]
	"Takes map m, label l and value v. Returns a subset of a dataset where the value for a key/label l is smaller than value v."
	(filter #(< v (l %)) m))

(defn choose-best-feature-to-split [m features]
	"Chooses best feature from vector of features for given map m. Returns feature and value to split on."
	(let [base-entropy (entropy m)
		  features-with-values (partition 2 (interleave features (map #(set (map % m)) features)))
		  all-combinations (partition 2 (flatten (map #(interleave (repeat (first %)) (second %)) features-with-values)))
		  split-datasets (map #(split-dataset m (first %) (second %)) all-combinations)
		  info-gain (map #(- base-entropy (entropy %)) split-datasets)
		  info-gain-extended (partition 2 (interleave all-combinations info-gain))]
		(first (last (sort-by second info-gain-extended)))))
		
