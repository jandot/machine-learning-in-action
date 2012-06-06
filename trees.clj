(ns trees)
(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/math.numeric-tower "0.0.1"]])
(use '[clojure.math.numeric-tower])

(def data [
	{:not-surfacing 1 :flippers 1 :class "fish"}
	{:not-surfacing 1 :flippers 1 :class "fish"}
	{:not-surfacing 1 :flippers 0 :class "not-fish"}
	{:not-surfacing 0 :flippers 1 :class "not-fish"}
	{:not-surfacing 0 :flippers 1 :class "not-fish"}])

(defn log2 [n]
	(/ (Math/log n) (Math/log 2)))
	
(defn entropy [m]
	"Calculates Shannon entropy in dataset m"
	(let [c (count (map :class m))
		  occurrences (frequencies (map :class m))]
		(reduce - (cons 0 (map #(* (/ (second %) c) (log2 (/ (second %) c))) occurrences)))))

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
		  info-gain-extended (group-by first (partition 2 (interleave (map first all-combinations) info-gain)))
		  labels (keys info-gain-extended)
		  sums (map #(reduce + (map second %)) (vals info-gain-extended))
		  ]
		(first (last (sort-by val (apply assoc {} (interleave labels sums)))))))
