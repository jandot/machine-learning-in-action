(ns knn)
(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/math.numeric-tower "0.0.1"]])
(use '[clojure.math.numeric-tower])

(load-file "machinelearninginaction/Ch02/trainingDigits/training-set.clj")
(load-file "machinelearninginaction/Ch02/testDigits/test-set.clj")

(defn calculate-distance [v1 v2]
	"Calculates distance between 2 vectors. The higher the result, the more different the vectors."
	(/ (count (filter #(= false %) (map = v1 v2))) (count v1)))

(defn count-occurences [v]
	"Counts occurences in a vector"
	(partition 2 (interleave  (set v) (map #(count (filter #{%} v)) (set v)))))

(defn majority-vote [v]
	"Calculates which item appears most often in a vector. CAUTION: in case two items appear equally often, will pick at random"
	(first (last (sort-by second (count-occurences v)))))

(defn classify [sample training-set k]
	(let [distances (pmap #(calculate-distance (:pattern %) sample) training-set)
		  labels (map :label training-set)
		  distances-with-labels (map first (take k (sort-by second (partition 2 (interleave labels distances)))))]
		(majority-vote distances-with-labels)))

(def sample-numbers (map int (take 100 (repeatedly #(rand 946)))))
(def sample-test-set (map #(nth test-set %) sample-numbers))
(def predictions (pmap #(classify (:pattern %) training-set 3) sample-test-set))
(partition 2 (interleave (map :label sample-test-set) predictions))