(ns trees)
(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/math.numeric-tower "0.0.1"]])
(use '[clojure.pprint :only (pprint)])

(def data [
	{:not-surfacing 1 :flippers 1 :class "fish"}
	{:not-surfacing 1 :flippers 1 :class "fish"}
	{:not-surfacing 1 :flippers 0 :class "not-fish"}
	{:not-surfacing 0 :flippers 1 :class "not-fish"}
	{:not-surfacing 0 :flippers 1 :class "not-fish"}])

(def play-tennis
	[{:outlook "Sunny"    :temperature "Hot"  :humidity "High"   :wind "Weak"   :class "No"}
	{:outlook "Sunny"    :temperature "Hot"  :humidity "High"   :wind "Strong" :class "No"}
	{:outlook "Overcast" :temperature "Hot"  :humidity "High"   :wind "Weak"   :class "Yes"}
	{:outlook "Rain"     :temperature "Mild" :humidity "High"   :wind "Weak"   :class "Yes"}
	{:outlook "Rain"     :temperature "Cool" :humidity "Normal" :wind "Weak"   :class "Yes"}
	{:outlook "Rain"     :temperature "Cool" :humidity "Normal" :wind "Strong" :class "No"}
	{:outlook "Overcast" :temperature "Cool" :humidity "Normal" :wind "Strong" :class "Yes"}
	{:outlook "Sunny"    :temperature "Mild" :humidity "High"   :wind "Weak"   :class "No"}
	{:outlook "Sunny"    :temperature "Cool" :humidity "Normal" :wind "Weak"   :class "Yes"}
	{:outlook "Rain"     :temperature "Mild" :humidity "Normal" :wind "Weak"   :class "Yes"}
	{:outlook "Sunny"    :temperature "Mild" :humidity "Normal" :wind "Strong" :class "Yes"}
	{:outlook "Overcast" :temperature "Mild" :humidity "High"   :wind "Strong" :class "Yes"}
	{:outlook "Overcast" :temperature "Hot"  :humidity "Normal" :wind "Weak"   :class "Yes"}
	{:outlook "Rain"     :temperature "Mild" :humidity "High"   :wind "Strong" :class "No"}])

(def test-data
	[{:first 1 :second 10 :third 100 :fourth 1000 :class "yes"}
	{:first 1 :second 10 :third 100 :fourth 2000 :class "yes"}
	{:first 1 :second 10 :third 200 :fourth 3000 :class "yes"}
	{:first 1 :second 10 :third 200 :fourth 4000 :class "yes"}
	{:first 1 :second 20 :third 100 :fourth 1000 :class "yes"}
	{:first 1 :second 20 :third 100 :fourth 2000 :class "yes"}
	{:first 1 :second 20 :third 200 :fourth 3000 :class "no"}
	{:first 1 :second 20 :third 200 :fourth 4000 :class "yes"}
	{:first 2 :second 10 :third 100 :fourth 1000 :class "no"}
	{:first 2 :second 10 :third 100 :fourth 2000 :class "no"}
	{:first 3 :second 10 :third 200 :fourth 3000 :class "yes"}
	{:first 4 :second 10 :third 200 :fourth 4000 :class "no"}
	{:first 5 :second 20 :third 100 :fourth 1000 :class "yes"}
	{:first 6 :second 20 :third 100 :fourth 2000 :class "no"}
	{:first 7 :second 20 :third 200 :fourth 3000 :class "no"}
	{:first 8 :second 20 :third 200 :fourth 4000 :class "no"}])

(defn splittable-features [m]
	(filter #(not= :class %) (set (flatten (map keys m)))))

(defn log2 [n]
	(/ (Math/log n) (Math/log 2)))
	
(defn entropy [m]
	"Calculates Shannon entropy in dataset m"
	(let [c (count m)
		  occurrences (frequencies (map :class m))]
		(reduce - (cons 0 (map #(* (/ (second %) c) (log2 (/ (second %) c))) occurrences)))))

(defn split-dataset [m l v]
	"Takes map m, label l and value v. Returns a subset of a dataset where the value for a key/label l is smaller than value v."
	(map #(dissoc % l ) (filter #(= v (l %)) m)))

(defn info-gain
	([m k]
	"Given a key and a dataset, calculates the net reduction in entropy
	 caused by partitioning the dataset according to the values assoiated with the key."
		(info-gain (entropy m) m k))
	([current-entropy m k]
		(let [unique-vals (set (map k m))
			  parts (map #(split-dataset m k %) unique-vals)]
			(apply + (map #(- current-entropy (entropy %)) parts)))))

(defn most-informative-key [m features]
	(let [info-gains (apply assoc {}
								(interleave features 
											(map #(info-gain m %) features)))]
		(first (last (sort-by val info-gains)))))

(defn majority-count [m]
	(first (last (sort-by val (frequencies (map :class m))))))

(defn leaf-reached [m]
	(or (= 0.0 (entropy m))
		(= 1 (count (splittable-features m)))))

(defn subset [m f v]
	(filter #(= v (f %)) m))

(defn id3
	([m]
		(id3 m {}))
	([m t]
		(if (leaf-reached m)
			(majority-count m)
			(let [feature-to-split (most-informative-key m (splittable-features m))
				  values-for-feature (set (map feature-to-split m))]
				{feature-to-split (apply assoc {} (interleave values-for-feature (map #(id3 (subset m feature-to-split %) t) values-for-feature)))}))))

(clojure.pprint/pprint (id3 data))
(clojure.pprint/pprint (id3 play-tennis))
(clojure.pprint/pprint (id3 test-data))