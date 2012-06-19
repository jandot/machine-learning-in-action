(ns naive-bayes)
(use '[leiningen.exec :only (deps)])

(defn nil-to-zero [n]
	(if (nil? n)
		0.0
		n))

(defn vocab-list [m]
	(set (flatten (map :words m))))

(defn pc [m c]
	"pc = probability for class; m = matrix; c = class"
	(/ (count (filter #(= c %) (map :class m))) (count m)))

(defn pws [m]
	(let [f (frequencies (flatten (conj (map :words m) (seq (vocab-list m)))))] ; added vocab-list because we need to +1 for each
		(apply
			assoc {}
				(interleave (keys f)
							(map #(Math/log (/ % (+ 2 (count (flatten (map :words m)))))) (vals f))))))

(defn pw [m w]
	"pw = probability for word; m = matrix; w = word"
	(nil-to-zero (get (pws m) w)))

(defn sum-pw [m]
	(reduce + (vals (pws m))))

(defn pw-c [m c w]
	(let [sub-m (filter #(= (:class %) c) m)]
		(nil-to-zero (float (pw sub-m w)))))

(defn pw-cs [m c]
	(apply assoc {} (interleave (vocab-list m) (map #(pw-c m c %) (vocab-list m)))))

(defn sum-pw-c [m c]
	(reduce + (map #(pw-c m c %) (vocab-list m))))

(defn classify-vec [vec-to-classify p-hams p-spams p-spam]
	(let [clipped-vec-to-classify (vec (clojure.set/intersection (set vec-to-classify) (set (keys p-hams))))
	  	  p-ham (+ (reduce + (map #(get p-hams %) clipped-vec-to-classify)) (- 1.0 (Math/log p-spam)))
	  	  p-spam (+ (reduce + (map #(get p-spams %) clipped-vec-to-classify)) (Math/log p-spam))]
		(if (< p-ham p-spam)
			"ham"
			"spam")))

(defn classify [map-to-classify p-hams p-spams p-spam]
	{:predicted (:class map-to-classify) :observed (classify-vec (:words map-to-classify) p-hams p-spams p-spam)})

(defn mail-to-vec [s]
	(remove #(> 3 (count %)) (filter #(not= "" %) (clojure.string/split (clojure.string/lower-case s) #" |,|\.|\?|\r|\n|:|\)|\(|\""))))
	
(def ham-data
	(let [ham-files (map #(str "machinelearninginaction/Ch04/email/ham/" % ".txt") (range 1 26))]
		(map #(mail-to-vec (slurp %)) ham-files)))

(def spam-data
	(let [spam-files (map #(str "machinelearninginaction/Ch04/email/spam/" % ".txt") (range 1 26))]
		(map #(mail-to-vec (slurp %)) spam-files)))

(def all-data
	(concat (map #(assoc {} :words % :class "ham") ham-data) (map #(assoc {} :words % :class "spam") spam-data)))

(def training-indices (set (take 10 (shuffle (range (count all-data))))))
(def validation-indices (clojure.set/difference (set (range (count all-data))) training-indices))
(def training-data (map #(nth all-data %) training-indices))
(def validation-data (map #(nth all-data %) validation-indices))
(def p-hams (pw-cs training-data "ham"))
(def p-spams (pw-cs training-data "spam"))
(def predictions (map #(classify % p-hams p-spams 0.5) validation-data))
(def wrong-predictions (filter #(not= (:predicted %) (:observed %)) predictions))
(println (str "error rate: " (float (/ (count wrong-predictions) (count predictions)))))