(ns logistic-regression)
(use '[leiningen.exec :only (deps)])
(use '[clojure.java.io :only (reader)])
(deps '[[incanter "1.3.0"]])
(use '[incanter.core])

; test-data looks like: ({:class 0, :x1 14.053064, :x0 -0.017612} {:class 1, :x1 4.662541, :x0 -1.395634} {:class 0, :x1 6.53862, :x0 -0.752157})
(def test-data
	(map #(apply assoc {} (interleave [:x0 :x1 :class] (map clojure.core/read-string (clojure.string/split % #"\t"))))
			(line-seq (reader "machinelearninginaction/Ch05/testSet.txt"))))

(def data-matrix (matrix (partition 3 (interleave (repeat 1) (map :x0 test-data) (map :x1 test-data)))))

(def labels (map :class test-data))

(defn sigmoid [z]
	(/ 1 (+ 1 (Math/exp (- z)))))

(defn weights [init c mat]
	(let [start-values init
		  alpha 0.001
		  error (minus labels (map sigmoid (mmult mat start-values)))
		  stop-values (plus start-values (mult alpha (mmult (trans mat) error)))]
		(if (> (+ c 1) 500)
			start-values
			(weights stop-values (+ c 1) mat))))

(println (weights (matrix 1 3 1) 1 data-matrix))