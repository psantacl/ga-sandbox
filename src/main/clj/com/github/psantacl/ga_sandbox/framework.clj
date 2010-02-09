(ns com.github.psantacl.ga-sandbox.framework
  (:gen-class))

(defn split-pipe-str [s]
  (vec (map keyword (.split s "\\s*\\|\\s*"))))

(defn list->set [things]
  (loop [res #{}
         [thing & things] things]
    (if thing
      (recur (conj res thing) things)
      res)))

(def *verbose* false)

(defn log [& stuff]
  (if *verbose*
    (prn (apply format stuff))))


(def *rand* (java.util.Random.))

(defn rand-float []
  (.nextFloat *rand*))

(defn flip-coin []
  (= 0 (.nextInt *rand* 2)))

(defn rand-elt [lst]
  (nth lst (.nextInt *rand* (count lst))))

(defn scored-population [population fitness-fn]
  (pmap
   (fn [g] [(fitness-fn g) g])
   population))


(defn rank-population [population fitness-fn]
  (sort (fn [a b]
          (> (first a)
             (first b)))
        (scored-population population fitness-fn)))

(defn rand-scored-pair-weighted [pairs]
  (let [total-weight (apply + (map first pairs))
        target       (* (rand-float) total-weight)]
    (loop [[elt & pairs] pairs
           score         0]
      (log "rand-scored-pair-weighted elt=%s score=%s target=%s" elt score target)
      (cond (not elt)                         nil
            (< target (+ (first elt) score))  elt
            :else                             (recur pairs (+ score (first elt)))))))

(defn breed-new-genome [scored-population]
  (let [[fscore father] (rand-scored-pair-weighted scored-population)
        [mscore mother] (rand-scored-pair-weighted scored-population)]
    (log "breed-new-genome: father=%s mother=%s" father mother)
    (vec (map (fn [idx]
                 (if (flip-coin)
                   (nth father idx)
                   (nth mother idx)))
               (range (count father))))))


(defn make-next-generation [ranked-population mutator-fn survival-fn]
  (let [population-size   (count ranked-population)
        survivors         (survival-fn ranked-population)]
    (log "contraulations to %s of you, you survived! (out of %s, with %s deaths, sorry, please play again)"
            (count survivors)
            (count ranked-population)
            (- (count ranked-population)
               (count survivors)))
    ;; NB: we know how many we have to create - why not use a (for [nn in (range (/ (count ranked-population 2)))] (breed...))
    ;; it should be significantly faster than a loop/recur
    (loop [new-population (vec (map #(mutator-fn (second %)) survivors))]
      (if (= population-size (count new-population))
        new-population
        (do
          (recur (conj new-population
                       (mutator-fn
                        (breed-new-genome survivors)))))))))

(def *stop-simulation* (atom false))

(defn stop-simulation []
  (reset! *stop-simulation* true))

(defn random-chromosome-swap [genome]
  (let [ch1 (rand-int (count genome))
        ch2 (rand-int (count genome))]
    (assoc genome
      (nth genome ch2)
      (nth genome ch1))))

(defn make-top-n-percent-survives [n]
  (fn [ranked-population]
   (take (* (count ranked-population) n)
         ranked-population)))

(defn random-weighted-survives [ranked-population max]
  (loop [survivors []]
    (if (>= (count survivors)
            max)
      survivors
      (recur (cons (rand-scored-pair-weighted ranked-population)
                   survivors)))))

(defn gen-population [size random-genome]
  (for [x (range 0 size)]
    (random-genome)))


(defn count-scored-population [scored-population]
  (reduce (fn [val [score genome]]
            (assoc val
              genome
              (inc (val genome 0))))
          {}
          scored-population))

(defn histogram-population [scored-population]
  (let [counts (count-scored-population scored-population)
        pairs  (reduce (fn [res k]
                         (cons [(counts k) k]
                               res))
                       []
                       (keys counts))]
    (sort (fn [a b]
            (> (first a)
               (first b)))
          pairs)))

(defn run-simulation [initial-population params]
  (let [stop-score        (:stop-score params)
        max-iterations    (:max-iterations params)
        fitness-fn        (:fitness-fn params)
        mutator-fn        (:mutator-fn params)
        survival-fn       (:survival-fn params (make-top-n-percent-survives 0.60))
        report-fn         (:report-fn params (fn [generation-number ranked-population params]
                                               (println (format "\nbest[%s]: %s" generation-number (first ranked-population)))))]
    (reset! *stop-simulation* false)
    (loop [population        initial-population
           generation-number 1]
      (let [ranked-population (rank-population population fitness-fn)]
        (report-fn generation-number ranked-population params)
        (if (or
             @*stop-simulation*
             (<= max-iterations generation-number)
             (>= (first (first ranked-population))
                 stop-score))
          (do
            (println (format "Simulation Terminated: best:%s params:%s" (first ranked-population) params))
            (first ranked-population)) ;; done, win?
          (recur (make-next-generation ranked-population mutator-fn survival-fn)
                 (inc generation-number)))))))


