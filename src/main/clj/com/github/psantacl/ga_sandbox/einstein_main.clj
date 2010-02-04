(ns com.github.psantacl.ga-sandbox.einstein-main
  (:gen-class))

;; http://www.stanford.edu/~laurik/fsmbook/examples/Einstein%27sPuzzle.html

;;    1. The Englishman lives in the red house.
;;    2. The Swede keeps dogs.
;;    3. The Dane drinks tea.
;;    4. The green house is just to the left of the white one.
;;    5. The owner of the green house drinks coffee.
;;    6. The Pall Mall smoker keeps birds.
;;    7. The owner of the yellow house smokes Dunhills.
;;    8. The man in the center house drinks milk.
;;    9. The Norwegian lives in the first house.
;;   10. The Blend smoker has a neighbor who keeps cats.
;;   11. The man who smokes Blue Masters drinks bier.
;;   12. The man who keeps horses lives next to the Dunhill smoker.
;;   13. The German smokes Prince.
;;   14. The Norwegian lives next to the blue house.
;;   15. The Blend smoker has a neighbor who drinks water.

;; 5 houses
;;  - color
;;  - nationality
;;  - drink
;;  - tobacco
;;  - pet

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


(def *colors*            (split-pipe-str "blue   | green      | red     | white    | yellow"))
(def *colors-set*        (list->set *colors*))
(def *nationalities*     (split-pipe-str "Dane   | Englishman | German  | Swede    | Norwegian"))
(def *nationalities-set* (list->set *nationalities*))
(def *drinks*            (split-pipe-str "bier   | coffee     | milk    | tea      | water"))
(def *drinks-set*        (list->set *drinks*))
(def *tobaccos*          (split-pipe-str "Blend  | BlueMaster | Dunhill | PallMall | Prince"))
(def *tobaccos-set*      (list->set *tobaccos*))
(def *pets*              (split-pipe-str "birds  | cats       | dogs    | fish     | horses"))
(def *pets-set*          (list->set *pets*))

(def *attributes* {:color       *colors*
                   :nationality *nationalities*
                   :drink       *drinks*
                   :tobacco     *tobaccos*
                   :pet         *pets*})

(def *all-attribute-values*
     (apply concat (vals *attributes*)))

(def *rand* (java.util.Random.))

(defn flip-coin []
  (= 0 (.nextInt *rand* 2)))

(defn rand-elt [lst]
  (nth lst (.nextInt *rand* (count lst))))

(defn random-house []
  [(rand-elt (:color       *attributes*))
   (rand-elt (:nationality *attributes*))
   (rand-elt (:drink       *attributes*))
   (rand-elt (:tobacco     *attributes*))
   (rand-elt (:pet         *attributes*))])

(def *genome-template*
     (vec (apply concat (for [x (range 5)]
                          [(:color       *attributes*)
                           (:nationality *attributes*)
                           (:drink       *attributes*)
                           (:tobacco     *attributes*)
                           (:pet         *attributes*)]))))

;; (random-house)

(defn random-genome []
  (vec (apply concat (for [x (range 5)]
                       (random-house)))))

;; (random-genome)

(defn get-house [genome house-number]
  (let [offset (* house-number 5)]
    (vec (for [idx (range offset (+ offset 5))]
           (nth genome idx)))))

(defn get-houses [genome]
  (vec (map #(get-house genome %) (range 0 5))))


(defn house-color       [house] (nth house 0))
(defn house-nationality [house] (nth house 1))
(defn house-drink       [house] (nth house 2))
(defn house-tobacco     [house] (nth house 3))
(defn house-pet         [house] (nth house 4))

(defn house-position [houses pred]
  (loop [pos 0
         [house & houses] houses]
    (cond (pred house)   pos
          (not house)    nil
          true           (recur (inc pos) houses))))

(defn has-neighbor [houses house-pos pred]
  (cond (not house-pos)                         false
        (= 0 house-pos)                         (pred (nth houses 1))
        (= 4 house-pos)                         (pred (nth houses 3))
        (pred (nth houses (dec house-pos)))     true
        (pred (nth houses (inc house-pos)))     true
        :else                                   false))


(def *einstein-score-fns*
     [{:name "1. The Englishman lives in the red house."
       :pred (fn [houses]
               (some #(and (= :Englishman (house-nationality %))
                           (= :red        (house-color %)))
                     houses))}
      {:name "2. The Swede keeps dogs."
       :pred
       (fn [houses]
         (some #(and (= :Swede (house-nationality %))
                     (= :dogs  (house-pet %)))
               houses))}
      {:name "3. The Dane drinks tea."
       :pred
       (fn [houses]
         (some #(and (= :Dane (house-nationality %))
                     (= :tea  (house-drink %)))
               houses))}
      {:name "4. The green house is just to the left of the white one."
       :pred
       (fn [houses]
         (let [green-pos (house-position houses #(= :green (house-color %)))
               white-pos (house-position houses #(= :white (house-color %)))]
           (and green-pos
                white-pos
                (= 1 (- white-pos green-pos)))))}
      {:name "5. The owner of the green house drinks coffee."
       :pred
       (fn [houses]
         (some #(and (= :green   (house-color %))
                     (= :coffee  (house-drink %)))
               houses))}
      {:name "6. The Pall Mall smoker keeps birds."
       :pred
       (fn [houses]
         (some #(and (= :PallMall   (house-tobacco %))
                     (= :birds      (house-pet %)))
               houses))}
      {:name "7. The owner of the yellow house smokes Dunhill."
       :pred
       (fn [houses]
         (some #(and (= :yellow    (house-color %))
                     (= :Dunhill  (house-tobacco %)))
               houses))}
      {:name "8. The man in the center house drinks milk."
       :pred
       (fn [houses]
         #(= :milk (house-drink (nth houses 2))))}
      {:name ":9 The Norwegian lives in the first house."
       :pred
       (fn [houses]
         #(= :Norwegian (house-nationality (nth houses 0))))}
      {:name "10. The Blend smoker has a neighbor who keeps cats."
       :pred
       (fn [houses]
         (has-neighbor houses
                       (house-position houses #(= :Blend (house-tobacco %)))
                       #(= :cats (house-pet %))))}
      {:name "11. The man who smokes Blue Masters drinks bier."
       :pred
       (fn [houses]
         (some #(and (= :BlueMaster    (house-tobacco %))
                     (= :bier          (house-drink %)))
               houses))}
      {:name "12. The man who keeps horses lives next to the Dunhill smoker."
       :pred
       (fn [houses]
         (has-neighbor houses
                       (house-position houses #(= :horses (house-pet %)))
                       #(= :Dunhill (house-tobacco %))))}
      {:name "13. The German smokes Prince."
       :pred
       (fn [houses]
         (some #(and (= :Prince    (house-tobacco %))
                     (= :German    (house-nationality %)))
               houses))}
      {:name "14. The Norwegian lives next to the blue house."
       :pred
       (fn [houses]
         (has-neighbor houses
                       (house-position houses #(= :blue (house-color %)))
                       #(= :Norwegian (house-nationality %))))}
      {:name "15. The Blend smoker has a neighbor who drinks water."
       :pred
       (fn [houses]
         (has-neighbor houses
                       (house-position houses #(= :Blend (house-tobacco %)))
                       #(= :water (house-drink %))))}])

(def *uniqueness-predicates*
  [
   {:name "unique.1 color"
    :pred (fn [houses] (= (count (list->set (map house-color       houses))) (count houses)))}
   {:name "unique.2 nationality"
    :pred (fn [houses] (= (count (list->set (map house-nationality houses))) (count houses)))}
   {:name "unique.3 drink"
    :pred (fn [houses] (= (count (list->set (map house-drink       houses))) (count houses)))}
   {:name "unique.4 tobacco"
    :pred (fn [houses] (= (count (list->set (map house-tobacco     houses))) (count houses)))}
   {:name "unique.5 pet"
    :pred (fn [houses] (= (count (list->set (map house-pet         houses))) (count houses)))}
   ])

(def *all-fitness-predicates* (concat *einstein-score-fns* *uniqueness-predicates*))

;; NB: can also score on uniqueness of data values (eg: 1 red house, 1 with Englishman)
(defn einstein-fitness-score [genome]
  (log "einstein-fitness-score: genome=%s" genome)
  (let [houses (get-houses genome)
        score (count (filter
                      (fn [test]
                        (let [{:keys [name pred]} test]
                          (if (pred houses)
                            (do
                              (log "%s :hit!" name)
                              true)
                            (do
                              (log "%s: miss" name)
                              false))))
                      *all-fitness-predicates*))]
    (/ (* 1.0 score) (count *all-fitness-predicates*))))

(defn gen-population [size]
  (for [x (range 0 size)]
    (random-genome)))

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
        target       (* (.nextFloat *rand*) total-weight)]
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

(defn mutate-genome-1 [genome mutation-rate chromosome-mutation-rate]
  (if (<= (.nextFloat *rand*) mutation-rate)
    (vec (map (fn [chromosome]
                (if (<= (.nextFloat *rand*) chromosome-mutation-rate)
                  (rand-elt *all-attribute-values*)
                  chromosome))
              genome))
    genome))

(defn mutate-genome [genome mutation-rate chromosome-mutation-rate]
  (if (<= (.nextFloat *rand*) mutation-rate)
    (vec (for [idx (range (count genome))]
           (if (<= (.nextFloat *rand*) chromosome-mutation-rate)
                   (rand-elt (nth *genome-template* idx))
                   (nth genome idx))))
    genome))

(defn random-chromosome-swap [genome]
  (let [chromosome-offset (rand-int 5)
        ch1 (+ chromosome-offset (* 5 (rand-int 5)))
        ch2 (+ chromosome-offset (* 5 (rand-int 5)))]
    (assoc genome ch1 (nth genome ch2) ch2 (nth genome ch1))))

(defn mutate-genome+chromosome-swap [genome mutation-rate chromosome-mutation-rate]
  (if (<= (.nextFloat *rand*) mutation-rate)
    (if (flip-coin) ;; make this another parameter? -- the swap rate?
      (random-chromosome-swap genome)
      (vec (for [idx (range (count genome))]
             (if (<= (.nextFloat *rand*) chromosome-mutation-rate)
               (rand-elt (nth *genome-template* idx))
               (nth genome idx)))))
    genome))

(def *mutation-rate* (atom 0.5))
(def *chromosome-mutation-rate* (atom 0.25))

(defn make-next-generation [ranked-population mutator-fn survival-rate]
  (let [population-size   (count ranked-population)
        survivors         (take (* (count ranked-population) survival-rate)
                                ranked-population)]
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

(defn run-simulation [initial-population params]
  (let [stop-score        (:stop-score params)
        max-iterations    (:max-iterations params)
        fitness-fn        (:fitness-fn params)
        mutator-fn        (:mutator-fn params)
        survival-rate     (:survival-rate params)
        report-fn         (:report-fn params (fn [generation-number ranked-population params]
                                               (println (format "\nbest[%s]: %s" generation-number (first ranked-population)))))]
    (loop [population        initial-population
           generation-number 1]
      (let [ranked-population (rank-population population fitness-fn)]
        (report-fn generation-number ranked-population params)
        (if (or
             (<= max-iterations generation-number)
             (>= (first (first ranked-population))
                 stop-score))
          (do
            (println (format "Simulation Terminated: best:%s params:%s" (first ranked-population) params))
            (first ranked-population)) ;; done, win?
          (recur (make-next-generation ranked-population mutator-fn survival-rate)
                 (inc generation-number)))))))

(comment

  (do
    (prn "starting simulation")
    (time (run-simulation (gen-population 1000)
                          {:stop-score     1.0
                           :max-iterations 250
                           :survival-rate  0.60
                           :mutator-fn     (fn [genome] (mutate-genome+chromosome-swap genome 0.40 0.30))
                           :report-fn      (fn [generation-number [best & not-best] params]
                                             (println (format "best[%s]: %s" generation-number best))
                                             (doseq [spec *all-fitness-predicates*]
                                               (if (not ((:pred spec) (get-houses (second best))))
                                                 (println (format "  failed: %s" (:name spec))))))
                           :fitness-fn     einstein-fitness-score
                           })))

  )


(def *positions* [ :color :nationality :drink :tobacco :pet ])




(defn -main [& args]
  (time (run-simulation (gen-population 1000)
                        {:stop-score     1.0
                         :max-iterations 1000
                         :survival-rate  0.50
                         :mutator-fn     (fn [genome] (mutate-genome genome @*mutation-rate* @*chromosome-mutation-rate*))
                         :fitness-fn     einstein-fitness-score
                         })))



