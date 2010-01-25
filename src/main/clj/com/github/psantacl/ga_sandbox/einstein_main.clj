(ns com.github.psantacl.ga-sandbox.einstein-main
  (:require)
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
  (vec (.split s "\\s*\\|\\s*")))

(def *colors*        (split-pipe-str "blue   | green      | red     | white    | yellow"))
(def *nationalities* (split-pipe-str "Dane   | Englishman | German  | Swede    | Norwegian"))
(def *drinks*        (split-pipe-str "bier   | coffee     | milk    | tea      | water"))
(def *tobaccos*      (split-pipe-str "Blend  | BlueMaster | Dunhill | PallMall | Prince"))
(def *pets*          (split-pipe-str "birds  | cats       | dogs    | fish     | horses"))

(def *attributes* {:color       *colors*
                   :nationality *nationalities*
                   :drink       *drinks*
                   :tobacco     *tobaccos*
                   :pet         *pets*})

(def *rand* (java.util.Random.))

(defn rand-elt [lst]
  (nth lst (.nextInt *rand* (count lst))))

(defn random-house []
  [(rand-elt (:color       *attributes*))
   (rand-elt (:nationality *attributes*))
   (rand-elt (:drink       *attributes*))
   (rand-elt (:tobacco     *attributes*))
   (rand-elt (:pet         *attributes*))])

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
  ;;(prn (format "house-position: houses=%s pred=%s" hosues pred))
  (loop [pos 0
         [house & houses] houses]
    (cond (pred house)   pos
          (not house)    nil
          true           (recur (inc pos) houses))))

(defn has-neighbor [houses house-pos pred]
  (cond (= 0 house-pos)                         (pred (nth houses 1))
        (= 4 house-pos)                         (pred (nth houses 3))
        (pred (nth houses (dec house-pos)))     true
        (pred (nth houses (inc house-pos)))     true
        :else                                   false))


;; NB: can also score on uniqueness of data values (eg: 1 red house, 1 with Englishman)
(defn einstein-fitness-score [genome]
  (let [houses (get-houses genome)]
   (count (filter
           (fn [test]
             (let [{:keys [name pred]} test]
               (if (pred houses)
                 (do
                   (prn name ":hit!")
                   true)
                 (do
                   (prn name ":miss")
                   false))))
           [
            {:name "1. The Englishman lives in the red house."
             :pred (fn [houses]
              (some #(and (= "Englishman" (house-nationality %))
                          (= "red"        (house-color %)))
                    houses))}
            {:name "2. The Swede keeps dogs."
             :pred
             (fn [houses]
              (some #(and (= "Swede" (house-nationality %))
                          (= "dogs"  (house-pet %)))
                    houses))}
            {:name "3. The Dane drinks tea."
             :pred
             (fn [houses]
              (some #(and (= "Dane" (house-nationality %))
                          (= "tea"  (house-pet %)))
                    houses))}
;;             {:name "4. The green house is just to the left of the white one."
;;              :pred
;;              (fn [houses]
;;               (let [green-pos (house-position houses #(= "green" (house-color %)))
;;                     white-pos (house-position houses #(= "white" (house-color %)))]
;;                 (and green-pos
;;                      white-pos
;;                      (= 1 (- white-pos green-pos)))))}
            {:name "5. The owner of the green house drinks coffee."
             :pred
             (fn [houses]
              (some #(and (= "green"   (house-color %))
                          (= "coffee"  (house-drink %)))
                    houses))}
            {:name "6. The Pall Mall smoker keeps birds."
             :pred
             (fn [houses]
              (some #(and (= "PallMall"   (house-tobacco %))
                          (= "birds"      (house-pet %)))
                    houses))}
            {:name "7. The owner of the yellow house smokes Dunhills."
             :pred
             (fn [houses]
              (some #(and (= "yellow"    (house-color %))
                          (= "Dunhills"  (house-tobacco %)))
                    houses))}

            {:name "8. The man in the center house drinks milk."
             :pred
             (fn [houses]
               #(= "milk" (house-drink (nth houses 2))))}

            {:name "9. The Norwegian lives in the first house."
             :pred
             (fn [houses]
               #(= "Norwegian" (house-nationality (nth houses 0))))}

;; HERE
            {:name "10. The Blend smoker has a neighbor who keeps cats."
             :pred
             (fn [houses]
               (has-neighbor houses
                             (house-position houses #(= "Blend" (house-tobacco %)))
                             #(= "cats" (house-pet %))))}

            {:name "11. The man who smokes Blue Masters drinks bier."
             :pred
             (fn [houses]
              (some #(and (= "BlueMaster"    (house-tobacco %))
                          (= "bier"          (house-drink %)))
                    houses))}
            {:name "12. The man who keeps horses lives next to the Dunhill smoker."
             :pred
             (fn [houses]
               (has-neighbor houses
                             (house-position houses #(= "horses" (house-pet %)))
                             #(= "Dunhill" (house-tobacco %))))}
            {:name "13. The German smokes Prince."
             :pred
             (fn [houses]
              (some #(and (= "Prince"    (house-tobacco %))
                          (= "German"    (house-nationality %)))
                    houses))}

            {:name "14. The Norwegian lives next to the blue house."
             :pred
             (fn [houses]
               (has-neighbor houses
                             (house-position houses #(= "blue" (house-color %)))
                             #(= "Norwegian" (house-nationality %))))}
            {:name "15. The Blend smoker has a neighbor who drinks water."
             :pred
             (fn [houses]
               (has-neighbor houses
                             (house-position houses #(= "Blend" (house-tobacco %)))
                             #(= "water" (house-drink %))))}
            ]))))



(comment

  (def x (random-genome))

  x
  ["red" "Englishman" "tea" "Dunhill" "dogs" "blue" "Norwegian" "water" "PallMall" "horses" "red" "Swede" "bier" "PallMall" "dogs" "green" "Dane" "milk" "Blend" "birds" "red" "Englishman" "tea" "Prince" "horses"]
  (get-house x 1)
  (= x (vec (apply concat (map #(get-house x %) (range 0 5)))))

  (house-pet (get-house x 1))

  (get-houses x)

  (einstein-fitness-score ["red"   "Englishman"    "tea"   "Dunhill"   "dogs"
                           "blue"  "Norwegian"     "water" "PallMall"  "horses"
                           "green" "Swede"         "bier"  "PallMall"  "dogs"
                           "green" "Dane"          "milk"  "Blend"     "birds"
                           "blue"  "Englishman"    "tea"   "Prince"    "horses"])

  (einstein-fitness-score ["green"  "Englishman"    "tea"   "Dunhill"   "dogs"
                           "white"  "Norwegian"     "water" "PallMall"  "horses"
                           "green"  "Swede"         "bier"  "PallMall"  "dogs"
                           "green"  "Dane"          "milk"  "Blend"     "birds"
                           "blue"   "Englishman"    "tea"   "Prince"    "horses"])
)


(def *positions* [ :color :nationality :drink :tobacco :pet ])

(defn -main [& args]
  (prn "in teh mainz"))


