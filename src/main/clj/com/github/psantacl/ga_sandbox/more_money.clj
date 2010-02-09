(ns com.github.psantacl.ga-sandbox.more-money
  (:require
      [com.github.psantacl.ga-sandbox.framework :as ga]))

(def *puzzle* "
       S E N D
   +   M O R E
   -------------
   = M O N E Y
")

;; (ga/list->set (seq(rest (.split "sendmoremoney" ""))))
;; #{"d" "e" "m" "n" "o" "r" "s" "y"}
;; (count #{"d" "e" "m" "n" "o" "r" "s" "y"}) => 8
;; (sort (seq #{"d" "e" "m" "n" "o" "r" "s" "y"}))
;;  => ("d" "e" "m" "n" "o" "r" "s" "y")
;; d e m n o r s y

(defn vec->int [v]
  (loop [res 0
         magnitude 1
         [v & vs] (reverse v)]
    (if v
      (recur (+ res (* magnitude v))
             (* magnitude 10)
             vs)
      res)))

(defn partial-test [d1 d2 expected]
  (let [n1 (vec->int d1)
        n2 (vec->int d2)
        expected (vec->int expected)
        mag (int (Math/pow 10 (count d1)))
        res (mod (+ n1 n2) mag)]
    ; (println (format "partial-test: n1=%d n2=%d expected=%d mag=%d res=%d" n1 n2 expected mag res))
    (= expected res)))

;; (partial-test [9 9] [9 9] [9 8])
;; (partial-test [n d] [r e] [e y])

(def *mm-fitness-predicates*
     [(fn [d e m n o r s y] (= (mod (+ d e) 10) y))
      (fn [d e m n o r s y] (partial-test [n d] [r e] [e y]))
      (fn [d e m n o r s y] (partial-test [e n d] [o r e] [n e y]))
      (fn [d e m n o r s y] (not (= 0 s)))           ;; leading digits may not be zero
      (fn [d e m n o r s y] (not (= 0 m)))           ;; leading digits may not be zero
      ;; the more finely the scoring is, the better the exploration will go
      ;; the more completely it tests for a solution the better it will go as well

      (fn [d e m n o r s y] (not (= d e)))
      (fn [d e m n o r s y] (not (= d m)))
      (fn [d e m n o r s y] (not (= d n)))
      (fn [d e m n o r s y] (not (= d o)))
      (fn [d e m n o r s y] (not (= d r)))
      (fn [d e m n o r s y] (not (= d s)))
      (fn [d e m n o r s y] (not (= d y)))

      (fn [d e m n o r s y] (not (= e m)))
      (fn [d e m n o r s y] (not (= e n)))
      (fn [d e m n o r s y] (not (= e o)))
      (fn [d e m n o r s y] (not (= e r)))
      (fn [d e m n o r s y] (not (= e s)))
      (fn [d e m n o r s y] (not (= e y)))

      (fn [d e m n o r s y] (not (= m n)))
      (fn [d e m n o r s y] (not (= m o)))
      (fn [d e m n o r s y] (not (= m r)))
      (fn [d e m n o r s y] (not (= m s)))
      (fn [d e m n o r s y] (not (= m y)))

      (fn [d e m n o r s y] (not (= n o)))
      (fn [d e m n o r s y] (not (= n r)))
      (fn [d e m n o r s y] (not (= n s)))
      (fn [d e m n o r s y] (not (= n y)))

      (fn [d e m n o r s y] (not (= o r)))
      (fn [d e m n o r s y] (not (= o s)))
      (fn [d e m n o r s y] (not (= o y)))

      (fn [d e m n o r s y] (not (= r s)))
      (fn [d e m n o r s y] (not (= r y)))

      (fn [d e m n o r s y] (not (= s y)))

      (fn [d e m n o r s y]                          ;; they must all be different
        (= (count [d e m n o r s y])
           (count (seq (ga/list->set [d e m n o r s y])))))
      (fn [d e m n o r s y]
        (= (+ (Integer/parseInt (format "%d%d%d%d" s e n d))
              (Integer/parseInt (format "%d%d%d%d" m o r e)))
           (Integer/parseInt (format "%d%d%d%d%d" m o n e y))))])

(defn mm-fitness-score [genome]
  (let [[d e m n o r s y & other] genome
        tests-passed (count
                      (filter
                       (fn [test]
                         (test d e m n o r s y))
                       *mm-fitness-predicates*))
        score (/ (* 1.0 tests-passed) (count *mm-fitness-predicates*))]
    (Math/pow score 3)))


(defn pp-mm-genome [genome]
  (let [[d e m n o r s y] genome]
    (println (format "   %d%d%d%d" s e n d))
    (println (format " + %d%d%d%d" m o r e))
    (println (format " ----------"))
    (println (format "  %d%d%d%d%d (%3.2f%% %s)"
                     m o n e y
                     (* 100 (mm-fitness-score genome))
                     (if (= 1.0 (mm-fitness-score genome))
                       "WIN"
                       "FAIL")))))

;; (pp-mm-genome [7 7 6 9 2 8 6 4])

;; Talk to Paul about the idea of 'dead' chromosomes
;; (mm-fitness-score (mm-random-genome))
;  (mm-fitness-score [7 7 6 9 2 8 6 4])
;; O = 0, M = 1, Y = 2, E = 5, N = 6, D = 7, R = 8, and S = 9.
;  (mm-fitness-score [7 5 1 6 0 8 9 2])

(defn mm-random-genome []
  (vec (for [x (range 12)] ;; 8 is the correct # of digits
         (rand-int 10))))

(defn mm-mutate-genome [genome mutation-rate chromosome-mutation-rate]
  (if (<= (ga/rand-float) mutation-rate)
    (if (ga/flip-coin)
      (ga/random-chromosome-swap genome)
      (vec (for [idx (range (count genome))]
             (if (<= (ga/rand-float) chromosome-mutation-rate)
               (rand-int 10)
               (nth genome idx)))))
    genome))


(defn mm-survives [ranked-population]
  (let [survivors (ga/random-weighted-survives ranked-population (* 0.80 (count ranked-population)))]
    (println (format "mm-survives: out of %s, %s survived this round"
                     (count ranked-population)
                     (count survivors)))
    survivors))

(defn mm-report [generation-number population params]
  (let [[best & not-best] population
        histogram (ga/histogram-population population)]
    (println (format "best[%s]: %s, avg: %3.2f%%" generation-number best
                     (* 100.0 (/ (apply + (map first population)) (count population)))))
    (println (format "%s unique genomes, %3.2f diversity, top 10 are %3.2f%% of the population"
                     (count (keys histogram))
                     (/ (* 1.0 (count (keys histogram)))
                        (count population))
                     (/ (* 1.0 (apply + (map first (take 10 histogram))))
                        (count population))))
    (pp-mm-genome (second best))))

(comment

    (do
    (prn "starting simulation")
    (time (ga/run-simulation (ga/gen-population 750 mm-random-genome)
                             {:stop-score     1.0
                              :max-iterations 500
                              :survival-fn    mm-survives
                              :mutator-fn     (fn [genome] (mm-mutate-genome genome 0.20 0.40))
                              :report-fn      mm-report
                              :fitness-fn     mm-fitness-score})))

    (ga/stop-simulation)


)
