(ns com.github.psantacl.ga-sandbox.more-money
  (:require
      [com.github.psantacl.ga-sandbox.einstein-main :as ga]))

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
      (fn [d e m n o r s y]                          ;; they must all be different
        (= (count [d e m n o r s y])
           (count (seq (ga/list->set [d e m n o r s y])))))
      (fn [d e m n o r s y]
        (= (+ (Integer/parseInt (format "%d%d%d%d" s e n d))
              (Integer/parseInt (format "%d%d%d%d" m o r e)))
           (Integer/parseInt (format "%d%d%d%d%d" m o n e y))))])

(defn vec->int [v]
  (loop [res 0
         magnitude 1
         [v & vs] (reverse v)]
    (if v
      (recur (+ res (* magnitude v))
             (* magnitude 10)
             vs)
      res)))


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

(defn mm-fitness-score [genome]
  (let [[d e m n o r s y] genome
        score (count
               (filter
                (fn [test]
                  (test d e m n o r s y))
                *mm-fitness-predicates*))]
    (/ (* 1.0 score) (count *mm-fitness-predicates*))))

;; Talk to Paul about the idea of 'dead' chromosomes
;; (mm-fitness-score (mm-random-genome))
;  (mm-fitness-score [7 7 6 9 2 8 6 4])


(defn mm-random-genome []
  (vec (for [x (range 8)]
         (rand-int 10))))

(defn mm-mutate-genome [genome mutation-rate chromosome-mutation-rate]
  (if (<= (ga/rand-float) mutation-rate)
    (vec (for [idx (range (count genome))]
           (if (<= (ga/rand-float) chromosome-mutation-rate)
             (rand-int 10)
             (nth genome idx))))
    genome))

(comment

    (do
    (prn "starting simulation")
    (time (ga/run-simulation (ga/gen-population 1000 mm-random-genome)
                             {:stop-score     1.0
                              :max-iterations 500
                              :survival-rate  0.50
                              :mutator-fn     (fn [genome] (mm-mutate-genome genome 0.50 0.30))
                              :report-fn      (fn [generation-number [best & not-best] params]
                                                (println (format "best[%s]: %s" generation-number best))
                                                (pp-mm-genome (second best)))
                              :fitness-fn     mm-fitness-score})))

    (ga/stop-simulation)


)
