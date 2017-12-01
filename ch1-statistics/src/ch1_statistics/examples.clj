(ns ch1-statistics.examples
  (:require [incanter.core :as icore]
            [incanter.stats :as istats]
            [incanter.charts :as icharts]
            [incanter.distributions :as idistributions]
            [ch1-statistics.data :as data]))


(defn uk-2010-column-names
  "Return all column names from the specified
   tabular data set"
  []
  (icore/col-names data/uk-election-data-2010))


(defn uk-2010-column-data
  "Get data for specified column name"
  [col-name]
  (icore/$ col-name data/uk-election-data-2010))

(defn uk-2010-frequency
  "Return frequency of values in the given column"
  [col-name]
  (->> (uk-2010-column-data col-name)
       frequencies))

;; Descriptive Statistics
(defn mean
  "Or average. he mean is the average of
   a set of numbers. Determined by adding
   them together and dividing by how many
   numbers are present"  [xs]
  (/ (reduce + xs) (count xs)))

(defn median
  "The median is the middle value between
   the smallest and largest of a set of
   numbers."
  [xs]
  (let [n (count xs)
        mid (int (/ n 2))
        sorted (sort xs)]
    (if (odd? n)
      (nth sorted mid)
      (->> sorted
           (drop (dec mid))
           (take 2)
           (mean)))))

(defn variance
  "Sums up the square of the deviation of
   values from the mean and creates the mean."
  [xs]
  (let [xm (mean xs)
        n (count xs)
        square-deviation (fn [x]
                           (icore/sq (- x xm)))]
    (-> (map square-deviation xs) mean)))

(defn standard-deviation
  "variance squares the deviation before
   taking the mean. Standard deviattion is the
   square root"
  [xs]
  (-> xs variance icore/sqrt))


;; Quantiles
(defn quantile
  [q xs]
  (let [n (dec (count xs))
        i (-> (* n q)
              (+ 1/2)
              (int))]
    (nth (sort xs) i)))


(defn five-number-summary
  "Calculate five number summary of specified list"
  [xs]
  (let [fq (fn [qn]
             (quantile qn xs))]
    (map fq [0 1/5 1/2 3/4 1])))


;;Bining data
(defn bin
  [n-bins xs]
  (let [minx (apply min xs)
        maxx (apply max xs)
        rangex (- maxx minx)
        bf (fn [x]
             (-> x
                 (- minx)
                 (/ rangex)
                 (* n-bins)
                 (int)
                 (min (dec n-bins))))]
    (map bf xs)))


;; Histogram
;;
(defn histo-electorate
  []
  (-> (uk-2010-column-data "Electorate")
      (icharts/histogram :nbins 20
                     :title "Electorate UK 2010"
                     :x-label "UK Electorate")
      (icore/view)))



;; Central limit theory
;; Values generated from diverse distributions tend to converge
;; to converge to the normal distribution. Such a distribution
;; requires only 2 numbers from which the rest can be approximated
;; - Mean
;; - Standard deviation

;; Using rand function as a fair random generator,
;; all floating point numbers between 0 and 1 have
;; an equal chance of being generated.
;; - 20 bins used
;; Results in a unfirom distribution
(defn uniform-distribution
  []
  (let [xs (->> (repeatedly rand)
                (take 10000))]
   (-> xs
       (icharts/histogram :x-label "Uniform Distribution"
                          :nbins 20)
       (icore/view))))

;; If instead of the generate value, we examine
;; the mean of a set of generated values we get
;; a graph resembling a normal distribution.
;; ----
;; Here we take the generated numbers (0.23 0.53 0.13 ...)
;; partion them into sets ((0.23 ...) (0.42 ...) (0.93 ...) ...)
;; map mean on these (map mean (..))
;; take 10000
;; draw histogram
;; CONCLUSION: the average affect of many small random fluctuations
;; leads to a normal distribution => CLT Central LIMIT THEOREM
(defn average-of-means
  []
  (let [xs (->> (repeatedly rand)
                (partition 10)
                (map mean)
                (take 10000))]
    (-> (icharts/histogram
          xs
          :nbins 20
          :x-label "Disribution of means")
        (icore/view))))

;;
(defn normal-distribution
  []
  (let [distribution (idistributions/normal-distribution)
        xs (->> (repeatedly #(idistributions/draw distribution))
                (take 10000))]
    (-> (icharts/histogram xs
                           :x-label "Normal distribution"
                           :nbins 20)
        (icore/view))))

;; Model an honest normal distribution
;; with a given mean and standard deviation
(defn normal-dist
  [mean sd]
  (let [ds (idistributions/normal-distribution mean sd)]
    (repeatedly #(idistributions/draw ds))))


;; draw histogram for an honests baker :)
;; Bread mean weight = 1000g
;;                sd = 30g
(defn honest-baker-histo
  []
  (-> (take 10000 (normal-dist 1000 30))
      (icharts/histogram
         :x-label "Honest Baker"
         :nbins 25)
      (icore/view)))

;; Model an skewed  normal distribution
;; with a given mean and standard deviation
(defn skewed-normal-dist
  [mean sd]
  (let [ds (idistributions/normal-distribution mean sd)]
    (->> (repeatedly #(idistributions/draw ds))
         (partition 13)
         (map (partial apply max)))))



;; draw histogram for an dishonests baker :)
;; Bread mean weight = 950g
;;                sd = 30g
(defn dishonest-baker
  []
  (-> (take 10000 (skewed-normal-dist 950 30))
      (icharts/histogram
        :x-label "Dishonest Baker"
        :nbins 25)
      (icore/view)))


;; Quantile-quantile plots
(defn qq-baker
  "Plot quatils of data agains the idealised
   normal distribution"
  []
  (->> (normal-dist 1000 30)
       (take 10000)
       (icharts/qq-plot)
       (icore/view))
  (->> (skewed-normal-dist 950 30)
       (take 10000)
       (icharts/qq-plot)
       (icore/view)))

;; Box plots for comparing median and variance
(defn box-plot-baker
  []
  (-> (icharts/box-plot (->> (normal-dist  1000 30)
                             (take 10000))
                        :legend true
                        :y-label "Weight"
                        :series-label "Honest Baker")
      (icharts/add-box-plot (->> (skewed-normal-dist 950 30)
                                 (take 10000))
                            :series-label "Dishonest Baker")
      (icore/view)))


;;CDF plot
(defn cdf-baker
  []
  (let [honest-sample (->> (normal-dist 1000 30)
                           (take 10000))
        dishonest-sample (->> (skewed-normal-dist 950 30)
                              (take 10000))
        empcdf-honest (istats/cdf-empirical honest-sample)
        empcdf-dishonest (istats/cdf-empirical dishonest-sample)]
    (-> (icharts/xy-plot honest-sample
                         (map empcdf-honest honest-sample)
                         :x-label "Loaf Weight"
                         :y-label "Probability"
                         :legend true
                         :series-label "Honest Baker")
        (icharts/add-lines dishonest-sample
                           (map empcdf-dishonest dishonest-sample)
                           :series-label "Dishonest Baker")
        (icore/view))))

;; Compare electorate data
(defn  cdf-electorate
  []
  (let [electorate (ch1-statistics.examples/uk-2010-column-data "Electorate")
        mean-electorate (istats/mean electorate)
        sd-electorate (istats/sd electorate)
        cdf-emp-fn (istats/cdf-empirical electorate)
        cdf-norm-fn (istats/cdf-normal electorate
                                       :mean mean-electorate
                                       :sd sd-electorate)]
    (-> (icharts/xy-plot electorate
                         cdf-norm-fn
                         :x-label "Electorate"
                         :y-label "Probability"
                         :series-label "Normal"
                         :legend true)
        (icharts/add-lines electorate
                           (map cdf-emp-fn electorate)
                           :series-label "Empirical")
        (icore/view))))

(defn qq-electorate
  []
  (->> (ch1-statistics.examples/uk-2010-column-data "Electorate")
       (icharts/qq-plot)
       (icore/view)))

;; Derived columns  1
;; Adding Con and Ld columns
;; NOTE: addition fails because some data is String
;; ClassCastException java.lang.String cannot be cast to java.lang.Number
;;                    clojure.lang.Numbers.add (Numbers.java:128)
;;
(defn add-con-ld
  []
  (->> data/uk-election-data-2010
       (icore/add-derived-column :victors
                                [:Con :LD]
                                (fn [c d] (+ c d)))))




;; Derived columns  2
;; Why the exception:
;; ClassCastException java.lang.String cannot be cast to java.lang.Number
;;                    clojure.lang.Numbers.add (Numbers.java:128)
;;
;; Because: addition fails because some data is String
;; Lets check which types the values in the columns "Con" and "LD" have
;; NOTE: will return something like this:
;;   - {java.lang.Double 631, java.lang.String 19} for "Con"
;;   - {java.lang.Double 631, java.lang.String 19} for "LD"
(defn data-types-in-col
  [col-name]
  (->> data/uk-election-data-2010
      (icore/$ col-name)
      (map type)
      (frequencies)))


;; Derived columns  2
;; Check what type of data is in these rows.
;;
;; |           Region | Electorate | Con | LD |
;; |------------------+------------+-----+----+
;; | Northern Ireland |    60204.0 |     |    |
;; | Northern Ireland |    73338.0 |     |    |
;; | Northern Ireland |    63054.0 |     |    |
(defn data-where-con-ld-nil
  []
  (->> data/uk-election-data-2010
       (icore/$where #(not-any? number? [(% "Con") (% "LD")]))
       (icore/$ [:Region :Electorate :Con :LD :Lab])))


;; Derived columns  3
;; Lets use the (load-data :uk-victors) to qq plot :victors-shares
;; and compare to compare against the normal distribution
(defn histo-victor-shares
  "Histogram of the share victors have (:Con + :LD) over total votes"
  []
  (let [dt (->> (data/load-data :uk-victors)
                (icore/$ :victors-share))]
    (-> dt
        (icharts/histogram :nbins 50
                           :x-label "UK victors share")
        (icore/view))))

(defn qq-plot-vitor-shares
  "QQ-plot of the share victors have (:Con + :LD) over total votes"
  []
  (->> (data/load-data :uk-victors)
       (icore/$ :victors-share)
       (icharts/qq-plot)
       (icore/view)))

;; Draw histogram of victors share
;; Some data is "Infinity"
(defn histo-victors-shares-ru
  []
  (let [dt (->> data/ru-victors-2010
                (icore/$where {:victors-share {:$ne Double/POSITIVE_INFINITY}})
                (icore/$ :victors-share))]

    (-> dt
        (icharts/histogram :nbins 20
                           :x-label "Victors share (RU)")
        (icore/view))))



(defn histo-victors-turnout-ru
  []
  (let [dt (->> data/ru-victors-2010
                (icore/$ :turnout))]

    (-> dt
        (icharts/histogram :nbins 20
                           :x-label "Russia turnout")
        (icore/view))))


(defn qq-plot-victors-turnout
  []
  (->> data/ru-victors-2010
      (icore/$ :turnout)
      (icharts/qq-plot)
      (icore/view)))