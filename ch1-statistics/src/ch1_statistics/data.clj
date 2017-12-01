(ns ch1-statistics.data
  (:require [clojure.java.io :as io]
            [incanter.core :as icore]
            [incanter.excel :as ixls]))




;; Multimethod for loading data based on
;; the identiy of the first argument
(defmulti load-data identity)

;; Unscrubbed data
;; Dispatch method for loading UK data provide by Medical University of Vienna
(defmethod load-data :uk [_]
  (-> (io/resource "UK2010.xls")
      (str)
      (ixls/read-xls)))



;; Scrubbed data
;; Dispatch method for loading scrubbed UK data
;; provide by Medical University of Vienna
(defmethod load-data :uk-scrubbed [_]
  (->> (load-data :uk)
       (icore/$where {"Election Year" {:$ne nil}})))

;; Data with derived columns for victors, share of vote and turnout
;;
;; Uses $where and filters out row with nil columns for :Con and :LD
;; then adds columns
;; :victors adding :Con and :LD
;; :victors-share dividing victor count with total voted
;; :turnout which is total votes to eclectorate
(defmethod load-data :uk-victors [_]
  (->> (load-data :uk-scrubbed)
       (icore/$where {:Con {:$fn number?} :LD {:$fn number?}})
       (icore/add-derived-column :victors [:Con :LD] (fn [cn ld] (+ cn ld)))
       (icore/add-derived-column :victors-share [:victors :Votes] (fn [vc vt] (/ vc vt)))
       (icore/add-derived-column :turnout [:Votes :Electorate] (fn [vt el] (/ vt el)))))


;; Implemenation of load-data for Russian data
(defmethod load-data :ru [_]
  (let [ru1 (-> (io/resource "Russia2011_1of2.xls")
                (str)
                (ixls/read-xls))
        ru2 (-> (io/resource "Russia2011_2of2.xls")
                (str)
                (ixls/read-xls))]
    (icore/conj-rows ru1 ru2)))



(defmethod load-data :ru-victors [_]
  (->> (load-data :ru)
       (icore/rename-cols {"Number of voters included in voters list" :electorate
                           "Number of invalid ballots" :valid-ballots
                           "United Russia" :victors})
       (icore/add-derived-column :victors-share
                                 [:victors :valid-ballots]
                                 icore/safe-div)
       (icore/add-derived-column :turnout
                                 [:valid-ballots :electorate]
                                 /)))




;; UK 2010 election data
;; SCRUBBED
(def uk-election-data-2010
  (load-data :uk-scrubbed))


;; RU 2010
(def ru-election-data-2010
  (load-data :ru))


(def ru-victors-2010
  (load-data :ru-victors))

;; Data Scrubbing
;; Detecting corrupt data and cleaning it
(defn query-nil-election-year
  "Search data with election year nil
   Does so by passing a map with predicates"
  []
  (-> uk-election-data-2010
      (icore/query-dataset {"Election Year" {:$eq nil}})
      (icore/to-map)))


