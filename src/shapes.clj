(ns shapes
  (:require [criterium.core :as criterium :refer [bench quick-bench]]
            [tech.v3.datatype :as dtype]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.datatype.emap :as emap]
            [tech.v3.datatype.unary-op :as unop]
            [tech.v3.datatype.binary-op :as binop]))





(defrecord Shape [type width height])

(def coeff
  {:square 1.0
   :rectangle 1.0
   :triangle 0.5
   :circle Math/PI})
(defn get-area-switch [shape]
  (*
   ^double (:width shape)
   ^double (:height shape)
   ^double (coeff
            (:type shape))
   ;; ^double
   ;; (case (:type shape)
   ;;   :square 1.0
   ;;   :rectangle 1.0
   ;;   :triangle 0.5
   ;;   :circle Math/PI)
   ))

(defn total-area-switch [shapes]
  (transduce (map get-area-switch)
             +
             0.0
             shapes))

(def switch-shapes
  (into []
        (map (fn [_]
               (->Shape (rand-nth [:square
                                   :rectangle
                                   :triangle
                                   :circle])
                        (* 10 (rand))
                        (* 10 (rand)))))
        (range 1000)))
(comment
  (bench
   (total-area-switch switch-shapes))


;; Evaluation count : 2991780 in 60 samples of 49863 calls.
;;              Execution time mean : 20.658554 µs
;;     Execution time std-deviation : 370.381137 ns
;;    Execution time lower quantile : 19.890768 µs ( 2.5%)
;;    Execution time upper quantile : 21.243919 µs (97.5%)
;;                    Overhead used : 1.854505 ns

  (set! *unchecked-math* true)
;; Evaluation count : 3741240 in 60 samples of 62354 calls.
;;              Execution time mean : 16.306733 µs
;;     Execution time std-deviation : 159.431064 ns
;;    Execution time lower quantile : 16.032687 µs ( 2.5%)
;;    Execution time upper quantile : 16.667047 µs (97.5%)
;;                    Overhead used : 1.854505 ns

;; Found 5 outliers in 60 samples (8.3333 %)
;; 	low-severe	 1 (1.6667 %)
;; 	low-mild	 4 (6.6667 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers



  ;; sorted
  (let [sorted-switch-shapes (vec
                              (sort-by :type switch-shapes))]
    (bench
     (total-area-switch sorted-switch-shapes )))

  ,)


(dt-struct/define-datatype! :shape
    [{:name :type :datatype :int64}
     {:name :width :datatype :float64}
     {:name :height :datatype :float64}])

(def type->int
  {:square 1
   :rectangle 2
   :triangle 3
   :circle 4})
(def type->constant
  {1 1
   2 1
   3 0.5
   4 Math/PI})

(comment
  (let [structs (dt-struct/new-array-of-structs :shape (count switch-shapes) )]
    (doseq [[struct m] (map vector structs switch-shapes)]
      (.put struct :type (type->int (get m :type)))
      (.put struct :width (get m :width))
      (.put struct :height (get m :height)))
    (let [widths
          (dt-struct/array-of-structs->column structs :width)

          heights
          (dt-struct/array-of-structs->column structs :height)

          types
          (dt-struct/array-of-structs->column structs :type)

          constants (emap/emap
                     (fn ^double [^long type]
                       #_(case type
                           1 1.0
                           2 1.0
                           3 0.5
                           4 Math/PI)
                       (type->constant type))
                     :float64
                     types)]
      (bench
       (dfn/sum-fast (dfn/* widths heights constants)))))


  ,)


(defprotocol IArea
  (area [this]))

(defrecord Square [^double width ^double  height]
  IArea
  (area [_]
    (* width height)))
(defrecord Rectangle [^double width ^double height]
  IArea
  (area [_]
    (* width height)))
(defrecord Triangle [^double width ^double height]
  IArea
  (area [_]
    (* 0.5 width height)))
(defrecord Circle [^double width ^double height]
  IArea
  (area [_]
    (* Math/PI width height)))

(def protocol-shapes
  (into []
        (map (fn [shape]
               (let [->Make (case (:type shape)
                              :square ->Square
                              :rectangle ->Rectangle
                              :triangle ->Triangle
                              :circle ->Circle)]
                 (->Make (:width shape)
                         (:height shape)))))
        switch-shapes))

(defn total-area-protocol [shapes]
  (transduce (map area)
             +
             0.0
             shapes))
(comment
  (bench
   (total-area-protocol protocol-shapes))
  ;; Evaluation count : 1613040 in 60 samples of 26884 calls.
  ;;            Execution time mean : 40.192681 µs
  ;;   Execution time std-deviation : 3.186844 µs
  ;;  Execution time lower quantile : 35.750980 µs ( 2.5%)
  ;;  Execution time upper quantile : 43.006773 µs (97.5%)
  ;;                  Overhead used : 1.854505 ns

  ,)

(definterface IAreai
  (^double areai []))

(defrecord SquareI [^double width ^double  height]
  IAreai
  (areai [this]
   (* width height)))
(defrecord RectangleI [^double width ^double height]
  IAreai
  (areai [_]
    (* width height)))
(defrecord TriangleI [^double width ^double height]
  IAreai
  (areai [_]
    (* 0.5 width height)))
(defrecord CircleI [^double width ^double height]
  IAreai
  (areai [_]
    (* Math/PI width height)))

(def interface-shapes
  (into []
        (map (fn [shape]
               (let [->Make (case (:type shape)
                              :square ->SquareI
                              :rectangle ->RectangleI
                              :triangle ->TriangleI
                              :circle ->CircleI)]
                 (->Make (:width shape)
                         (:height shape)))))
        switch-shapes))

(defn total-areai-interface [shapes]
  (transduce (map #(.areai ^IAreai %))
             +
             0.0
             shapes))
(comment
  (bench
   (total-areai-interface interface-shapes))

;;   Evaluation count : 1619460 in 60 samples of 26991 calls.
;;              Execution time mean : 37.135684 µs
;;     Execution time std-deviation : 172.768544 ns
;;    Execution time lower quantile : 36.659479 µs ( 2.5%)
;;    Execution time upper quantile : 37.361014 µs (97.5%)
;;                    Overhead used : 1.854505 ns

;; Found 2 outliers in 60 samples (3.3333 %)
;; 	low-severe	 2 (3.3333 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers

  ,)




(defmulti multi-area :type)
(defmethod multi-area :square [shape]
  (* ^double (:width shape)
     ^double (:height shape)))
(defmethod multi-area :rectangle [shape]
  (* ^double (:width shape)
     ^double (:height shape)))
(defmethod multi-area :triangle [shape]
  (* 0.5
     ^double (:width shape)
     ^double (:height shape)))
(defmethod multi-area :circle [shape]
  (* Math/PI
     ^double (:width shape)
     ^double (:height shape)))

(defn total-area-multi [shapes]
  (transduce (map multi-area)
             +
             0.0
             shapes))

(comment
  (bench
   (total-area-multi switch-shapes))

;; Evaluation count : 1058280 in 60 samples of 17638 calls.
;;              Execution time mean : 56.812461 µs
;;     Execution time std-deviation : 180.900006 ns
;;    Execution time lower quantile : 56.409643 µs ( 2.5%)
;;    Execution time upper quantile : 57.183371 µs (97.5%)
;;                    Overhead used : 1.854505 ns

;; Found 4 outliers in 60 samples (6.6667 %)
;; 	low-severe	 2 (3.3333 %)
;; 	low-mild	 2 (3.3333 %)
;;  Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
 
  ,)


(declare area-macro)
(def area-types (atom {}))
(defmacro define-shape-area [type impl]
  (let [area-impls (swap! area-types assoc type impl)
        code
        `(alter-var-root
          #'area-macro
          (constantly
           (fn [~'shape]
             (case (:type ~'shape)
               ~@(sequence
                  cat
                  area-impls)))))]
    code))

(define-shape-area :square
  (* ^double (:width shape)
     ^double (:height shape)))
(define-shape-area :rectangle
  (* ^double (:width shape)
     ^double (:height shape)))
(define-shape-area :triangle
  (* 0.5
     ^double (:width shape)
     ^double (:height shape)))
(define-shape-area :circle
  (* Math/PI
     ^double (:width shape)
     ^double (:height shape)))

(defn total-area-macro [shapes]
  (transduce (map area-macro)
             +
             0.0
             shapes))

(comment
  (bench
   (total-area-macro switch-shapes))
;;   Evaluation count : 2953380 in 60 samples of 49223 calls.
;;              Execution time mean : 20.835453 µs
;;     Execution time std-deviation : 713.703170 ns
;;    Execution time lower quantile : 20.224252 µs ( 2.5%)
;;    Execution time upper quantile : 22.758592 µs (97.5%)
;;                    Overhead used : 1.854505 ns

;; Found 8 outliers in 60 samples (13.3333 %)
;; 	low-severe	 5 (8.3333 %)
;; 	low-mild	 3 (5.0000 %)
;;  Variance from outliers : 20.6248 % Variance is moderately inflated by outliers

 
  ,)
