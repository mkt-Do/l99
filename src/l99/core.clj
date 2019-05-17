(ns l99.core)

;; P01
(defn my-last [arr]
  (if (empty? arr)
    nil
    (if (empty? (rest arr))
      (first arr)
      (my-last (rest arr)))))

;; P02
(defn my-but-last [arr]
  (if (empty? arr)
    nil
    (if (empty? (rest arr))
      nil
      (if (= (count arr) 2)
        arr
        (my-but-last (rest arr))))))

;; P03
(defn element-at [arr idx]
  (if (<= idx 0)
    nil
    (if (< (count arr) idx)
       nil
       (if (= idx 1)
         (first arr)
         (element-at (rest arr) (- idx 1))))))

;; P04
(defn my-count [arr]
  (if (empty? arr)
    0
    (+ (my-count (rest arr)) 1)))

;; P05
(defn my-reverse [arr]
  (letfn [(rev [a res]
         (if (empty? a)
           res
           (rev (rest a) (conj res (first a)))))]
    (rev arr nil)))

;; P06
(defn palindrone [arr]
  (= (my-reverse arr) arr))

;; P07
(defn my-flatten [arr]
  (letfn [(f [a r]
            (if (empty? a)
              r
              (if (or (seq? (first a)) (vector? (first a)))
                (f (rest a) (concat r (f (first a) '())))
                (f (rest a) (reverse (conj (reverse r) (first a)))))))]
    (f arr '())))

;; P08
(defn compress [arr]
  (letfn [(comp [a r]
            (if (empty? a)
              r
              (if (= (first (reverse r)) (first a))
                (comp (rest a) r)
                (comp (rest a) (reverse (conj (reverse r) (first a)))))))]
    (comp arr '())))

;; P09
(defn pack [arr]
  (letfn [(p [a r]
            (if (empty? a)
              r
              (if (empty? r)
                (p (rest a) (list (list (first a))))
                (if (= (first (first (reverse r))) (first a))
                  (p (rest a) (reverse (concat (list (conj (first (reverse r)) (first a))) (rest (reverse r)))))
                  (p (rest a) (concat r (list (list (first a)))))))))]
    (p arr '())))
