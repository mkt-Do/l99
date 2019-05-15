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
  (if (<= (count arr) idx)
    nil
    (if (= idx 1)
      (first arr)
      (element-at (rest arr) (- idx 1)))))

;; P04
(defn my-count [arr]
  (if (empty? arr)
    0
    (+ (my-count (rest arr)) 1)))

;; P05
(defn my-reverse [arr]
  (defn rev [a res]
         (if (empty? a)
           res
           (rev (rest a) (conj res (first a)))))
  (rev arr nil))

;; P06
(defn palindrone [arr]
  (= (my-reverse arr) arr))
