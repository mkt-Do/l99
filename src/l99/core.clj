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

;; P10
(defn encode [arr]
  (map (fn [s] (list (count s) (first s))) (pack arr)))

;; P11
(defn encode-modified [arr]
  (map (fn [s]
         (if (= (first s) 1)
           (first (rest s))
           s)) (encode arr)))

;; P12
(defn decode [arr]
  (letfn [(f [a]
            (if (seq? a)
              (if (= (first a) 2)
                (concat (f (first (rest a))) (rest a))
                (concat (f (list (- (first a) 1) (first (rest a)))) (rest a)))
              (list a)))]
    (flatten (map f arr))))

;; P13
(defn encode-direct [arr]
  (letfn [(f [a r]
            (if (empty? a)
              (reverse r)
              (if (seq? (first r))
                (if (= (first a) (first (reverse (first r))))
                  (f (rest a) (conj (rest r) (list (+ (first (first r)) 1) (first a))))
                  (f (rest a) (conj r (first a))))
                (if (= (first a) (first r))
                  (f (rest a) (conj (rest r) (list 2 (first a))))
                  (f (rest a) (conj r (first a)))))))]
    (f arr '())))

;; P14
(defn dupli [arr]
  (flatten (map (fn [v] (list v v)) arr)))

;; P15
(defn repli [arr times]
  (flatten (map (fn [v] (take times (repeat v))) arr)))

;; P16
(defn my-drop [arr idx]
  (letfn [(f [a i]
            (if (or (empty? a) (< i 1))
              a
              (if (= i 1)
                (f (rest a) idx)
                (concat (list (first a)) (f (rest a) (- i 1))))))]
    (f arr idx)))

;; P17
(defn split [arr idx]
  (letfn [(f [a i r]
            (if (or (empty? a) (< i 0))
              r
              (if (= i 0)
                (list r a)
                (f (rest a) (- i 1) (reverse (conj (reverse r) (first a)))))))]
    (f arr idx '())))

;; P18
(defn slice [arr st ed]
  (if (or (< st 1) (< ed 1) (< (count arr) st) (< (count arr) ed) (> st ed))
    arr
    (letfn [(f [a s e r]
              (if (= e 0)
                (conj r (first a))
                (if (= s 0)
                  (f (rest a) s (- e 1) (conj r (first a)))
                  (f (rest a) (- s 1) (- e 1) r))))]
      (reverse (f arr (- st 1) (- ed 1) '())))))

;; P19
(defn rotate [arr t]
  (if (= t 0)
    arr
    (if (< t 0)
      (rotate (conj (reverse (rest (reverse arr))) (first (reverse arr))) (+ t 1))
      (rotate (reverse (conj (reverse (rest arr)) (first arr))) (- t 1)))))

;; P20
(defn remove-at [arr idx]
  (if (< idx 1)
    arr
    (if (= idx 1)
      (rest arr)
      (-> (remove-at (rest arr) (- idx 1))
          (conj (first arr))))))

;; P21
(defn insert-at [v arr idx]
  (if (< idx 1)
    arr
    (if (= idx 1)
      (conj arr v)
      (-> (insert-at v (rest arr) (- idx 1))
          (conj (first arr))))))

;; P22
(defn my-range [st ed]
  (if (> st ed)
    '()
    (if (= st ed)
      (list st)
      (-> (my-range (+ st 1) ed)
          (conj st)))))

;; P23
(defn rnd-select [arr n]
  (if (<= n 0)
    '()
    (let [r (+ (rand-int (count arr)) 1)]
      (conj (rnd-select (remove-at arr r) (- n 1)) (element-at arr r)))))

;; P24
(defn lotto-select [n mx]
  (if (<= mx 0)
    '()
    (let [ls (range 1 mx)]
      (letfn [(f [arr l]
                (if (<= l 0)
                  '()
                  (let [r (rand-int (+ (count arr) 1))]
                    (conj (f (remove-at arr r) (- l 1)) (element-at arr r)))))]
        (f ls n)))))

;; P25
(defn rnd-permu [arr]
  (rnd-select arr (count arr)))
