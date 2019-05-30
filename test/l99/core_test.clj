(ns l99.core-test
  (:use clojure.test
        l99.core))

(deftest my-last-test
  (testing "return last element in vector/sequence"
    (are [r v] (= r (my-last v))
      3 [1 2 3]
      "d" '("a", "b", "c", "d")
      1 [1]
      nil [])))

(deftest my-but-last-test
  (testing "return last and before elements as sequence in vector/sequence"
    (are [r v] (= r (my-but-last v))
      '(3 4) [1 2 3 4]
      '("c" "d") '("a" "b" "c" "d")
      nil [1]
      nil [])))

(deftest element-at-test
  (testing "find element by index in vector/sequence"
    (are [r v1 v2] (= r (element-at v1 v2))
      2 [1 2 3] 2
      "d" '("a" "b" "c" "d") 4
      nil [1 2] 3
      nil [] 0)))

(deftest my-count-test
  (testing "count vector/sequence"
    (are [r v] (= r (my-count v))
      3 [1 2 3]
      4 '("a" "b" "c" "d")
      1 [1]
      0 [])))

(deftest my-reverse-test
  (testing "reverse vector/sequence"
    (are [r v] (= r (my-reverse v))
      [3 2 1] [1 2 3]
      '("d" "c" "b" "a") '("a" "b" "c" "d")
      [1] [1]
      nil [])))

(deftest palindrone-test
  (testing "check palindrone vector/sequence"
    (are [r v] (= r (palindrone v))
      true [1 2 3 2 1]
      true '("a" "b" "b" "a")
      false [1 2 3]
      false '("a" "b" "c" "d")
      true [1]
      false []; because return value is "nil"
      )))

(deftest my-flatten-test
  (testing "flatten vector/sequence"
    (are [r v] (= r (my-flatten v))
      '(1 2 3) [1 [2 3]]
      '(1 2 3) '(1 (2 3))
      '("a" "b" "c") ["a" ["b" "c"]]
      '(1 2 3 4 5) [1 [2 [3 4]] 5]
      '(1) [[1]]
      '() [])))

(deftest compress-test
  (testing "remove duplicate element one-before emement in vector/sequence"
    (are [r v] (= r (compress v))
      '(1 2 3) [1 2 2 3 3 3]
      '(1 2 3 2 1) [1 2 2 3 3 3 2 2 1]
      '("a" "b" "c" "a" "d" "e") '("a" "a" "a" "a" "b" "c" "c" "a" "a" "d" "e" "e" "e" "e")
      '() '())))

(deftest pack-test
  (testing "pack same element before element"
    (are [r v] (= r (pack v))
      '((1) (2 2) (3 3 3)) [1 2 2 3 3 3]
      '((a a a a) (b) (c c) (a a) (d) (e e e e)) '(a a a a b c c a a d e e e e)
      '((1)) [1]
      '() '())))

(deftest encode-test
  (testing "convert element count same element in packed list"
    (are [r v] (= r (encode v))
      '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) '(a a a a b c c a a d e e e e))))

(deftest encode-modified-test
  (testing "convert element count same element in packed list if number of elements over 2"
    (are [r v] (= r (encode-modified v))
      '((4 a) b (2 c) (2 a) d (4 e)) '(a a a a b c c a a d e e e e))))

(deftest decode-test
  (testing "convert flatten list from encode list"
    (are [r v] (= r (decode v))
      '(a a a a b c c a a d e e e e) '((4 a) b (2 c) (2 a) d (4 e)))))

(deftest encode-direct-test
  (testing "encode directly"
    (are [r v] (= r (encode-direct v))
      '((4 a) b (2 c) (2 a) d (4 e)) '(a a a a b c c a a d e e e e))))

(deftest dupli-test
  (testing "duplicate elements of the list"
    (are [r v] (= r (dupli v))
      '(a a b b c c c c d d) '(a b c c d))))

(deftest repli-test
  (testing "replicate elements of the list"
    (are [r v t] (= r (repli v t))
      '(a a a b b b c c c) '(a b c) 3)))

(deftest my-drop-test
  (testing "drop every Nth elements in list"
    (are [r v i] (= r (my-drop v i))
      '(a b d e g h k) '(a b c d e f g h i k) 3)))

(deftest split-test
  (testing "split list by given number"
    (are [r v i] (= r (split v i))
      '((a b c) (d e f g h i k)) '(a b c d e f g h i k) 3)))

(deftest slice-test
  (testing "slice list"
    (are [r v s e] (= r (slice v s e))
      '(c d e f g) '(a b c d e f g h i k) 3 7)))
