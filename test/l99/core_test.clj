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
