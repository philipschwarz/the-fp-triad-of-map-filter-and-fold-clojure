(ns fp-triad-map-filter-fold-clojure.core)

(def x
  (cons
    (cons 1
          (cons 2 '()))
    (cons 3
          (cons 4 '()))))

(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (- n 1))))

(def squares (list 1 4 9 16 25))

(defn length [items]
  (if (empty? items)
    0
    (+ 1 (length (rest items)))))

(def odds (list 1 3 5 7))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(defn map [proc items]
  (if (empty? items)
    '()
    (cons (proc (first items))
          (map proc (rest items)))))

(defn scale-list [items factor]
  (map #(* % factor) items))

(defn count-leaves [x]
  (cond (not (seq? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

(defn scale-tree [tree factor]
  (cond (not (seq? tree)) (* tree factor)
        (empty? tree) '()
        :else (cons (scale-tree (first tree) factor)
                    (scale-tree (rest tree) factor))))

(defn filter [predicate sequence]
  (cond (empty? sequence) '()
        (predicate (first sequence))
        (cons (first sequence)
              (filter predicate (rest sequence)))
        :else (filter predicate (rest sequence))))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence) (accumulate op initial (rest sequence)))))

(defn enumerate-tree [tree]
  (cond (not (seq? tree)) (list tree)
        (empty? tree) '()
        :else (append (enumerate-tree (first tree)) (enumerate-tree (rest tree)))))

(defn square [n] (* n n))

(defn sum-odd-squares [tree]
  (accumulate +
              0
              (map square
                   (filter odd? (enumerate-tree tree)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn even-fibs [n]
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (range 0 (inc n))))))

(defn list-fib-squares [n]
  (accumulate cons
              '()
              (map square
                   (map fib
                        (range 0 (inc n))))))

(defn product-of-squares-of-odd-elements [sequence]
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(defn -main [args]
  (list-fib-squares 10))