(ns discern.core
  (:require [clojure.java.io :as io]))

(comment (defn longest [xs ys]
           (if (> (count xs) (count ys))
             xs
             ys))

         (defn lcs [seqx seqy]
           (when-let [[x & xs] (seq seqx)]
             (when-let [[y & ys] (seq seqy)]
               (if (= x y)
                 (cons x (lcs xs ys))
                 (longest (lcs seqx ys) (lcs xs seqy)))))))

(defn nth-2d [coll m i j]
  (nth coll (+ (* i m) j)))

(defmacro assoc-2d! [coll m i j val]
  `(assoc! ~coll (+ (* ~i ~m) ~j) ~val))

(defn print-table [lcs-table m n]
  (loop [i 0]
    (when (not= i m)
      (do (loop [j 0]
            (when (not= j n)
              (do (print (str (nth-2d lcs-table n i j) " "))
                  (recur (inc j)))))
          (println)
          (recur (inc i))))))

(defn lcs-count [seqx seqy]
  (let [m (inc (count seqx))
        n (inc (count seqy))
        c (transient (vec (take (* m n) (repeat 0))))]
    (loop [i 1 xs seqx]
      (if (= i m)
        (persistent! c)
        (do (loop [j 1 ys seqy]
              (when (not= j n)
                (do (if (= (first xs) (first ys))
                      (assoc-2d! c n i j (inc (nth-2d c n (dec i) (dec j))))
                      (if (>= (nth-2d c n (dec i) j)
                              (nth-2d c n i (dec j)))
                        (assoc-2d! c n i j (nth-2d c n (dec i) j))
                        (assoc-2d! c n i j (nth-2d c n i (dec j)))))
                    (recur (inc j) (rest ys)))))
            (recur (inc i) (rest xs)))))))

(defn lcs-seq [lcs-table seqx seqy]
  (let [m (count seqx)
        n (count seqy)
        c (inc n)]
    (loop [i m j n res '()]
      (if (or (zero? i) (zero? j))
        res
        (if (= (nth-2d lcs-table c (dec i) j)
               (nth-2d lcs-table c i (dec j))
               (nth-2d lcs-table c (dec i) (dec j)))
          (recur (dec i) (dec j) (conj res (nth seqx (dec i))))
          (if (= (nth-2d lcs-table c i j)
                 (nth-2d lcs-table c (dec i) j))
            (recur (dec i) j res)
            (recur i (dec j) res)))))))

(defn lcs [seqx seqy]
  (lcs-seq (lcs-count seqx seqy) seqx seqy))

(defn print-diff [table seqx seqy]
  (let [m (count seqx)
        n (count seqy)
        c (inc n)]
    (loop [i m j n res '()]
      (if (and (> i 0)
               (> j 0)
               (= (nth seqx (dec i))
                  (nth seqy (dec j))))
        (recur (dec i) (dec j) (conj res (str "  " (nth seqx (dec i)) "\n")))
        (if (and (> j 0)
                 (or (zero? i)
                     (>= (nth-2d table c i (dec j))
                         (nth-2d table c (dec i) j))))
          (recur i (dec j) (conj res (str "+ " (nth seqy (dec j)) "\n")))
          (if (and (> i 0)
                   (or (zero? j)
                       (< (nth-2d table c i (dec j))
                          (nth-2d table c (dec i) j))))
            (recur (dec i) j (conj res (str "- " (nth seqx (dec i)) "\n")))
            (doseq [row res]
              (print row))))))))

(defn diff-files [old new]
  (with-open [ofile (io/reader old)
              nfile (io/reader new)]
    (let [seqo (line-seq ofile)
          seqn (line-seq nfile)]
      (print-diff (lcs-count seqo seqn) seqo seqn))))
