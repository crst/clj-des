(ns clj-des.des
  (:use [clj-des.bitmanipulation])
  (:use [clj-des.des-data]))


(defn- permute [b p]
  (reduce
   (fn [blk [t f]] (set-bit-to blk (inc t) (get-bit b f)))
   (mk-empty-bitlist (count p))
   (map-indexed vector p)))



;; ------------------------------------------------------------------------------------------------
;; Generate the keys.

(defn- shift-key [k]
  (reduce
   (fn [value [pos to]] (set-bit-to value pos to))
   (apply mk-bitlist (map #(bit-and 0xff (bit-shift-left (k %) 1)) (range 1 8)))
   (map vector [8 16 24 28 32 40 48 56] (map #(get-bit k %) [9 17 25 1 33 41 49 29]))))


(defn key-gen
  "Takes a 64 bit key bitlist (mk-bitlist) as input and returns a map
  with the 16 round keys."
  [k]
  (first (reduce
          (fn [[res cur] [shifts num]]
            (let [shifted (reduce (fn [x _] (shift-key x)) cur (range shifts))]
              [(assoc res num (permute shifted PC2)) shifted]))
          (list {} (permute k PC1))
          (map vector LSHIFTS (range 1 17)))))



;; ------------------------------------------------------------------------------------------------
;; Implementation of the round function f

(defn- from-bits [num bits]
  (reduce (fn [n bit] (bit-or (bit-shift-left n 1) (get-bit num bit))) 0 bits))

(defn- get-sbox-value [inp num]
  (let [f (partial + (* 6 num))
        col (from-bits inp (map f [1 6]))
        row (from-bits inp (map f [2 3 4 5]))
        box (get BOXES num)]
    (get (get box col) row)))

(defn- s [inp]
  (let [s-out (map #(get-sbox-value inp %) (range 8))]
    (apply mk-bitlist
           (map (fn [[h l]] (bit-or (bit-shift-left h 4) l)) (partition 2 s-out)))))

(defn- f [r k]
  (permute (s (xor (permute r E) k)) P))



;; ------------------------------------------------------------------------------------------------
;; Implementation of the Feistel network

(defn- split-block [blk]
  (letfn [(f [x y] (apply mk-bitlist (map #(get-byte blk %) (range x y))))]
    [(f 1 5) (f 5 9)]))

(defn- join-blocks [l r]
  (apply mk-bitlist
         (mapcat (fn [c](map (fn [i] (get-byte c i)) (range 1 5))) [l r])))

(defn- run-network [blk ks key-fn]
  (let [[l r] (split-block (permute blk IP))
        [a b] (reduce (fn [[x y] n] [y (xor x (f y (ks (key-fn n))))]) [l r] (range 1 17))]
    (permute (join-blocks b a) IIP)))



;; ------------------------------------------------------------------------------------------------
;; Encryption/Decryption

(defn des-encrypt-block
  "Encrypts one 64 bit block (created with mk-bitlist) with the keymap
  ks (created with key-gen)."
  [blk ks]
  (run-network blk ks identity))

(defn des-decrypt-block
  "Decrypts one 64 bit block (created with mk-bitlist) with the keymap
  ks (created with key-gen)"
  [blk ks]
  (run-network blk ks (partial - 17)))
