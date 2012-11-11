(ns clj-des.bitmanipulation)


;; ------------------------------------------------------------------------------------------------
;; Create "bitlists" as maps of bytes

(defn mk-bitlist
  "Create a bitlist of the bytes."
  [ & bytes]
  (zipmap (range 1 (inc (count bytes))) bytes))

(defn mk-empty-bitlist
  "Create an empty bitlist with num-bits bits."
  [num-bits]
  (apply mk-bitlist (repeat (/ num-bits 8) 0)))


;; ------------------------------------------------------------------------------------------------
;; Read or modify bits from a "bitlist"

(defn- get-block-bit-pos [n]
  [(int (Math/ceil (/ n 8))) (- 7 (mod (dec n) 8))])

(defn get-bit
  "Return the bit value (0 or 1) from the block at position pos."
  [block pos]
  (let [[byte bit] (get-block-bit-pos pos)]
    (if (bit-test (block byte) bit) 1 0)))

(defn get-byte
  "Return the nth byte of the block."
  [block n]
  (block n))

(defn set-bit-to
  "Set the bit at position pos of the block to val (0 or 1)."
  [block pos val]
  (let [[byte bit] (get-block-bit-pos pos)]
    (update-in block [byte] #((if (zero? val) bit-clear bit-set) % bit))))

(defn set-byte
  "Set the nth byte of the block to b."
  [block n b]
  (update-in block [n] (fn [_] b)))

(defn xor
  "Calculate the bitwise xor of bitlists a and b."
  [a b]
  (reduce (fn [x n] (assoc x n (bit-xor (a n) (b n)))) {} (keys a)))
