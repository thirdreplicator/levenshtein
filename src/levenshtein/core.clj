(ns levenshtein.core
  (:use	[clojure.contrib.duck-streams :only (read-lines)])
  (:use	[clojure.contrib.map-utils :only (deep-merge-with)])
  (:use [clojure.test :as t] :reload))


(defn reload []
  (load "levenshtein/core")
  (load "levenshtein/test/core")
  (clojure.test/run-tests 'levenshtein.test.core))

(defn front-substrings [word]
  (let [lastid     (count word)
        inc-lastid (inc lastid)]
   (map #(subs word 0 %) 
         (range inc-lastid))))

(defn back-substrings [word]
  (let [lastid     (count word)
        inc-lastid (inc lastid)]
   (map #(subs word % lastid) 
         (range inc-lastid))))

(defn c-indices 
  ([word]            (c-indices word 0))
  ([word adjustment] 
    (let [length          (count word)
          num-elts        (inc length)
          adjusted-length (+ length adjustment)]
    (partition 3 (interleave (repeat num-elts adjusted-length) 
                           (front-substrings word) 
                           (back-substrings word))))))

(defn du-front-substrings [word]
  (take (count word) (front-substrings word)))

(defn du-back-substrings [word]
  (rest (back-substrings word)))

(defn du-indices
  ([word] (du-indices word 0)) 
  ([word adjustment]
   (let [length          (count word)
         adjusted-length (+ length adjustment)]
       (partition 3 (interleave (repeat length adjusted-length)
                                (du-front-substrings word)
                                (du-back-substrings  word))))))

(defn cdu-indices [word]
  (into (du-indices word) (c-indices word) ))

(defn generate-index
  ([word-list]
   (reduce generate-index {} word-list))
  ([deep-hash word]
   (reduce #(update-in %1 %2 conj word)
          deep-hash 
          (cdu-indices word))))

(defn lev-c-indices [word]
  (c-indices word 1))

(defn lev-u-indices [word]
  (du-indices word))

(defn lev-d-indices [word]
  (if (= 1 (count word))
      '()
      (du-indices word -1)))

(defn lev-indices [word]
  (reduce into '() (list (lev-c-indices word) (lev-u-indices word) (lev-d-indices word))))

(defn friend-list [word-index word]
  (filter #(not (= word %)) 
         (reduce into '() 
                 (map #(get-in word-index %) (lev-indices word)))))

(defn generate-friend-book [word-index word-list]
  (map #(friend-list word-index %) word-list)) 
  
(defn generate-social-network 
  ([word-index word-list friend-book]
    (apply hash-map (interleave word-list friend-book)))
  ([word-index word-list]
    (let [friend-book (generate-friend-book word-index word-list)]
      (generate-social-network word-index word-list friend-book)))
  ([word-list]
   "Returns a map of words to word-friends."
    (let [word-index  (generate-index       word-list)
          friend-book (generate-friend-book word-index word-list)] 
      (generate-social-network word-index word-list))))


;> (fn visit [node]
;>   (lazy-cons node (map visit (get-children node)))) 

;(defn friend-network-hash [word-list]
;  (let [word-index (generate-index word-list)]
;    (map
; End-game
;(def word-file "/home/david/clj/levenshtein/word.list")

;(def words (read-lines word-file))

; Divide 250k words into 4 chunks
;(def word-partition (partition-all 66020 l/words)) 

; Parallel map-reduce each chunk.
;(def word-chunks (pmap #(reduce generate-index {} %) word-parition))

; Then, deep-merge them together.
;(def word-index 
;   (reduce #(deep-merge-with into %1 %2) {} (reduce generate-index {} words))

; Index the whole word list.
;  write friending indexes.
; generate two words friends
;
; Make the whole friend network.
; Traverse the friend network starting with "causes" and count how big the network is.
 
;(doseq [word words]
;  (prn word))


