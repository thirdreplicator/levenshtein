(ns levenshtein.core
  (:use	[clojure.contrib.duck-streams :only (read-lines)])
  (:use	[clojure.contrib.map-utils :only (deep-merge-with)])
  (:use [clojure.contrib.graph :only (lazy-walk)])
  (:use [clojure.test :as t] :reload))


(defn reload []
"A convenience function for reloading and testing the code."
  (load "levenshtein/core")
  (load "levenshtein/test/core")
  (clojure.test/run-tests 'levenshtein.test.core))

(defn front-substrings [word]
"Generate a list of substring pairs, representing the two pieces that the word would be split into had it be split at each character position, start iterating from the front of the word."
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
"Indexes words based on size and modification fragments, for single-character create, delete and update operations."
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

(defn generate-friend-book
"A utility function that just generates a list of friends based on a list of words."
  [word-index word-list]
  (map #(friend-list word-index %) word-list)) 
  
(defn generate-social-network 
"Returns a map of words to word-friends."
  ([word-index word-list friend-book]
    (apply hash-map (interleave word-list friend-book)))
  ([word-index word-list]
    (let [friend-book (generate-friend-book word-index word-list)]
      (generate-social-network word-index word-list friend-book)))
  ([word-list]
    (let [word-index  (generate-index       word-list)
          friend-book (generate-friend-book word-index word-list)] 
      (generate-social-network word-index word-list))))

(defn reformat-graph
"Reformat the graph for clojure.contrib.graph to be able to operate on it."
  [social-network]
  {:neighbors social-network})

(defn subnetwork [g n]
  (let [formatted-graph (reformat-graph g)]
    (lazy-walk formatted-graph n)))

(defn size-of-network [g n]
  (count (subnetwork g n)))

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
 
(defn print-words [words]
"Print a list of words to STDOUT."
  (doseq [word words]
    (prn word)))


