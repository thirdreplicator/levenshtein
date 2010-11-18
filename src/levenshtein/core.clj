(ns levenshtein.core
  (:use	[clojure.contrib.duck-streams :only (read-lines)])
  (:use	[clojure.contrib.map-utils :only (deep-merge-with)])
  (:use [clojure.contrib.graph :only (lazy-walk)])
  (:use [clojure.test :as t] :reload))

; (use 'levenshtein.core :reload)

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

(defn size-of-network [g n]
  (count (lazy-walk g n)))

; End-game
(def word-file "/home/david/clj/levenshtein/word.list")

; Change this to a smaller number if you want to run on a subset of the dictionary words.
(def n-words 264061)
(def words (take n-words (read-lines word-file)))

; Divide 264,000 words into 4 chunks
(def word-partition 
  (let [partition-size (Math/ceil (/ n-words 4))]
    (partition-all partition-size words)))

(prn "***pmap generate-index partitions***")

(def word-index-atom (atom {}))
; (def word-index-ref (ref {}))
(def word-index {})  
(def social-graph-atom (atom {}))

(defn generate-index-atom
  ([word-list]
    (doseq [word word-list]
      (doseq [triplet (cdu-indices word)]
        ; 50% faster than ref version
        (swap! word-index-atom update-in triplet conj word))))) 

;        (dosync (commute word-index-ref update-in triplet conj word)))))) ; ref version

(defn map-words-to-friends
  [word-list]
    (doseq [word word-list]
      (let [index            [:neighbors word]
            list-of-friends  (friend-list @word-index-atom word)]
        (swap! social-graph-atom update-in index into list-of-friends))))

; 1. Create word-index-atom.
(defn step1 []
  (pmap #(generate-index-atom %) word-partition))

; 2. Create friend book (a list of friend lists).
(defn step2 []
  (pmap #(map-words-to-friends %) word-partition))

; 3. Count the number of members in the "causes" social network.... call "print-result"  after the CPU calms down.

(defn print-result []
  (prn (str "The size of the \"causes\" social network is: " 
            (count (clojure.contrib.graph/lazy-walk @social-graph-atom "causes")))))
 
