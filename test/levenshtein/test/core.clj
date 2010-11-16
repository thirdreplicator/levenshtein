(ns levenshtein.test.core
  (:use [levenshtein.core] :reload)
  (:use [clojure.test] :reload))

(defn reload-tests []
  (load "/levenshtein/test/core"))

(deftest test-front-substrings
  (is (= ["" "f" "fa" "fat" "fate"] (front-substrings "fate")))
  (is (= ["" "m" "ma" "man" ] (front-substrings "man"))))

(deftest test-back-substrings
  (is (= ["fate" "ate" "te" "e" ""] (back-substrings "fate")))
  (is (= ["man" "an" "n" ""] (back-substrings "man"))))

(deftest test-du-front-substrings
  (is (= ["" "f" "fa" "fat"] (du-front-substrings "fate")))
  (is (= ["" "m" "ma"]       (du-front-substrings "man"))))

(deftest test-du-back-substrings
  (is (= ["ate" "te" "e" ""] (du-back-substrings "fate")))
  (is (= ["an" "n" ""]       (du-back-substrings "man"))))


; "c" stands for "create".
(deftest test-c-indices
  (is (= [[4 "" "fate"] [4 "f" "ate"] [4 "fa" "te"] [4 "fat" "e"] [4 "fate" ""]] 
         (c-indices "fate")))
  (is (= [[5 "" "fate"] [5 "f" "ate"] [5 "fa" "te"] [5 "fat" "e"] [5 "fate" ""]] 
         (c-indices "fate" 1)))
  (is (= [[3 "" "man"] [3 "m" "an"] [3 "ma" "n"] [3 "man" ""]]
         (c-indices "man"))))

(deftest test-du-indices
  (is (= [[4 "" "ate"] [4 "f" "te"] [4 "fa" "e"] [4 "fat" ""]]
         (du-indices "fate")))
  (is (= [[3 "" "ate"] [3 "f" "te"] [3 "fa" "e"] [3 "fat" ""]]
         (du-indices "fate" -1)))
  (is (= [[3 "" "an"] [3 "m" "n"] [3 "ma" ""]] 
         (du-indices "man")))
  (is (= [[2 "" "an"] [2 "m" "n"] [2 "ma" ""]] 
         (du-indices "man" -1))))

; The combination between du-indices and cdu-indices.  Order doesn't matter.
(deftest test-cdu-indices
  (is (= (set [[3 "" "man"] [3 "m" "an"] [3 "ma" "n"] [3 "man" ""]
               [3 "" "an"] [3 "m" "n"] [3 "ma" ""]]))))


(deftest test-generate-index-for-word
  (is (= {3 {""     {"man" '("man")
                     "an"  '("man")}
             "m"    {"an"  '("man")
                     "n"   '("man")} 
             "ma"   {"n"   '("man")
                     ""    '("man")}
             "man"  {""    '("man")}}}

         (generate-index {} "man"))))

; Below are the indices that we are searching for to make friends.
; Lev-c indices represent indices of words that are obtained by an addition of 1
;   character.

(deftest test-lev-c-indices
  (is (= (set [[4 "" "man"] [4 "m" "an"] [4 "ma" "n"] [4 "man" ""]])
         (set (lev-c-indices "man")))))

; Lev-u indices represent indices of words that are obtained by the replacement of 1
;   character.

(deftest test-lev-u-indices
  (is (= (set [[3 "" "an"] [3 "m" "n"] [3 "ma" ""]]) 
         (set (lev-u-indices "man")))))

; Lev-d indices represent indices of words that are obtained by deleting 1 character.
(deftest test-lev-d-indices
  (is (= (set [[2 "" "an"] [2 "m" "n"] [2 "ma" ""]]) 
         (set (lev-d-indices "man"))))
  ; The lev-u index of a 1 character word is just the empty set.
  (is (= (set [])
         (set (lev-d-indices "a")))))

; lev-indices represent all indices for which a word w1 has a levenshtein distance of 1
;   to a word w2.  If a word w2 is indexed by one of w1's lev-indices, then there exists
;   a 1-character transformation from w1 to w2 and vice-versa.  The relationship is 
;   symmetrical.  w1 and w2 are "friends."

(deftest test-lev-indices
  (is (= (set [[2 "" "an"] [2 "m" "n"] [2 "ma" ""]
               [3 "" "an"] [3 "m" "n"] [3 "ma" ""] 
               [4 "" "man"] [4 "m" "an"] [4 "ma" "n"] [4 "man" ""]])
         (set (lev-indices "man")))))

; Cat and hat should be friends.
(deftest test-friend-list-degree-1
  (let [word-index (generate-index '("at" "cat" "hat" "fat" "fate" "dog"))]
    (is (= (set '("hat" "fat" "at"))
           (set (friend-list word-index "cat"))))
    (is (= (set '("cat" "hat" "fate" "at"))
           (set (friend-list word-index "fat")))))) 

(deftest test-list-friends-a-complete-example
  (let [word-index (generate-index '("atp" "at" "cat" "dog" "hat" "fat" "fate" "fates" 
                                     "hate" "hale" "can" "man" "tan" "an" "stan" ))]
    (is (= (set '("hat" "fat" "can" "at"))
           (set (friend-list word-index "cat"))))
    (is (= (set '("fate"))
           (set (friend-list word-index "fates"))))
    (is (= (set '())
           (set (friend-list word-index "dog"))))
    (is (= (set '("stan" "man" "can" "an"))
           (set (friend-list word-index "tan")))))) 

; Make a hash-map that indexes each word with a list of its friends.
(deftest test-generate-social-network
  (let [word-list   '("man" "can" "mat")]
    (is (= {"mat" '("man"), "can" '("man"), "man" '("can" "mat")}
           (generate-social-network word-list)))))

; Create 2 disjoint social networds and then count the members of each.
; The network with "dog" in it is disjoint from the one with "man" in it.

(deftest test-size-of-network
  (let [network (generate-social-network '("man" "can" "cans" "tan" "ton" "stan" "dog"
                                           "dot" "cot" "hot"))]
    (is (= 4
           (size-of-network network "dog")))
    (is (= 6
           (size-of-network network "man")))))

