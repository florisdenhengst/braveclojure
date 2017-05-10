; 1
(str 123)
(str (+ 123 45))
(str [1 2 3])
(str #{ 1 1 2})
(str '(1 2 3 3))

; 2
(defn add-100 [number] (+ number 100))
(add-100 10)

; 3
(defn dec-maker [dec-by] #(- % dec-by))
(def dec-9 (dec-maker 9))
(dec-9 10)
((dec-maker 9) 10)

; 4 - (map #{}) returns a list, so the (into #{}) should wrap the map, yo
(defn mapset [fun, iterable] (into #{} (map fun iterable)))
(mapset inc [1 1 2 2])

; 5 - basic idea here is to use the function from the example, but return a set of matching body
; parts instead of a single one. Uses juxt to 'map' multiple functions over a single body-part
(defn matcher-maker [direction]
	#(-> {:name (clojure.string/replace (:name %) #"^left-" (str direction "-"))
		 :size (:size %)}))
(def match-top (matcher-maker "top"))
(def match-right (matcher-maker "right"))
(def match-lower-left (matcher-maker "lower-left"))
(def match-lower-right (matcher-maker "lower-right"))
(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
										 (set [part ((juxt match-top match-right match-lower-left
match-lower-right) part)])))))))

(def asym-alien-parts [{:name "head" :size 3}
                       {:name "left-eye" :size 1}
                       {:name "left-ear" :size 1}
                       {:name "mouth" :size 1}
                       {:name "nose" :size 1}
                       {:name "neck" :size 2}
                       {:name "left-shoulder" :size 3}
                       {:name "left-upper-arm" :size 3}
                       {:name "chest" :size 10}
                       {:name "back" :size 10}
                       {:name "left-forearm" :size 3}
                       {:name "abdomen" :size 6}
                       {:name "left-kidney" :size 1}
                       {:name "left-hand" :size 2}
                       {:name "left-knee" :size 2}
                       {:name "left-thigh" :size 4}
                       {:name "left-lower-leg" :size 3}
                       {:name "left-achilles" :size 1}
                       {:name "left-foot" :size 2}])

(symmetrize-body-parts asym-alien-parts)

; 6 - idea similar to 5, but dynamically make the match-X methods
(defn symmetrize-body-parts-maker
  "Expects a seq of maps that have a :name and :size"
  [N]
  #(loop [remaining-asym-parts %
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
										 (set [part ((apply juxt (into [] (map matcher-maker (range N)))) part)])))))))

((symmetrize-body-parts-maker 10) asym-alien-parts)
