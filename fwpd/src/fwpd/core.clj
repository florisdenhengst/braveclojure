(ns fwpd.core)
(def filename "suspects.csv")

(slurp filename)

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
 ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
    (clojure.string/split string #"\n")))

(defn mapify
  "Returns a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
           {}
           (map vector vamp-keys unmapped-row)))
    rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(def all (mapify (parse (slurp filename))))
(def filtered (glitter-filter 3 (mapify (parse (slurp filename)))))

; Chapter 4 ex. 1

(defn to-name-list
  "Solution to Ch4 ex. 1"
  [glitter-maps]
  (map :name glitter-maps))

(to-name-list filtered)

; Chapter 4 ex .2

(defn append
  "Solution to Ch4 ex. 2"
  [to-append list]
  (reverse (conj (reverse list) to-append)))

(append {:name "Count Dracula" :glitter-index "101"} all)

; Chapter 4 ex. 3

(defn check-name
  [name]
  (not (clojure.string/blank? name)))

(defn check-glitter-index
  [glitter-index]
  (integer? (Integer. glitter-index)))

(def validators {:name check-name
                 :glitter-index check-glitter-index})

(defn validate
  "Write a function, validate, which will check that :name and :glitter-index are present when you append.
  The validate function should accept two arguments: a map of keywords to validating functions, similar to
  conversions, and the record to be validated."
  [validation-map to-validate]
  (every? identity (map
                     (fn [key]
                       ((validation-map key) (to-validate key)))
                     (keys validation-map))))

(defn append2
  "Appends if item checks validations"
  [to-append list]
  (if (validate validators to-append) (append to-append list)))

(append2 {:name "Count Dracula" :glitter-index "101"} all)
(append2 {:name "" :glitter-index "4"} all)

; Chapter 4 ex. 4:
; Write a function that will take your list of maps and convert it back to a CSV string.
; You’ll need to use the clojure.string/join function.

(defn to-csv-string-specific
  "This solution outputs a specific CSV for our use case. See below for a more generic approach"
  [records]
  (clojure.string/join "\n"
    (map (fn [record] (clojure.string/join "," (conj [(record :name)]  (record :glitter-index))))
      records)))

(defn to-csv-string
  "This solution outputs a generic CSV from a list of dicts."
  [records]
  (clojure.string/join "\n"
    (map (fn [record]
           (clojure.string/join "," (into [] (map (fn[k]
                                                    (record k))
                                               (keys record)))))
                                               
      records)))

(to-csv-string-specific all)
(to-csv-string all)
(to-csv-string '({:x "y", :z "z"} {:x "1", :z "2"}))
                                

