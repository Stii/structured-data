(ns structured-data)

(defn do-a-thing
  [x]
  (let [add (+ x x)]
    (Math/pow add add)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x1 x2) (- y1 y2))
    ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
    )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (if (and (<= x1 p1 x2) (<= y1 p2 y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[x1 x2] inner]
    (and (contains-point? outer x1) (contains-point? outer x2))))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [authors (get book :authors)]
    (let [new-authors (conj authors new-author)]
      (assoc book :authors new-authors))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [coll] (get coll 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (let [ascending (fn [s] (apply <= s))
        descending (fn [s] (apply >= s))]
    (or (ascending a-seq) (descending a-seq))))

(defn stars [n]
  (let [make_stars (fn [how_much] (repeat how_much "*"))]
    (apply str (make_stars n))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [years (if (contains? author :birth-year) (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str (:name author) years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str
    (:title book)
    ", written by "
    (authors->string (:authors book))))

(defn books->string [books]
  (let [bookcount (count books)
        bookstr (fn [books] (apply str (interpose ". "(map book->string books))))]
    (cond
      (= bookcount 1) (str bookcount " book. " (bookstr books) ".")
      (> bookcount 1) (str bookcount " books. " (bookstr books) ".")
      :else "No books.")))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [authorname (filter (fn [author] (= (:name author) name)) authors)]
    (if (empty? authorname)
      nil
      (first authorname))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (let [livingauthor (living-authors (:authors book))]
    (not (empty? livingauthor))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
