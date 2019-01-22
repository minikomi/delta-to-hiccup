(ns delta-to-hiccup.core
  (:require [clojure.string :as str]))

(def list-test
  ( clojure.walk/keywordize-keys
   [
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "first lonely paragrapth"
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     }
    {
     "insert" "a"
     },
    {
     "attributes" {
                   "list" "ordered"
                   },
     "insert" "\n"
     },
    {
     "insert" "b"
     },
    {
     "attributes" {
                   "indent" 1,
                   "list" "ordered"
                   },
     "insert" "\n"
     },
    {
     "insert" "c"
     },
    {
     "attributes" {
                   "indent" 1,
                   "align" "right",
                   "direction" "rtl",
                   "list" "ordered"
                   },
     "insert" "\n"
     },
    {
     "insert" "d"
     },
    {
     "attributes" {
                   "indent" 1,
                   "list" "ordered"
                   },
     "insert" "\n"
     },
    {
     "insert" "e"
     },
    {
     "attributes" {
                   "list" "ordered"
                   "indent" 2
                   },
     "insert" "\n"
     }


    {
     "insert" "Hello how are you?"
     },
    {
     "attributes" {
                   "align" "center"
                   },
     "insert" "\n"
     },
    {
     "insert" "I'm feeling pretty good about myself."
     },
    {
     "attributes" {
                   "align" "center"
                   },
     "insert" "\n\n"
     },
    {
     "insert" "AAA bbb ccc\nddd eee fff\nhjhh ii jjj\nkkk lll mmm \nnnnn "
     },
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "ooo ppp"
     },
    {
     "insert" "\n"
     },
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "qqq rrr sss"
     },
    {
     "insert" "\n"
     },
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "ttt uuu vvv"
     },
    {
     "insert" "\n\n\n"
     },
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "AAA bbb ccc"
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     },
    {
     "insert" "ddd eee fff"
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     },
    {
     "insert" "hjhh ii jjj"
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     },
    {
     "insert" "kkk lll mmm "
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     },
    {
     "insert" "nnnn "
     },
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "ooo ppp"
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     },
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "qqq rrr sss"
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     },
    {
     "attributes" {
                   "bold" true
                   },
     "insert" "ttt uuu vvv"
     },
    {
     "attributes" {
                   "align" "right"
                   },
     "insert" "\n"
     }


    ]
   ))

(defn delta-has-internal-linebreak? [{:keys [insert]}]
  (re-find #"[^\n]\n|\n[^\n]" insert))

(defn delta-is-newline-only? [{:keys [insert]}]
  (= #{\newline} (set insert)))

(defn break-up-multiline-delta
  "If a delta has newlines within, split the delta into
  text-only parts and newline parts."
  [{:keys [insert attributes]}]
  (->> insert
       (partition-by #{\newline})
       (map #(apply str %))
       (mapv
        (fn [part] {:insert part
                    :attributes attributes}))))

(defn line-grouper [[acc op-group] delta]
  "groups all deltas up until a newline, for processing
   that block's block-attributes"
  (if (delta-is-newline-only? delta)
    [(conj acc (assoc op-group
                      :attributes (:attributes delta)
                      :newlines (count (:insert delta))))
     {:inserts []}]
    [acc (update op-group :inserts (fnil conj []) delta)]))

(def default-block-elements
  [{:name "Ordered List"
    :pred (fn [{:keys [attributes] :as op}]
            (= "ordered" (:list attributes)))
    :type :nested-block
    :outer (fn ordered-list-outer-fn [attributes children]
             (into [:ol] children))
    :inner-tag :li}
   {:name "Bullet list"
    :pred (fn [{:keys [attributes] :as op}]
            (= "bullet" (:list attributes)))
    :type :nested-block
    :outer (fn bullet-list-outer-fn [attributes children]
             (into [:ul] children))
    :inner-tag :li}
   {:name "Block Quote"
    :pred (fn [{:keys [attributes] :as op}]
            (:blockquote attributes))
    :type :block
    :outer (fn block-quote-outer-fn [attributes children]
             [:blockquote])}
   {:name "Code Block"
    :pred (fn [{:keys [attributes] :as op}]
            (:code-block attributes))
    :type :block
    :outer (fn code-block-outer-fn [attributes children]
             (into [:pre] children))}
   {:name "Header"
    :pred (fn [{:keys [attributes] :as op}]
            (#{1 2 3 4 5 6 7} (:header attributes)))
    :type :block
    :outer (fn header-outer-fn [attributes children]
             (let [header-tag (keyword (str "h" (:header attributes)))]
               (into [header-tag] children)))}
   {:name "Default Paragrapgh"
    :pred (fn [children] true)
    :type :block
    :outer (fn [attributes children]
             (into [:p] children))}])

(def block-elements (atom default-block-elements))

(defn determine-block-element [{:keys [inserts attributes newlines] :as op}]
  (some #(when ((:pred %) op)
           (assoc %
                  :inserts inserts
                  :newlines newlines
                  :attributes attributes))
        @block-elements))

(defn normalize-ops [deltas]
  (->> deltas
       (mapcat break-up-multiline-delta)
       (reduce line-grouper [[] nil])
       first
       (mapv determine-block-element)))

(defn render-block [{:keys [outer attributes children inner-tag]}]
  (outer attributes children))

(defn render-inner [{:keys [inner-tag inserts newlines attributes]}]
  (cond-> (vector (or inner-tag :span))
    attributes (conj attributes)
    (not-empty inserts) (conj inserts)
    newlines (into (take newlines (repeat [:br])))))

(defn add-op-to-current [op stack]
  (let [current (peek stack)]
    (conj (pop stack)
          (update current
                  :children
                  (fnil conj [])
                  (render-inner op)))))

(defn unwind-stack [acc stack]
  (if (empty? stack) acc
    (let [rev (reverse stack)
          collapsed (reduce
                     (fn [prev op]
                       (render-block
                        (update op
                                :children
                                conj
                                (render-inner (assoc op :inserts prev)))))
                     (render-block (first stack))
                     (rest rev))]
      (conj acc collapsed))))

(defn add-to-stack [stack op]
  (conj stack
   (-> op
       (assoc :children [(render-inner op)])
       (dissoc :inserts :attributes :newlines))))

(defn to-hiccup [deltas]
  (loop [ops (normalize-ops deltas)
         stack []
         acc []]
    (if (empty? ops) (unwind-stack acc stack)
        (let [op (first ops)]
          (cond
            (and (not-empty stack)
                 (= :nested-block (:type (peek stack)))
                 (= :nested-block (:type op)))
            (let [depth (inc (get-in op [:attributes :indent] 0))
                  current-depth (count stack)]
              (cond
                ;; deeper
                (< current-depth depth)
                (recur ops
                       (add-to-stack stack op)
                       acc)
                ;; shallower
                (> current-depth depth)
                (let [st (pop (pop stack))
                      o1 (peek stack)
                      o2 (peek (pop stack))
                      new-stack (conj st
                                      (update o2 :children conj
                                              [(:inner-tag op)
                                               (render-block o1)]))]
                  (recur ops new-stack acc))
                ;; same depth, same type
                (= (:name op) (:name (peek stack)))
                (recur (rest ops)
                       (add-op-to-current op stack)
                       acc)
                ;; same depth but doesn't match - start new
                :else
                (do
                  (recur (rest ops)
                         (add-to-stack [] op)
                         (conj acc stack)))))
            (= (:name (first stack)) (:name op))
            (recur (rest ops)
                   (add-op-to-current op stack)
                   acc)
            :else
            (recur (rest ops)
                   (add-to-stack [] op)
                   (unwind-stack acc stack)))))))

(comment
  (to-hiccup list-test)
    )
