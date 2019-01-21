(ns delta-to-hiccup.core
  (:require [clojure.string :as str]))

(def list-test
  ( clojure.walk/keywordize-keys
   [
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
             [:ol children])
    :inner-tag :li}
   {:name "Bullet list"
    :pred (fn [{:keys [attributes] :as op}]
            (= "bullet" (:list attributes)))
    :type :nested-block
    :outer (fn bullet-list-outer-fn [attributes children]
             [:ul children])
    :inner-tag :li}
   {:name "Block Quote"
    :pred (fn [{:keys [attributes] :as op}]
            (:blockquote attributes))
    :type :block
    :outer (fn block-quote-outer-fn [attributes children]
             [:ul children])}
   {:name "Code Block"
    :pred (fn [{:keys [attributes] :as op}]
            (:code-block attributes))
    :type :block
    :outer (fn code-block-outer-fn [attributes children]
             [:pre children])}
   {:name "Header"
    :pred (fn [{:keys [attributes] :as op}]
            (#{1 2 3 4 5 6 7} (:header attributes)))
    :type :block
    :outer (fn header-outer-fn [attributes children]
             (let [header-tag (keyword (str "h" (:header attributes)))]
               [header-tag children]))}
   {:name "Default Paragrapgh"
    :pred (fn [children] true)
    :type :block
    :outer (fn [attributes children]
             [:p children])}])

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

(defn render-inline [op]

 ["INLINE" (:inserts op)])

(defn render-block [op]
  ((:outer op) (:attributes op)
   (mapv #(vector (or (:inner-tag op) :span) (:insert %))
         (:children op))))

(defn add-op-to-current [op stack]
  (let [current (peek stack)]
    (conj (pop stack)
          (update current
                  :children
                  (fnil conj [])
                  (render-inline op)))))

(defn collapse [stack]
  (println "collapse" (prn-str stack))
  (let [rev (reverse stack)]
    (reduce #(conj % (render-block %2))
            (render-block (first rev))
            (rest rev))))

(defn unwind-stack [acc stack]
  (println "unwind" stack)
  (when (not-empty stack) (println (collapse stack)))
  (if (not-empty stack)
    (conj acc (collapse stack))
    acc))

(defn new-stack [op]
  (-> op
      (assoc :children
             [(render-inline op)])
      (dissoc :inserts :attributes :newlines)
      vector))

(comment
(collapse [(first (normalize-ops list-test))])
  (let [n (normalize-ops list-test)]
    (doall (loop [ops n
            stack []
            acc []]
       (if (empty? ops) (unwind-stack acc stack)
           (let [op (first ops)]
             ( println "-----------------OP--------------")
             (println stack)
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
                          (conj stack op)
                          [])
                   ;; shallower
                   (> current-depth depth)
                   (recur ops
                          (let [st (pop (pop stack))
                                o1 (peek stack)
                                o2 (peek (pop stack))]
                            (conj st
                                  (update o2 :children conj o1)))
                          acc)
                   ;; same depth
                   :else
                   (if (= (:name op)
                          (:name (peek stack)))
                     (recur (rest ops)
                            (add-op-to-current op stack)
                            acc)
                     (recur (rest ops)
                            (new-stack op)
                            (unwind-stack acc stack)))))

               ;; new block
               (= (:name (first stack)) (:name op))
               (do
                 (println "not nested continue")
                 (recur (rest ops)
                        (add-op-to-current op stack)
                        acc))
               ;; not nested - continue
               :else
               (do
                 (println "new block")
                 (recur (rest ops)
                        (new-stack op)
                        (unwind-stack acc stack)))

               ))))))


  )
