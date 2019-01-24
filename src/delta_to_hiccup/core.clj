(ns delta-to-hiccup.core
  (:require [clojure.string :as str]))

;; Helper function

(defn update-attr
  "Takes a hiccup like tag and adds to the attributes
   depending on the type of the new attribute
     - If the attribute is a vector, will conj new attr
     - If map, will merge
     - If string, will join with a space in the middle

    The insert can be a bare string, in which case it will
    be wrapped in a :span. Otherwise, it will try to add to
    the attributes map in the 1-th position, or will create it.
  "
  [insert attr new-attr]
  (let [[f base] (cond
                   (vector? new-attr) [conj []]
                   (map? new-attr) [merge {}]
                   (string? new-attr) [#(str % " " %2) ""])]
    (cond
      (string? insert)
      [:span {attr (f base new-attr)} insert]
      (map? (get insert 1))
      (update-in insert [1 attr] (fnil f base) new-attr)
      :else
      (into
       [(first insert)
        {attr (f base new-attr)}]
       (subvec insert 1)))))

;; defaults

(def default-embeds
  [{:pred :image
    :convert (fn [{:keys [image]}]
               [:image {:src image}])}
   {:pred :video
    :convert (fn [{:keys [video]}]
               [:iframe {:class "ql-video",
                         :frameborder "0",
                         :allowfullscreen "true",
                         :src video}])}])

(def embeds (atom default-embeds))

(defn convert-embed [insert]
  (println "convert:" insert)
  (if (string? insert) insert
      (if-let [convert-fn
               (some #(when ((:pred %) insert)
                        (:convert %))
                     @embeds)]
        (convert-fn insert)
        "")))

(def default-inline-elements
  [{:pred :underline
    :wrap (fn [insert attributes]
            [:u insert])}
   {:pred :strike
    :wrap (fn [insert attributes]
            [:s insert])}
   {:pred :italic
    :wrap (fn [insert attributes]
            [:em insert])}
   {:pred :bold
    :wrap (fn [insert attributes]
            [:strong insert])}
   {:pred (fn [{:keys [script]}]
            (= script "sub"))
    :wrap (fn [insert attributes]
            [:sup insert])}
   {:pred (fn [{:keys [script]}]
            (= script "super"))
    :wrap (fn [insert attributes]
            [:sub insert])}
   {:pred :link
    :wrap (fn [insert attributes]
            [:a {:href (:link attributes)}
             insert])}
   {:pred :color
    :wrap (fn [insert attributes]
            (update-attr insert
                         :style
                         {:color (:color attributes)}))}
   {:pred :background
    :wrap (fn [insert attributes]
            (update-attr insert
                         :style
                         {:background-color (:background attributes)}))}
   {:pred (fn [{:keys [size]}]
            (and size (not= size "normal")))
    :wrap (fn [insert attributes]
            (update-attr insert
                         :class
                         (str "ql-size-" (:size attributes))))}
   {:pred (fn [{:keys [font]}]
            (and font (not= font "normal")))
    :wrap (fn [insert attributes]
            (update-attr insert
                         :class
                         (str "ql-font-" (:font attributes))))}])

(def inline-elements (atom default-inline-elements))

(def default-block-elements
  [{:name "Ordered List"
    :pred (fn [{:keys [attributes] :as op}]
            (= "ordered" (:list attributes)))
    :type :nested-block
    :outer-tag :ol
    :inner-tag :li}
   {:name "Bullet list"
    :pred (fn [{:keys [attributes] :as op}]
            (= "bullet" (:list attributes)))
    :type :nested-block
    :outer-tag :ul
    :inner-tag :li}
   {:name "Block Quote"
    :pred (fn [{:keys [attributes] :as op}]
            (:blockquote attributes))
    :type :block
    :outer-tag :blockquote}
   {:name "Code Block"
    :pred (fn [{:keys [attributes] :as op}]
            (:code-block attributes))
    :type :block
    :outer-tag :pre}
   {:name "Header"
    :pred (fn [{:keys [attributes] :as op}]
            (#{1 2 3 4 5 6 7} (:header attributes)))
    :type :block
    :outer-tag (fn header-outer-fn [attributes]
                 (keyword (str "h" (:header attributes))))}
   {:name "Default Paragrapgh"
    :pred (fn [children] true)
    :type :block
    :outer-tag :p}])

(def block-elements (atom default-block-elements))

(def default-tag-attrs
  [{:pred (fn [{:keys [align]}]
            (#{"center" "right" "justify"} align))
    :wrap (fn [tag attributes]
            (update-attr tag
                         :class
                         (str "ql-align-" (:align attributes))))}
   {:pred (fn [{:keys [direction]}]
            (#{"rtl"} direction))
    :wrap (fn [tag attributes]
            (update-attr tag
                         :class
                         (str "ql-direction-" (:direction attributes))))}])

(def tag-attributes (atom default-tag-attrs))

;; rendering inner-tag attributes

(defn render-tag [tag attributes]
  (let [fns (map :wrap (filter #((:pred %) attributes) @tag-attributes))]
    (reduce #(%2 % attributes) [tag] fns)))

;; rendering inline elements

(defn render-inline-elements [{:keys [insert attributes]}]
  (let [fns (map :wrap (filter #((:pred %) attributes) @inline-elements))]
    (reduce #(%2 % attributes) (convert-embed insert) fns)))

(defn render-inner [{:keys [inner-tag inserts newlines attributes]}]
  (let [base-tag (or inner-tag :span)
        inserts (when inserts (into (mapv render-inline-elements inserts)))
        newlines (into (take newlines (repeat [:br])))]
    (cond-> (render-tag base-tag attributes)
      inserts (into inserts)
      newlines (into newlines))))

;; render block elements

(defn render-block [{:keys [outer-tag attributes children]}]
  (let [outer (if (fn? outer-tag)
                (outer-tag attributes)
                outer-tag)]
    (into [outer] children)))

;; processing raw deltas

(defn delta-has-internal-linebreak? [{:keys [insert]}]
  (re-find #"[^\n]\n|\n[^\n]" insert))

(defn delta-is-newline-only? [{:keys [insert]}]
  (= #{\newline} (set insert)))

(defn break-up-multiline-delta
  "If a delta has newlines within, split the delta into
  text-only parts and newline parts."
  [{:keys [insert attributes]}]
  (if (string? insert)
   (->> insert
        (partition-by #{\newline})
        (map #(apply str %))
        (mapv
         (fn [part] {:insert part
                     :attributes attributes})))
   [{:insert insert
     :attributes attributes}]))

(defn line-grouper [[acc op-group] delta]
  "groups all deltas up until a newline, for processing
   that block's block-attributes"
  (if (delta-is-newline-only? delta)
    [(conj acc (assoc op-group
                      :attributes (:attributes delta)
                      :newlines (count (:insert delta))))
     {:inserts []}]
    [acc (update op-group :inserts (fnil conj []) delta)]))

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

;; stack / loop fns

(defn add-child [op stack]
  (let [current (peek stack)]
    (conj (pop stack)
          (update current
                  :children
                  (fnil conj [])
                  (render-inner op)))))

(defn collapse-once [stack]
  (let [st (pop (pop stack))
        o1 (peek stack)
        o2 (peek (pop stack))]
    (conj st
          (update o2 :children
                  (fnil conj [])
                  [(:inner-tag o2)
                   (render-block o1)]))))

(defn unwind-stack [acc stack]
  (if (empty? stack) acc
    (loop [stack stack]
      (if (= (count stack) 1)
        (conj acc (render-block (first stack)))
        (recur (collapse-once stack))))))

(defn add-block-to-stack [stack op]
  (conj stack (dissoc op :inserts :attributes :children)))

(defn to-hiccup [deltas]
  (loop [ops (normalize-ops deltas)
         stack []
         acc []]
    (if (empty? ops) (into [:div] (unwind-stack acc stack))
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
                       (add-block-to-stack stack op)
                       acc)
                ;; shallower
                (> current-depth depth)
                (recur ops
                       (collapse-once stack)
                       acc)
                ;; same depth, same type
                (= (:name op) (:name (peek stack)))
                (recur (rest ops)
                       (add-child op stack)
                       acc)
                ;; same depth but doesn't match - start new
                :else
                (recur ops
                       (add-block-to-stack [] op)
                       (unwind-stack acc stack))
                ))
            (= (:name (first stack)) (:name op))
            (recur (rest ops)
                   (add-child op stack)
                   acc)
            :else
            (recur ops
                   (add-block-to-stack [] op)
                   (unwind-stack acc stack)))))))
