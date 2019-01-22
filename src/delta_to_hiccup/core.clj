(ns delta-to-hiccup.core
  (:require [clojure.string :as str]))

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

(defn determine-block-element [{:keys [inserts attributes newlines] :as op}]
  (some #(when ((:pred %) op)
           (assoc %
                  :inserts inserts
                  :newlines newlines
                  :attributes attributes))
        @block-elements))

(defn update-attr [insert attr new-attr]
  (let [[f base] (cond
                     (vector? new-attr) [conj []]
                     (map? new-attr) [merge {}]
                     (string? new-attr) [str ""])]
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

(defn normalize-ops [deltas]
  (->> deltas
       (mapcat break-up-multiline-delta)
       (reduce line-grouper [[] nil])
       first
       (mapv determine-block-element)))

(defn render-block [{:keys [outer-tag attributes children inner-tag]}]
  (let [outer (if (fn? outer-tag)
                (outer-tag attributes)
                outer-tag)]
    (into [outer]
          children)))

(defn render-inline-elements [{:keys [insert attributes]}]
  (let [fns (map :wrap (filter #((:pred %) attributes) @inline-elements))]
    (reduce #(%2 % attributes) insert fns)))

(defn render-inner [{:keys [inner-tag inserts raw-inserts newlines attributes]}]
  (cond-> (vector (or inner-tag :span))
    attributes (conj attributes)
    (not-empty inserts) (into (mapv render-inline-elements inserts))
    raw-inserts (conj raw-inserts)
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
                                  (render-inner (assoc op :raw-inserts prev)))))
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
  (to-hiccup
   (clojure.walk/keywordize-keys
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
      "attributes" {"bold" true}
      },
     {
      "attributes" {
                    "indent" 3,
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
      }
     {
      "attributes" {
                    "align" "right"
                    },
      "insert" "\n"
      }

     {
      "attributes" {
                    "underline" true,
                    "strike" true,
                    "italic" true,
                    "bold" true,
                    "color" "#e60000",
                    "background" "#008a00",
                    "size" "small",
                    "font" "monospace",
                    "link" "google.com"
                    },
      "insert" "bbbzz"
      },
     {
      "insert" "\t"
      },
     {
      "attributes" {
                    "align" "center",
                    "direction" "rtl",
                    "list" "bullet"
                    },
      "insert" "\n"
      }

     ]
    ))

  )
