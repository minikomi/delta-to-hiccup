(ns delta-to-hiccup.core
  (:require [clojure.string :as str]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def delta
  [{"insert" "This "}
   {"attributes" {"italic" true} "insert" "is"}
   {"insert" " "}
   {"attributes" {"bold" true}, "insert" "great!"}
   {"insert" "\n"}])

(def list-test
  ( clojure.walk/keywordize-keys
   [
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

(def block-level-attrs #{:list
                         :code-block
                         :header
                         :blockquote})

(def list-tags {"ordered" :ol
                "bullet" :ul})

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
     {:insert []}]
    [acc (update op-group :insert (fnil conj []) delta)]))

(defn bold [insert]
  [:strong insert])

(defn italic [insert]
  [:em insert])

(defn insert->hiccup [{:keys [bold]}])

(defn hiccup-insert-blocks [{:keys [insert] :as op}]
  (assoc op :hiccup (map insert->hiccup insert)))

(defn normalize-ops [deltas]
  (reduce line-grouper [[] nil] (mapcat break-up-multiline-delta deltas))
  )

(defn continuing-current-list?
  [{:keys [acc
           list-level
           block-type
           block-meta] :as state}
   {{:keys [list indent] :or {indent 0}} :attributes}]
  (and
   list
   (= (get list-tags list) block-type)
   (= indent list-level)))

(defn deeper-or-new-list? [{:keys [acc
                                   list-level
                                   block-type
                                   block-meta] :as state}
                           {{:keys [list indent]
                             :or {indent 0}} :attributes}]
  (and list
       (let [list-block-type (get list-tags list)]
         (if block-type
           (and
            (= list list-block-type)
            (< list-level indent))
           (or (not (#{:ul :ol} block-type))
               (not= block-type list-block-type))))))

(defn shallower-list?
  [{:keys [acc
           list-level
           block-type
           block-meta] :as state}
   {{:keys [list indent]
     :or {indent 0}} :attributes}]
  (and
   list
   (#{:ul :li} block-type)
   (> list-level indent)))

(defn continuing-block-quote?
  [{:keys [acc
           list-level
           block-type
           block-meta] :as state}
   {{:keys [blockquote indent]
     :or {indent 0}} :attributes}]
  (and
   blockquote
   (= :blockquote block-type)))

(defn continuing-header?
  [{:keys [acc
           list-level
           block-type
           block-meta] :as state}
   {{:keys [header indent]
     :or {indent 0}} :attributes}]
  (and
   header
   (= :header block-type)
   (= header (:header-level block-meta))))

(defn close-block-type? [{:keys [block-type
                                 block-meta]}
                         {{:keys [blockquote
                                 list
                                 header
                                 code-block
                                 ]} :attributes}]
  (and block-type
       (or
        (and list (not (#{:ul :ol} block-type)))
        (and header (or (not= :header block-type)
                        (not= header (:header-level block-meta))))
        (and blockquote (not= :blockquote block-type))
        (and code-block (not= :pre block-type)))))

(defn new-block-type?  [{{:keys [blockquote
                                 header
                                 code-block
                                 ]} :attributes}]
  (cond
    (= list "ordered")
    [:ol nil]
    (= list "bullet")
    [:ul nil]
    blockquote
    [:blockquote nil]
    header
    [:header {:header-level header}]
    code-block
    [:pre nil]
    :else false))

(def default-state {:acc []
                    :list-level nil
                    :block-type nil
                    :block-meta {}})

(defn group-block-elements
  ([deltas] (group-block-elements deltas default-state))
  ([deltas {:keys [acc list-level block-type block-meta] :as state}]
   (prn state)
   (if (empty? deltas)
     [acc []]
     (let [[d & eltas] deltas
           {:keys [attributes insert]} d
           {:keys [list
                   blockquote
                   code-block]} attributes]
       (cond
         ;; Continue tag
         ;; --------------------------------------
         (or (continuing-current-list? state d)
             (continuing-block-quote? state d)
             (continuing-header? state d))
         (recur eltas (update state :acc conj d))
         ;; Close tag / pop list
         ;; --------------------------------------
         (or (close-block-type? state d)
             (shallower-list? state d))
         (do
           (println "close")
           [acc deltas])
         ;; List
         ;; --------------------------------------
         (deeper-or-new-list? state d)
         (let [[contents remaining-deltas]
               (do (println "deeper list")
                   (group-block-elements
                    deltas
                    {:list-level (if list-level
                                   (inc list-level)
                                   0)
                     :acc []
                     :block-type (get list-tags list)
                     :block-meta nil}))])
         ;; Default
         ;; ---------------------------------------
         :else
         (if-let [[block-type block-meta] (new-block-type? d)]
           ;; new block type
           (let [[contents remaining-deltas]
                 (group-block-elements
                  deltas
                  {:list-level nil
                   :acc []
                   :block-type block-type
                   :block-meta block-meta})]
             (recur remaining-deltas
                    (update state
                            :acc conj [block-type contents])))
           ;; regular insert
           (recur eltas (update state :acc conj [:p insert]))))))))

(comment
  (def n (first (normalize-ops list-test)))
  (first (group-block-elements n))

  )
