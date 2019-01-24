(ns delta-to-hiccup.core-test
  (:require [clojure.test :refer :all]
            [delta-to-hiccup.core :refer :all]))

(deftest test-simple-inputs
  (testing "simple span"
    (is
     (=
      (to-hiccup
       [{:insert "hi"}
        {:insert "\n"}])
      [:div [:p [:span "hi" [:br]]]]))
    (testing "bold -> strong"
      (is
       (=
        [:div [:p [:span [:strong "hi"] [:br]]]]
        (to-hiccup
         [{:insert "hi" :attributes {:bold true}}
          {:insert "\n"}]))))
    (testing "italic -> em"
      (is
       (=
        (to-hiccup
         [{:insert "hi" :attributes {:italic true}}
          {:insert "\n"}])
        [:div [:p [:span [:em "hi"] [:br]]]])))
    (testing "script:sub -> sub"
      (is
       (=
        (to-hiccup
         [{:insert "hi" :attributes {:script "sub"}}
          {:insert "\n"}])
        [:div [:p [:span [:sub "hi"] [:br]]]])))
    (testing "script:super -> sup"
      (is
       (=
        (to-hiccup
         [{:insert "hi" :attributes {:script "super"}}
          {:insert "\n"}])
        [:div [:p [:span [:sup "hi"] [:br]]]])))
    (testing "background color"
      (is
       (=
        (to-hiccup
         [{:insert "hi" :attributes {:background "#ff0000"}}
          {:insert "\n"}])
        [:div
         [:p [:span
              [:span {:style {:background-color "#ff0000"}} "hi"]
              [:br]]]])))
    (testing "font color"
      (is
       (=
        (to-hiccup
         [{:insert "hi" :attributes {:color "#ff0000"}}
          {:insert "there" :attributes {:background "#ff0000"}}
          {:insert "\n"}])
        [:div [:p
               [:span
                [:span {:style {:color "#ff0000"}} "hi"]
                [:span {:style {:background-color "#ff0000"}} "there"]
                [:br]]]])))
    (testing "adjacent attributes"
      (is
       (=
        (to-hiccup
         [{:insert "hi" :attributes {:color "#ff0000"}}
          {:insert "\n"}])
        [:div [:p [:span [:span {:style {:color "#ff0000"}} "hi"] [:br]]]])))
    (testing "breaking up lines"
      (is
       (=
        (to-hiccup
         [{:insert "hi\n\n\n\nbabyyyy"}
          {:insert "\n"}])
        [:div
         [:p
          [:span "hi" [:br] [:br] [:br] [:br]]
          [:span "babyyyy" [:br]]]])))
    (testing "nested attributes"
      (is
       (=
        [:div [:p [:span [:strong [:em "hi"]] [:br]]]]
        (to-hiccup
         [{:insert "hi" :attributes {:bold true :italic true}}
          {:insert "\n"}]))))
    (testing "all inline elements"
      (is
       (=
        [:div
         [:p
          [:span
           [:a {:href "http://example.com"
                :class " ql-size-large ql-font-serif",
                :style {:background-color "#e60000", :color "#008a00"}}
            [:sub [:strong [:em [:s [:u "hi"]]]]]]
           [:br]]]]
        (to-hiccup
         [{:attributes
           {:bold true,
            :link "http://example.com"
            :color "#008a00",
            :script "sub",
            :background "#e60000",
            :underline true,
            :font "serif",
            :size "large",
            :strike true,
            :italic true},
           :insert "hi"}
          {:insert "\n"}]))))
    (testing "across lines"
      (is
       (=
        (to-hiccup
         [{:insert "aa"}
          {:attributes {:italic true}, :insert "a"}
          {:insert "\n"}
          {:attributes {:italic true}, :insert "b"}
          {:attributes {:italic true, :bold true}, :insert "bb"}
          {:insert "\n"}
          {:attributes {:italic true, :bold true}, :insert "ccc"}
          {:insert "\n"}
          {:attributes {:italic true, :bold true}, :insert "d"}
          {:attributes {:italic true}, :insert "d"}
          {:attributes {:underline true, :italic true}, :insert "d"}
          {:insert "\n"}
          {:attributes {:underline true, :italic true}, :insert "e"}
          {:attributes {:underline true}, :insert "ee"}
          {:insert "\n"}
          {:attributes {:underline true}, :insert "fff"}
          {:insert "\n"}])
        [:div
         [:p
          [:span "aa" [:em "a"] [:br]]
          [:span [:em "b"] [:strong [:em "bb"]] [:br]]
          [:span [:strong [:em "ccc"]] [:br]]
          [:span [:strong [:em "d"]] [:em "d"] [:em [:u "d"]] [:br]]
          [:span [:em [:u "e"]] [:u "ee"] [:br]]
          [:span [:u "fff"] [:br]]]])))))

(deftest test-inserts
  (testing "image insert"
    (is
     (=
      [:div
       [:p
        [:span [:img {:src "test"}] [:br]]]]
      (to-hiccup
       [{:insert {:image "test"}}
        {:insert "\n"}]))))
  (testing "video insert"
    (is
     (=
      [:div
       [:p
        [:span
         [:iframe {:class "ql-video",
                   :frameborder "0",
                   :allowfullscreen "true",
                   :src "test"}]
         [:br]]]]
      (to-hiccup
       [{:insert {:video "test"}}
        {:insert "\n"}])))))
