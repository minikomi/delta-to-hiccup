(ns delta-to-hiccup.core-test
  (:require [clojure.test :refer :all]
            [delta-to-hiccup.core :refer :all]))

(deftest test-simple-inputs
  (testing "simple span"
    (is
     (=
      [:div [:p [:span "hi" [:br]]]]
      (to-hiccup
       [{:insert "hi"}
        {:insert "\n"}])
      ))
    (testing "bold -> strong"
     (is
      (=
       [:div [:p [:span [:strong "hi"] [:br]]]]
       (to-hiccup
        [{:insert "hi" :attributes {:bold true}}
         {:insert "\n"}])
       )))
    (testing "nested attributes"
      (is
       (=
        [:div [:p [:span [:strong [:em "hi"]] [:br]]]]
        (to-hiccup
         [{:insert "hi" :attributes {:bold true :italic true}}
          {:insert "\n"}])
        )))))
