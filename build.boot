(def +project+ 'co.poyo/delta-to-hiccup)
(def +version+ "0.1.0-SNAPSHOT")

(set-env!
 :source-paths #{"src"}
 :dependencies '[;; ---- clj ----
                 [org.clojure/clojure "RELEASE"]
                 ;; ---- dev ----
                 [samestep/boot-refresh "0.1.0" :scope "test"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]
                 ;; ---- test ---
                 [metosin/bat-test "0.4.2"
                  :exclude #{org.clojure.tools.reader}
                  :scope "test"]
                 [org.clojure/tools.reader "1.3.2"
                  :scope "test"]
                 [org.clojure/tools.nrepl "0.2.12"
                  :scope "test"]
                 ])

(require
 '[samestep.boot-refresh :refer [refresh]]
 '[adzerk.bootlaces :refer [bootlaces! build-jar push-snapshot push-release]]
 '[metosin.bat-test :refer [bat-test]])

(bootlaces! +version+)

(task-options!
 pom  {:project     +project+
       :version     +version+
       :description "opinionated quill delta -> hiccup conversion"
       :url         "https://github.com/minikomi/delta-to-hiccup"
       :scm         {:url "https://github.com/minikomi/delta-to-hiccup"}
       :license     {"Eclipse Public License"
                     "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask test-options []
  (merge-env! :source-paths #{"test"}
              :resources #{"testdata"})
  identity)

(deftask run-tests
  []
  (comp
   (test-options)
   (bat-test)
   ))

(deftask cider "CIDER profile" []
  (alter-var-root #'clojure.main/repl-requires conj
                  '[sekistone.server.repl :refer [start! stop! restart!]])
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[cider/cider-nrepl "0.19.0"]
                  [refactor-nrepl "2.4.0"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  (repl :server true))

(deftask dev-repl
  []
  (comp
   (cider)
   (watch)
   (refresh)))

(deftask snapshot-to-clojars
  []
  (comp
   (build-jar)
   (push-snapshot)))
