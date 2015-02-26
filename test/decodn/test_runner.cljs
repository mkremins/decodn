(ns decodn.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            decodn.tests))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful? (run-tests 'decodn.tests)) 0 1))

(set! *main-cli-fn* runner)
