(in-ns 'user)

((requiring-resolve 'clojure+.error/install!))
((requiring-resolve 'clojure+.hashp/install!))
((requiring-resolve 'clojure+.print/install!))

(import 'org.junit.platform.console.ConsoleLauncher)
(def console (System/console))
(def stdout (.writer console))
(def stderr (.writer console))

(defn junit
  ([] (junit "^(Test.*|.+[.$]Test.*|.*Tests?)$"))
  ([include]
   (ConsoleLauncher/run stdout stderr
                        (into-array String ["execute" "--disable-banner" "--scan-classpath"
                                            "-cp" "java/target/classes:java/target/test-classes"
                                            "-n" include]))))

(require '[clj-reload.core :as reload])
(reload/init {:dirs ["clojure/src"]})

(require 'virgil)
(virgil/watch-and-recompile ["java/src" "java/test"]
                            :options ["--release" "21" "-Xlint:unchecked"]
                            :post-hook #(do
                                          (junit)
                                          (reload/reload {:only :loaded}))
                            :verbose true)
