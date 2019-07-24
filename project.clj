(defproject smallrouter "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [io.pedestal/pedestal.service "0.5.7"]
                 [metosin/reitit "0.3.9"]
                 [bidi "2.1.6"]
                 [clout "2.2.1"]
                 [criterium "0.4.4"]

                 ;; Cleanup
                 [com.fasterxml.jackson.core/jackson-core "2.9.9"]
                 [com.fasterxml.jackson.core/jackson-databind "2.9.9"]
                 [prismatic/schema "1.1.11"]

                 ]
  :global-vars {*warn-on-reflection* true
                ;*unchecked-math* :warn-on-boxed
                ;*compiler-options* {:disable-locals-clearing true}
                *assert* true}
  :pedantic? :abort
  ;:offline? true
  :jvm-opts ^:replace [;"-D\"clojure.compiler.direct-linking=true\""
                       "-d64" "-server"
                       "-Xms1g"                             ;"-Xmx1g"
                       "-XX:+UnlockCommercialFeatures"      ;"-XX:+FlightRecorder"
                       ;"-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8030"
                       "-XX:+UseG1GC"
                       ;"-XX:+UseConcMarkSweepGC" "-XX:+UseParNewGC" "-XX:+CMSParallelRemarkEnabled"
                       ;"-XX:+ExplicitGCInvokesConcurrent"
                       "-XX:+AggressiveOpts"
                       ;-XX:+UseLargePages
                       "-XX:+UseCompressedOops"])
