(ns smallrouter.core
  (:require [io.pedestal.http.route.router :as router]
            [io.pedestal.http.route.path :as path]
            [io.pedestal.http.route.prefix-tree :as prefix-tree]
            [io.pedestal.http.route.map-tree :as map-tree]
            [io.pedestal.http.route.linear-search :as linear-search]
            [criterium.core :as criterium :refer [bench quick-bench]]
            [profile.core :as thunkprofile]))

;; Let's assume I have a web service with 10 routes, all static...

(def static-routes {"/app" (fn [req] "Hello App")
                    "/resource1" (fn [req] "Hello Resource")
                    "/resource1/attribute1" (fn [req] "Hello Attr1")
                    "/resource1/attribute2" (fn [req] "Hello Attr2")
                    "/resource1/attribute2/subattr1" (fn [req] "Hello SubAttr2-1")
                    "/resource1/attribute2/anothersubattr2" (fn [req] "Hello SubAttr2-2")
                    "/tworesource" (fn [req] "Hello Resource2")
                    "/tworesource/attribute1" (fn [req] "Hello Resource2-Attr1")
                    "/tworesource/attr2" (fn [req] "Hello Resource2-Attr2")
                    "/tworesource/onemoreattribute3" (fn [req] "Hello Resource2-Attr3")})

(def ped-static-routes (mapv #(hash-map :path %) (keys static-routes)))

;; Let's assume traffic is evenly spread across all URLs, and all requests are HTTP GETs

(defn random-route []
  (rand-nth (keys static-routes)))

;; Utils for old Pedestal route transforms
(defn expand-route-path [route]
  (->>  (:path route)
       path/parse-path
       (merge route)
       path/merge-path-regex))

;; What is the fastest way to find a matching static route?
;; ---------------------------------------------------------

;; Map lookup
(defn map-matcher []
  (fn [route]
    (static-routes route)))

;; String compare in a sequence with map
(defn seqmap-matcher []
  (fn [route]
    (first (keep (fn [[static-rt handler]]
                   (when (= route static-rt)
                     handler))
                 static-routes))))

;; String compare in a sequence with loop
(defn seqloop-matcher []
  (let [vec-routes (vec (seq static-routes))]
    (fn [^String route]
      (loop [[static-rt handler] (first vec-routes)
             routes (next vec-routes)]
        (cond
          (= route static-rt) handler
          (nil? static-rt) nil
          :else (recur (first routes) (next routes)))))))

;; String compare in an array with areduce
(defn areduce-matcher []
  (let [arr-routes (into-array Object (flatten (seq static-routes)))]
    (fn [^String route]
      (areduce ^objects arr-routes i ret nil
               (or ret (when (= (aget ^objects arr-routes i) route)
                         (aget ^objects arr-routes (unchecked-inc i))))))))

;; String compare in an array with loop
(defn arrloop-matcher []
  (let [arr-routes (into-array Object (flatten (seq static-routes)))
        arrlen (alength ^objects arr-routes)]
    (fn [^String route]
      (loop [i (int 0)]
        (cond
          (>= i arrlen) nil
          (= (aget ^objects arr-routes i) route) (aget ^objects arr-routes (unchecked-inc i))
          :else (recur (unchecked-add i 2)))))))

(comment
  (def mm-router (map-matcher))
  (def sm-router (seqmap-matcher))
  (def sl-router (seqloop-matcher))
  (def al-router (arrloop-matcher))
  (def ar-router (areduce-matcher))

  (= (mm-router "/not-found")
     (sm-router "/not-found")
     (sl-router "/not-found")
     (al-router "/not-found")
     (ar-router "/not-found"))

  (= (mm-router "/app")
     (sm-router "/app")
     (sl-router "/app")
     (al-router "/app")
     (ar-router "/app"))

  (quick-bench (mm-router "/app")) ;;   28.738859 ns
  (quick-bench (sm-router "/app")) ;; 1246.463000 ns
  (quick-bench (sl-router "/app")) ;;  710.010481 ns
  (quick-bench (al-router "/app")) ;;  434.769799 ns
  (quick-bench (ar-router "/app")) ;;  889.633374 ns

  (quick-bench (mm-router "/resource1/attribute2/anothersubattr2")) ;;  39.843952 ns
  (quick-bench (sm-router "/resource1/attribute2/anothersubattr2")) ;; 517.020029 ns
  (quick-bench (sl-router "/resource1/attribute2/anothersubattr2")) ;; 300.951481 ns
  (quick-bench (al-router "/resource1/attribute2/anothersubattr2")) ;; 141.761121 ns
  (quick-bench (ar-router "/resource1/attribute2/anothersubattr2")) ;; 313.458445 ns

  (def mt-router (map-tree/router ped-static-routes))
  (def pt-router (prefix-tree/router ped-static-routes))
  (def ls-router (linear-search/router (mapv expand-route-path ped-static-routes)))

  (def app-route {:path-info "/app"})
  (def resource-route {:path-info "/resource1/attribute2/anothersubattr2"})

  (quick-bench (router/find-route mt-router app-route)) ;;   63.465847 ns
  (quick-bench (router/find-route pt-router app-route)) ;;  741.584785 ns
  (quick-bench (router/find-route ls-router app-route)) ;; 1722.548000 ns

  (quick-bench (router/find-route mt-router resource-route)) ;;   68.847378 ns
  (quick-bench (router/find-route pt-router resource-route)) ;; 2298.445000 ns
  (quick-bench (router/find-route ls-router resource-route)) ;; 1021.260000 ns

  ;; Let's isolate just the prefix tree lookup, which is much closer to the above
  (def ptree (reduce (fn [t [path f]]
                       (prefix-tree/insert t path f))
                     nil
                     static-routes))
  (quick-bench (prefix-tree/lookup ptree "/app")) ;; 519.284763 ns
  (quick-bench (prefix-tree/lookup ptree "/resource1/attribute2/anothersubattr2")) ;; 1.993672 µs
  )

