(ns smallrouter.core
  (:require [io.pedestal.http.route.router :as router]
            [io.pedestal.http.route.path :as path]
            [io.pedestal.http.route.prefix-tree :as prefix-tree]
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

;; What is the fastest way to find a matching route?
;; --------------------------------------------------

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
  (def sm-router (seqmap-matcher))
  (def sl-router (seqloop-matcher))
  (def al-router (arrloop-matcher))
  (def ar-router (areduce-matcher))

  (= (sm-router "/not-found")
     (sl-router "/not-found")
     (al-router "/not-found")
     (ar-router "/not-found"))

  (= (sm-router "/app")
     (sl-router "/app")
     (al-router "/app")
     (ar-router "/app"))

  (quick-bench (sm-router "/app")) ;; 1.906463 µs
  (quick-bench (sl-router "/app")) ;; 959.100146 ns
  (quick-bench (al-router "/app")) ;; 529.997928 ns
  (quick-bench (ar-router "/app")) ;; 1.079943 µs

  (quick-bench (sm-router "/resource1/attribute2/anothersubattr2")) ;; 766.096879 ns
  (quick-bench (sl-router "/resource1/attribute2/anothersubattr2")) ;; 431.016547 ns
  (quick-bench (al-router "/resource1/attribute2/anothersubattr2")) ;; 190.126422 ns
  (quick-bench (ar-router "/resource1/attribute2/anothersubattr2")) ;; 452.719062 ns

  (def pt-router (prefix-tree/router ped-static-routes))
  (def ls-router (linear-search/router (mapv expand-route-path ped-static-routes)))

  (quick-bench (router/find-route pt-router {:path-info "/app"})) ;; 1.063224 µs
  (quick-bench (router/find-route ls-router {:path-info "/app"})) ;; 2.319130 µs

  (quick-bench (router/find-route pt-router {:path-info "/resource1/attribute2/anothersubattr2"})) ;; 3.326260 µs
  (quick-bench (router/find-route ls-router {:path-info "/resource1/attribute2/anothersubattr2"})) ;; 1.278639 µs
  )

