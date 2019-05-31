(ns smallrouter.core
  (:require [io.pedestal.http.route.router :as router]
            [io.pedestal.http.route.path :as path]
            [io.pedestal.http.route.prefix-tree :as prefix-tree]
            [io.pedestal.http.route.map-tree :as map-tree]
            [io.pedestal.http.route.linear-search :as linear-search]
            [clout.core :as clout]
            [bidi.bidi :as bidi]
            [criterium.core :as criterium :refer [bench quick-bench]]
            ;[profile.core :as thunkprofile]
            )
  (:import (java.util Map
                      HashMap
                      Collections)))

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

(def java-static-routes (Collections/unmodifiableMap static-routes))

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

;; Expand the routes into a `case` statement
(defmacro make-case-router
  [route-map]
  (let [route-seq (mapcat identity (if (symbol? route-map) @(resolve route-map) route-map))]
    `(fn [route#]
       (case route#
         ~@route-seq
         nil))))

(defn case-matcher []
  (make-case-router static-routes))

(case-matcher)

(defn clout-router []
  ;; Compojure's router is a linear sequence walk using `some` via the `routing` fn
  ;; It uses Clout's Route protocol to find a match via `route-matches`.
  ;; To make this as realistic as possible, without middlewares in play, compile all routes.
  (let [croutes (mapv clout/route-compile (keys static-routes))]
    (fn [req]
      (some #(clout/route-matches % req) croutes))))

(defn bidi-router []
  ;; Bidi doesn't normalize route paths, removing redundant "/",
  ;; so we have to 'compile' and normalize the routes by hand
  (let [broutes ["/" (reduce (fn [acc [path handler-fn]]
                               (assoc acc (subs path 1) handler-fn))
                             {}
                             static-routes)]]
    (fn [req]
      (:handler (bidi/match-route broutes (:path-info req))))))


(comment
  (def mm-router (map-matcher))
  (def sm-router (seqmap-matcher))
  (def sl-router (seqloop-matcher))
  (def al-router (arrloop-matcher))
  (def ar-router (areduce-matcher))
  (def ca-router (case-matcher))

  (= (mm-router "/not-found")
     (sm-router "/not-found")
     (sl-router "/not-found")
     (al-router "/not-found")
     (ar-router "/not-found")
     (ca-router "/not-found"))

  (= (mm-router "/app")
     (sm-router "/app")
     (sl-router "/app")
     (al-router "/app")
     (ar-router "/app")
     ;(ca-router "/app") ;; Note: This makes new functions
     )

  (= ((mm-router "/resource1") 1)
     ((sm-router "/resource1") 1)
     ((sl-router "/resource1") 1)
     ((al-router "/resource1") 1)
     ((ar-router "/resource1") 1)
     ((ca-router "/resource1") 1))

  (quick-bench (mm-router "/app")) ;;   28.738859 ns
  (quick-bench (sm-router "/app")) ;; 1246.463000 ns
  (quick-bench (sl-router "/app")) ;;  710.010481 ns
  (quick-bench (al-router "/app")) ;;  434.769799 ns
  (quick-bench (ar-router "/app")) ;;  889.633374 ns
  (quick-bench (ca-router "/app")) ;;  0.2 ns

  (quick-bench (mm-router "/resource1/attribute2/anothersubattr2")) ;;  39.843952 ns
  (quick-bench (sm-router "/resource1/attribute2/anothersubattr2")) ;; 517.020029 ns
  (quick-bench (sl-router "/resource1/attribute2/anothersubattr2")) ;; 300.951481 ns
  (quick-bench (al-router "/resource1/attribute2/anothersubattr2")) ;; 141.761121 ns
  (quick-bench (ar-router "/resource1/attribute2/anothersubattr2")) ;; 313.458445 ns
  (quick-bench (ca-router "/resource1/attribute2/anothersubattr2")) ;; 0.5 ns

  (def mt-router (map-tree/router ped-static-routes))
  (def pt-router (prefix-tree/router ped-static-routes))
  (def ls-router (linear-search/router (mapv expand-route-path ped-static-routes)))
  (def cc-router (clout-router))
  (def bb-router (bidi-router))

  (def app-route {:path-info "/app"})
  (def resource-route {:path-info "/resource1/attribute2/anothersubattr2"})

  (quick-bench (router/find-route mt-router app-route)) ;;   59.537676 ns
  (quick-bench (router/find-route pt-router app-route)) ;;  741.584785 ns
  (quick-bench (router/find-route ls-router app-route)) ;; 1722.548000 ns
  (quick-bench (cc-router app-route))                   ;; 1236.624000 ns
  (quick-bench (bb-router app-route))                   ;; 4870.922000 ns

  (quick-bench (router/find-route mt-router resource-route)) ;;   68.847378 ns
  (quick-bench (router/find-route pt-router resource-route)) ;; 2298.445000 ns
  (quick-bench (router/find-route ls-router resource-route)) ;; 1021.260000 ns
  (quick-bench (cc-router resource-route))                   ;;  763.189831 ns
  (quick-bench (bb-router resource-route))                   ;; 2655.867000 ns

  ;; Let's isolate just the prefix tree lookup, which is much closer to the above
  (def ptree (reduce (fn [t [path f]]
                       (prefix-tree/insert t path f))
                     nil
                     static-routes))
  (quick-bench (prefix-tree/lookup ptree "/app")) ;; 519.284763 ns
  (quick-bench (prefix-tree/lookup ptree "/resource1/attribute2/anothersubattr2")) ;; 1.993672 µs

  ;; Just map lookups
  (def java-static-routes2 (Collections/unmodifiableMap (HashMap. ^Map java-static-routes)))

  (quick-bench (.get ^Map java-static-routes2 "/tworesource/attribute1")) ;; 0.322747 ns
  (quick-bench (.get ^Map java-static-routes "/tworesource/attribute1")) ;; 27.767645 ns
  (quick-bench (get static-routes "/tworesource/attribute1")) ;; 28.201482 ns


  )

