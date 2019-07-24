(ns smallrouter.core
  (:require [io.pedestal.http.route.router :as router]
            [io.pedestal.http.route.path :as path]
            [io.pedestal.http.route.prefix-tree :as prefix-tree]
            [io.pedestal.http.route.map-tree :as map-tree]
            [io.pedestal.http.route.linear-search :as linear-search]
            [clout.core :as clout]
            [bidi.bidi :as bidi]
            [reitit.core :as reitit]
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

(defn reitit-router []
  (let [rrouter (reitit/router static-routes)]
    (fn [req]
      (:data (reitit/match-by-path rrouter (:path-info req))))))

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
  (def rr-router (reitit-router))

  (def app-route {:path-info "/app"})
  (def resource-route {:path-info "/resource1/attribute2/anothersubattr2"})

  (quick-bench (rr-router app-route))                   ;;   20.4926   ns
  (quick-bench (router/find-route mt-router app-route)) ;;   59.537676 ns
  (quick-bench (router/find-route pt-router app-route)) ;;  741.584785 ns
  (quick-bench (router/find-route ls-router app-route)) ;; 1722.548000 ns
  (quick-bench (cc-router app-route))                   ;; 1236.624000 ns
  (quick-bench (bb-router app-route))                   ;; 4870.922000 ns

  (quick-bench (rr-router resource-route))                   ;;   31.7966   ns
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


  ;; Looking at map lookups...
  (def am {:a 1 :b 2})
  (def hm {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9})
  (def um (Map/of :a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9))
  (def jhm (doto (HashMap.)
             (.put :a 1)
             (.put :b 2)
             (.put :c 3)
             (.put :d 4)
             (.put :e 5)
             (.put :f 6)))
  (def uhm (Collections/unmodifiableMap ^Map jhm))
  (defrecord TRec [a b])
  (def ar (TRec. 1 2))
  (def mar (assoc ar :c 3 :d 4))
  (def cf (fn [x]
            (case x
              :a 1
              :b 2
              :c 3
              :d 4
              :e 5)))

  ;; AMap with key access
  (quick-bench (:a am)) ;; 8.66 ns

  ;; HMap with key access
  (quick-bench (:f hm)) ;; 25.54 ns

  ;; AMap with get access
  (quick-bench (get am :a)) ;; 6.92 ns

  ;; HMap with get access
  (quick-bench (get hm :a)) ;; 17.37 ns

  ;; UMap with get access
  (quick-bench (get um :a)) ;; 43.91 ns
  (quick-bench (.get ^java.util.Map um :a)) ;; 10.36 ns
  (quick-bench (.get ^java.util.Map uhm :a)) ;; 9.01 ns
  (quick-bench (.get ^java.util.Map um :f)) ;; 10.34 ns
  (quick-bench (.get ^java.util.Map uhm :f)) ;; 8.83 ns

  ;; JHMap with get access
  ;; The cost here is RT.get -- you have to fall through to another method call,
  ;; another instance check, and a cast before `.get` is called -- 25 ns.
  (quick-bench (get jhm :a)) ;; 33.22 ns
  (quick-bench (.get ^java.util.Map jhm :a)) ;; 7.69 ns

  ;; AMap with two get calls -- 13.77 ns
  (quick-bench (do
                 (get am :a)
                 (get am :b)))

  ;; Unmod'Map with two get calls -- 85.54 ns
  (quick-bench (do
                 (get um :a)
                 (get um :b)))

  ;; Two maps with interleaving get calls (does the JIT deoptimize?)
  ;; 35.57 ns
  (quick-bench (do
                 (get am :a)
                 (get hm :b)))

  ;; Record with key access
  (quick-bench (:a ar)) ;; 6.36 ns

  ;; Record with field access
  (quick-bench (.a ^TRec ar)) ;; 5.13 ns

  ;; Modified Record with key access
  (quick-bench (:a mar)) ;; 7.12 ns
  (quick-bench (:c mar)) ;; 16.97 ns

  ;; Modified Record with field access
  (quick-bench (.a ^TRec mar)) ;; 5.58 ns

  ;; Case-fn with key lookup
  (quick-bench (cf :a)) ;; 3.27 ns
  (quick-bench (cf :e)) ;; 0.2 ns


  )

