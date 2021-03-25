(ns pathom-smart-maps.async
  (:refer-clojure :exclude [let])
  (:require-macros [pathom-smart-maps.async])
  (:require [cljs.core :as c]
            [cljs.core.async :as async]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]))

(defn- gen-parser [resolvers]
  (p/async-parser
    {::p/env {::p/reader [p/map-reader
                          pc/async-reader2
                          pc/open-ident-reader
                          pc/index-reader
                          p/env-placeholder-reader]
              ::p/placeholder-prefixes #{">"}}
     ::p/plugins [(pc/connect-plugin {::pc/register resolvers})
                  p/error-handler-plugin
                  p/trace-plugin]}))

(def SmartMap (js* "class SmartMap extends Promise {
  __state_then(fn) {
    return super.then(fn)
  }
}"))

(defn- norm-cache [cache]
  (atom
   (cond
     (map? cache) cache
     (nil? cache) {}
     (instance? Atom cache) @cache
     :else cache)))

(defn- norm-state [{:keys [cache cursor req-cache not-found-cache env] :as params}]
  (c/let [req-cache (norm-cache req-cache)
          cache (norm-cache cache)]
    (assoc params
           :env (assoc env
                       ::p/entity cache
                       ::p/request-cache req-cache)
           :cursor (or cursor [])
           :cache cache
           :req-cache req-cache
           :not-found-cache (atom (or not-found-cache #{})))))

(defn- ->SmartMap [state]
    (. SmartMap resolve (norm-state state)))

(defn- compare-to [^js this other]
  (if (instance? SmartMap other)
    (= (.-_state this) (.-_state other))
    (= (.-_state this) other)))

; (defn- wrap-result [^js sm [key cached-value]]
;   (c/let [state (.-_state sm)]
;     (->SmartMap (update state :cursor conj key))))

(declare sm-get)
(defn- make-query! [state k default]
  (c/let [{:keys [parser env]} state]
    (-> (js/Promise.
         (fn [resolve reject]
           (async/go
             ;; FIXME - error case
             (c/let [res (async/<! (parser env [k]))]
               (if (#{::p/not-found ::p/reader-error} (get res k ::p/not-found))
                 (swap! (:not-found-cache state) conj k)
                 (swap! (:cache state) merge res))
               (resolve (sm-get state k default)))))))))
               ; (resolve state)))]))
        ; (.__state_then #(sm-get % k default))))

(defn- sm-get [state k default]
  (c/let [{:keys [not-found-cache cache eql cursor]} state
          cache (get-in @cache cursor)
          cached (find cache k)]
    (cond
      cached (update state :cursor conj (first cached))
      (contains? @not-found-cache k) (assoc state :final-val default)
      :else (make-query! state k default))))

; (extend-protocol ILookup
;   SmartMap
;   (-lookup
;    ([this k] (.__state_then this #(doto (sm-get % k nil) (prn ::LOL))))
;    ([this k not-found] (.__state_then this #(sm-get % k not-found)))))
;
(defn- dissoc-children [cache idx-info key]
  (c/let [resolvers (get-in idx-info [::pc/index-attributes key ::pc/attr-input-in])
          children (mapcat #(get-in idx-info [::pc/index-resolvers % ::pc/output])
                           resolvers)
          new-cache (dissoc cache key)]

    (reduce (fn [acc key] (dissoc-children acc idx-info key))
            new-cache children)))

(defn- dissoc-req-cache [req-cache idx-info key]
  (c/let [resolvers (get-in idx-info [::pc/index-attributes key ::pc/attr-input-in])
          children (mapcat #(get-in idx-info [::pc/index-resolvers % ::pc/output])
                           resolvers)

          key-resolvers (get-in idx-info [::pc/index-attributes key ::pc/attr-output-in])
          keys-to-remove (for [res key-resolvers
                               res-key (->> req-cache keys (filter #(-> % first (= res))))]
                           res-key)
          new-req-cache (apply dissoc req-cache keys-to-remove)]
    (reduce (fn [acc new-key]
              (dissoc-req-cache acc idx-info new-key))
            new-req-cache children)))

(defn- sm-dissoc [state k]
  (c/let [old-cache @(:cache state)
          old-req-cache @(:req-cache state)
          idx-info (:idx-info state)]
    (assoc state
           :cache (dissoc-children old-cache idx-info k)
           :req-cache (dissoc-req-cache old-req-cache idx-info k))))

(defn- sm-assoc [state k v]
  (norm-state (update (sm-dissoc state k) :cache assoc k v)))

(defn- get-keys-dependencies [state]
  (-> state :idx-info ::pc/index-oir))

;
; (defn- sm-seq [^js this]
;   (some-> this
;           .-_state
;           :cache
;           deref
;           seq))
;
(defn- sm-find [this k]
  (c/let [not-found (js/Object.)]
    (.. (get this k not-found)
        (then (fn [val]
                (when-not (= not-found val)
                  (MapEntry. k val (hash [k val]))))))))

; (defn- sm-doall [^js this]
;   (c/let [not-found (js/Object.)
;           kvs-prom (->> this
;                         .-_state
;                         get-keys-dependencies
;                         keys
;                         (map (fn [k]
;                                (-> this
;                                    (get k not-found)
;                                    (.then (fn [v] [k v])))))
;                         (js/Promise.all))]
;     (.then kvs-prom (fn [kvs] (remove #(-> % second (= not-found)) kvs)))))

; (defn- sm-reduce [sequence f seed]
;   (reduce (fn [acc-prom [key val]]
;             (.then acc-prom
;                    (fn [acc]
;                      (c/let [entry (MapEntry. key val (hash [key val]))]
;                        (f acc entry)))))
;           (js/Promise.resolve seed)
;           sequence))

(defn- sm-then [old-state fun]
  ; (tap> [:old-state old-state])
  (c/let [cache (:cache old-state)
          value (:final-val old-state (get-in @cache (:cursor old-state)))
          ; _ (prn :cursor (:cursor old-state)
          ;        :from-cache (get-in @cache (:cursor old-state))
          ;        :final-val (:final-val old-state))
          res (fun value)]

    ; (prn :res res
    ;      :promise? (instance? js/Promise res)
    ;      :smart? (instance? SmartMap res))

    (assoc old-state :final-val res)))

(defn- sm-first [^js this]
  #_
  (.__state_then this
         #(prn :STATEA (-> @% :path))))
  ; (c/let [state (.-_state this)]
  ;   (def st state)
  ;   []))

(extend-type SmartMap
  Object
  (equiv [this other] (compare-to this other))
  (then [this fun] (.__state_then this #(sm-then % fun)))

  ILookup
  (-lookup
   ([this k] (.__state_then this #(sm-get % k nil)))
   ([this k not-found] (.__state_then this #(sm-get % k not-found))))
  ; IEquiv
  ; (-equiv [this other] (compare-to this other))
  ;
  IAssociative
  (-assoc [this k v] (.__state_then this #(sm-assoc % k v)))
  (-contains-key? [this k] (.then this #(contains? % k)))

  ICollection
  (-conj [this [k v]] (.__state_then this #(sm-assoc % k v)))

  IMap
  (-dissoc [this k] (.__state_then this #(norm-state (sm-dissoc % k))))

  IEmptyableCollection
  (-empty [this]
    (.__state_then this #(norm-state (assoc %
                                            :cache {}
                                            :req-cache {}
                                            :not-found-cache #{}))))
  ;
  ; IReduce
  ; (-reduce
  ;  ([this f] (-> this sm-doall (.then (fn [[fst & rst]] (sm-reduce rst f fst)))))
  ;  ([this f seed] (-> this sm-doall (.then (fn [seq] (sm-reduce seq f seed))))))
  ;
  ; IKVReduce
  ; (-kv-reduce [this f init]
  ;             (-> this
  ;                 sm-doall
  ;                 (.then #(sm-reduce % (fn [acc [k v]] (f acc k v)) init))))
  ;
  ; ISeqable
  ; (-seq [this] (sm-seq this))
  ;
  ICloneable
  (-clone [this] (.__state_then this #(-> %
                                          (update :cache deref)
                                          (update :req-cache deref)
                                          norm-state)))
  ;
  ; IWithMeta
  ; (-with-meta [this new-meta] (-> (.-_state this)
  ;                                 (assoc :meta new-meta)
  ;                                 ->SmartMap))
  ;
  ; IMeta
  ; (-meta [this] (:meta (.-_state this)))
  ;
  IFind
  (-find [this k] (sm-find this k)))
  ;
  ; ISeq
  ; (-first [this] (sm-first this))
  ; (^clj -rest [coll]))

(defn smart-map
  ([resolvers] (smart-map resolvers {}))
  ([resolvers env]
   (->SmartMap {:env env
                :path []
                :parser (gen-parser resolvers)
                :idx-info (reduce pc/register {} resolvers)})))

; (deftype PersistentVector [meta cnt shift root tail ^:mutable __hash]
;   Object
;   (toString [coll])
;   (equiv [this other])
;   (indexOf [coll x])
;   (indexOf [coll x start])
;   (lastIndexOf [coll x])
;   (lastIndexOf [coll x start])
;
;   ICloneable
;   (-clone [_])
;
;   IWithMeta
;   (-with-meta [coll new-meta])
;
;   IMeta
;   (-meta [coll] meta)
;
;   IStack
;   (-peek [coll])
;   (-pop [coll])
;
;   ICollection
;   (-conj [coll o])
;
;   IEmptyableCollection
;   (-empty [coll])
;
;   ISequential
;   IEquiv
;   (-equiv [coll other])
;
;   IHash
;   (-hash [coll])
;
;   ISeqable
;   (-seq [coll])
;
;   ICounted
;   (-count [coll])
;
;   IIndexed
;   (-nth [coll n])
;   (-nth [coll n not-found])
;
;   ILookup
;   (-lookup [coll k])
;   (-lookup [coll k not-found])
;
;   IAssociative
;   (-assoc [coll k v])
;   (-contains-key? [coll k])
;
;   IFind
;   (-find [coll n])
;
;   APersistentVector
;   IVector
;   (-assoc-n [coll n val])
;
;   IReduce
;   (-reduce [v f])
;   (-reduce [v f init])
;
;   IKVReduce
;
;   IFn
;   (-invoke [coll k])
;   (-invoke [coll k not-found])
;
;   IEditableCollection
;   (-as-transient [coll])
;
;   IReversible
;   (-rseq [coll])
;
;   IIterable
;   (-iterator [this]))
