(ns pathom-smart-maps.async
  (:refer-clojure :exclude [let doall])
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
           :cache cache
           :req-cache req-cache
           :not-found-cache (atom (or not-found-cache #{})))))

; (defn- wrap-result [^js sm [key cached-value]]
;   (c/let [state (.-_state sm)]
;     (->SmartMap (update state :cursor conj key))))

#_
(js/Promise.
 (fn [resolve]
   (async/go
     (resolve (async/<! (parser env [{[{:person/full-name "Child One" :person/age 9}]
                                      [:person/next-age]}]))))))

#_
(:cursor state)
#_
@(:cache state)
#_
(update-in {} [0] merge {:foo 10})

(defn- merge-parent! [parent-cache result]
  (c/let [cursor (:cursor parent-cache)
          parent-state (:state parent-cache)
          parent-cache (:cache parent-state)
          parent-result (swap! parent-cache assoc cursor result)]
    (prn :MERGING cursor)
    (when-let [grandparent-cache (:parent-cache parent-state)]
      (merge-parent! grandparent-cache parent-result))))

(defn- merge-result! [current-state result]
  (c/let [new-result (swap! (:cache current-state) merge result)]
    (when-let [parent-cache (:parent-cache current-state)]
      (merge-parent! parent-cache new-result))))

(declare sm-get)
(defn- make-query! [state k default]
  (c/let [{:keys [parser env]} state]
    (def parser parser)
    (def env env)
    (-> (js/Promise.
         (fn [resolve reject]
           (async/go
             (try
               (c/let [res (async/<! (parser env [k]))]
                 (if (#{::p/not-found ::p/reader-error} (get res k ::p/not-found))
                   (swap! (:not-found-cache state) conj k)
                   (merge-result! state res))
                 (resolve (sm-get state k default)))
               (catch :default e (reject e)))))))))

(defn- sm-get [state k default]
  (c/let [{:keys [not-found-cache cache eql cursor]} state
          [cached-key cached-val] (find @cache k)]
    ; (when cached-key
    ;   (prn :GETTING-CACHE cached-key (coll? cached-val)))
    (def state state)
    (cond
      cached-key (if (coll? cached-val)
                   (do
                     (tap> [:parent (-> state :parent-cache) :state state])
                     (norm-state (assoc state
                                        :cache (get @cache cached-key)
                                        :parent-cache {:state state :cursor cached-key})))
                   (assoc state :final-val cached-val))
      (contains? @not-found-cache k) (assoc state :final-val default)
      :else (make-query! state k default))))

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

(defn- sm-find [this k]
  (c/let [not-found (js/Object.)]
    (.. (get this k not-found)
        (then (fn [val]
                (when-not (= not-found val)
                  (MapEntry. k val (hash [k val]))))))))

(defn doall
  "Resolves everything that this smart map needs to have resolved. Will return the same
SmartMap (promise) that will have all fields resolved. Please note that as SmartMap is
also a promise, you'll need to use .then to access the results."
  [^js smart-map]
  (.__state_then smart-map
                 (fn [state]
                   (.then
                    (->> state
                         get-keys-dependencies
                         keys
                         (map (fn [k] (get smart-map k)))
                         (js/Promise.all))
                    (constantly state)))))

(defn- sm-then [state fun]
  (c/let [value (:final-val state @(:cache state))
          res (fun value)]

    (assoc state :final-val res)))

(extend-type SmartMap
  Object
  (then [this fun] (.__state_then this #(sm-then % fun)))

  ILookup
  (-lookup
   ([this k] (.__state_then this #(sm-get % k nil)))
   ([this k not-found] (.__state_then this #(sm-get % k not-found))))

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

  IReduce
  (-reduce
   ([this f] (-> this doall (.then #(reduce f %))))
   ([this f seed] (-> this doall (.then #(reduce f seed %)))))

  IKVReduce
  (-kv-reduce [this f init] (-> this doall (.then #(reduce-kv f init %))))
  ;
  ; ISeqable
  ; (-seq [this] (sm-seq this))
  ;
  ICloneable
  (-clone [this] (.__state_then this #(-> %
                                          (update :cache deref)
                                          (update :req-cache deref)
                                          norm-state)))

  ; IWithMeta
  ; (-with-meta [this new-meta] ())
  ;
  ; IMeta
  ; (-meta [this] (:meta (.-_state this)))

  IFind
  (-find [this k] (sm-find this k))

  ISeq
  (-first [this] (-nth this 0))
  (^clj -rest [this] (.then this rest))

  ICounted
  (-count [this] (.then this count))

  IIndexed
  (-nth
   ([this n] (-nth this 0 nil))
   ([this n not-found] (.__state_then this #(sm-get % n not-found)))))

(defn smart-map
  ([resolvers] (smart-map resolvers {}))
  ([resolvers env]
   (. SmartMap resolve (norm-state {:env env
                                    :path []
                                    :parser (gen-parser resolvers)
                                    :idx-info (reduce pc/register {} resolvers)}))))

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
  ; ICounted
  ; (-count [this] (.then this count)))
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
