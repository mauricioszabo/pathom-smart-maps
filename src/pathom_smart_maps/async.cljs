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

(defn- eql [parser env query]
  (js/Promise.
   (fn [resolve reject]
       (async/go
         (try
           (resolve (async/<! (parser env query)))
           (catch :default e
             (reject e)))))))

(def SmartMap (js* "class SmartMap extends Promise {}"))

(defn- norm-cache [cache]
  (atom
   (cond
     (map? cache) cache
     (nil? cache) {}
     (instance? Atom cache) @cache
     :else cache)))

(defn- ->SmartMap [{:keys [cache req-cache not-found-cache env] :as params}]
  (c/let [req-cache (norm-cache req-cache)
          cache (norm-cache cache)
          state (assoc params
                       :env (assoc env
                                   ::p/entity cache
                                   ::p/request-cache req-cache)

                       :cache cache
                       :req-cache req-cache
                       :not-found-cache (or not-found-cache #{}))]
    (if (map? @cache)
      (doto (. SmartMap resolve state)
            (aset "_state" (atom state)))
      (js/Promise.resolve @cache))))

(defn- compare-to [^js this other]
  (if (instance? SmartMap other)
    (= (.-_state this) (.-_state other))
    (= (.-_state this) other)))

(defn- sm-then [^js this fun]
  (c/let [old-state @(.-_state this)
          cache (:cache old-state)
          res (fun @cache)]
    (if (instance? js/Promise res)
      (.then res #(->SmartMap (assoc old-state :cache % :req-cache {})))
      (->SmartMap (assoc old-state :cache res :req-cache {})))))

(extend-protocol Object
  SmartMap
  (equiv [this other] (compare-to this other))
  (then [this fun] (sm-then this fun)))

(extend-protocol IEquiv
  SmartMap
  (-equiv [this other] (compare-to this other)))

(defn- make-query! [state cache k]
  (.then (eql (:parser @state) (:env @state) [k])
    #(do %)))

(defn- sm-get [^js sm k default]
  (c/let [state (.-_state sm)
          {:keys [not-found-cache cache eql]} @state
          cached (find @cache k)]
    (cond
      ;; FIXME: check if this should be a Promise
      cached (js/Promise.resolve (second cached))
      (contains? not-found-cache k) (js/Promise.resolve default)
      :else (.then (make-query! state cache k)
                   (fn [res]
                     (if (#{::p/not-found ::p/reader-error} (get res k ::p/not-found))
                       (swap! state update :not-found-cache conj k)
                       (swap! (:cache @state) merge res))
                     (sm-get sm k default))))))

(extend-protocol ILookup
  SmartMap
  (-lookup
   ([this k] (sm-get this k nil))
   ([this k not-found] (sm-get this k not-found))))

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

(defn- sm-dissoc [^js this k]
  (c/let [state @(.-_state this)
          old-cache @(:cache state)
          old-req-cache @(:req-cache state)
          idx-info (:idx-info state)]
    (assoc state
           :cache (dissoc-children old-cache idx-info k)
           :req-cache (dissoc-req-cache old-req-cache idx-info k))))

(defn- sm-assoc [^js this k v]
  (c/let [new-state (sm-dissoc this k)]
    (->SmartMap (update new-state :cache assoc k v))))

(defn- get-keys-dependencies [state-atom]
  (c/let [state @state-atom]
    (-> state :idx-info ::pc/index-oir)))

(defn- sm-contains? [^js this, k]
  (c/let [state-atom (.-_state this)
          state @state-atom]
    (if (contains? (:not-found-cache state) k)
      false
      (-> state-atom get-keys-dependencies (contains? k)))))

(extend-protocol IAssociative
  SmartMap
  (-assoc [this k v] (sm-assoc this k v))
  (-contains-key? [this k] (sm-contains? this k)))

(extend-protocol IMap
  SmartMap
  (-dissoc [this k] (->SmartMap (sm-dissoc this k))))

(extend-protocol IEmptyableCollection
  SmartMap
  (-empty [this] (c/let [state @(.-_state this)]
                   (->SmartMap (assoc state :cache {} :req-cache {} :not-found-cache #{})))))

(extend-protocol ISeqable
  SmartMap
  (-seq [this] (->> this
                    .-_state
                    get-keys-dependencies
                    keys
                    (map (fn [k]
                           (c/let [entry (get this k)]
                             (MapEntry. k entry (hash [k entry])))))
                    doall)))

(extend-protocol ICloneable
  SmartMap
  (-clone [this] (-> @(.-_state this)
                     (update :cache deref)
                     (update :req-cache deref)
                     ->SmartMap)))

(extend-protocol IWithMeta
  SmartMap
  (-with-meta [this new-meta] (-> @(.-_state this)
                                  (assoc :meta new-meta)
                                  ->SmartMap)))

(extend-protocol IMeta
  SmartMap
  (-meta [this] (:meta @(.-_state this))))

(defn- sm-find [this k]
  (c/let [not-found (js/Object.)]
    (when (-contains-key? this k)
      (.then (get this k not-found) (fn [v]
                                      (if (= not-found v)
                                        nil
                                        (MapEntry. k v (hash [k v]))))))))

(extend-protocol IFind
  SmartMap
  (-find [this k] (sm-find this k)))

; (extend-protocol IChunkedSeq
;   SmartMap
;   (-chunked-first [coll])
;   (-chunked-rest [coll]))
;
(defn- sm-reduce [sequence f seed]
  (reduce (fn [acc-prom [key elem]]
            (.then acc-prom
                   (fn [acc]
                     (.then elem
                            (fn [val]
                              (c/let [entry (MapEntry. key val (hash [key val]))]
                                (f acc entry)))))))
          (js/Promise.resolve seed)
          sequence))

(extend-protocol IReduce
  SmartMap
  (-reduce
   ([this f] (c/let [[fst & rst] (seq this)] (sm-reduce rst f fst)))
   ([this f start] (sm-reduce (seq this) f start))))

(extend-protocol IKVReduce
  SmartMap
  (-kv-reduce [this f init]
              (sm-reduce (seq this)
                         (fn [acc [k v]] (f acc k v))
                         init)))

; (deftype SmartMap [env]
;   ;; ES6
;   (keys [_] (es6-iterator (sm-keys env)))
;   (entries [_] (es6-entries-iterator (seq (p.ent/entity env))))
;   (values [_] (es6-iterator (vals (p.ent/entity env))))
;   (has [_ k] (sm-contains? env k))
;   (get [_ k not-found] (-lookup (p.ent/entity env) k not-found))
;   (forEach [_ f] (doseq [[k v] (p.ent/entity env)] (f v k)))
;
;   ICollection
;   (-conj [coll entry]
;          (if (vector? entry)
;            (-assoc coll (-nth entry 0) (-nth entry 1))
;            (loop [ret coll
;                   es  (seq entry)]
;              (if (nil? es)
;                ret
;                (let [e (first es)]
;                  (if (vector? e)
;                    (recur (-assoc ret (-nth e 0) (-nth e 1))
;                      (next es))
;                    (throw (js/Error. "conj on a map takes map entries or seqables of map entries"))))))))
;

(defn smart-map
  ([resolvers] (smart-map resolvers {}))
  ([resolvers env]
   (->SmartMap {:env env
                :parser (gen-parser resolvers)
                :idx-info (reduce pc/register {} resolvers)})))
