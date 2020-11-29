(ns pathom-smart-maps.async
  (:require [cljs.core.async :as async]
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
  (let [req-cache (norm-cache req-cache)
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

(extend-protocol Object
  SmartMap
  (equiv [this other] (compare-to this other))

  (then [this fun]
    (let [old-state @(.-_state this)
          cache (:cache old-state)
          res (fun @cache)]
      (if (instance? js/Promise res)
        (.then res #(->SmartMap (assoc old-state :cache % :req-cache {})))
        (->SmartMap (assoc old-state :cache res :req-cache {}))))))

(extend-protocol IEquiv
  SmartMap
  (-equiv [this other] (compare-to this other)))

(defn- make-query! [state cache k]
  ; (prn :MAKING-QUERY-FOR k)
  (.then (eql (:parser @state) (:env @state) [k])
    #(do %)))

(defn- sm-get [^js sm k default]
  (let [state (.-_state sm)
        {:keys [not-found-cache cache eql]} @state
        cached (find @cache k)]
    (cond
      ;; FIXME: check if this should be a Promise
      cached (js/Promise.resolve (second cached))
      (contains? not-found-cache k) (js/Promise.resolve default)
      :else (.then (make-query! state cache k)
                   (fn [res]
                     (if (= ::p/not-found (get res k ::p/not-found))
                       (swap! state update :not-found-cache conj k)
                       (swap! (:cache @state) merge res))
                     (sm-get sm k default))))))

(extend-protocol ILookup
  SmartMap
  (-lookup
   ([this k] (sm-get this k nil))
   ([this k not-found] (sm-get this k not-found))))

(defn- dissoc-children [cache idx-info key]
  (let [resolvers (get-in idx-info [::pc/index-attributes key ::pc/attr-input-in])
        children (mapcat #(get-in idx-info [::pc/index-resolvers % ::pc/output])
                         resolvers)
        new-cache (dissoc cache key)]

    (reduce (fn [acc key] (dissoc-children acc idx-info key))
            new-cache children)))

(defn- dissoc-req-cache [req-cache idx-info key]
  (let [resolvers (get-in idx-info [::pc/index-attributes key ::pc/attr-input-in])
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
  (let [state @(.-_state this)
        old-cache @(:cache state)
        old-req-cache @(:req-cache state)
        idx-info (:idx-info state)]
    (assoc state
           :cache (dissoc-children old-cache idx-info k)
           :req-cache (dissoc-req-cache old-req-cache idx-info k))))

(defn- sm-assoc [^js this k v]
  (let [new-state (sm-dissoc this k)]
        ; state @(.-_state this)
        ; old-cache @(:cache state)
        ; old-req-cache @(:req-cache state)
        ; idx-info (:idx-info state)
        ; new-cache (-> old-cache
        ;               (dissoc-children idx-info k))
        ;
        ;
        ; new-req-cache (dissoc-req-cache old-req-cache idx-info k)]
    (->SmartMap (update new-state :cache assoc k v))))

(defn- sm-contains? [^js this, k]
  (let [state @(.-_state this)]
    (-> state :idx-info ::pc/index-oir (contains? k))))

(extend-protocol IAssociative
  SmartMap
  (-assoc [this k v] (sm-assoc this k v))
  (-contains-key? [this k] (sm-contains? this k)))

(extend-protocol IMap
  SmartMap
  (-dissoc [this k] (->SmartMap (sm-dissoc this k))))

; (deftype SmartMap [env]
;   ;; ES6
;   (keys [_] (es6-iterator (sm-keys env)))
;   (entries [_] (es6-entries-iterator (seq (p.ent/entity env))))
;   (values [_] (es6-iterator (vals (p.ent/entity env))))
;   (has [_ k] (sm-contains? env k))
;   (get [_ k not-found] (-lookup (p.ent/entity env) k not-found))
;   (forEach [_ f] (doseq [[k v] (p.ent/entity env)] (f v k)))
;
;   ICloneable
;   (-clone [_] (smart-map env (p.ent/entity env)))
;
;   IWithMeta
;   (-with-meta [_ new-meta] (sm-with-meta env new-meta))
;
;   IMeta
;   (-meta [_] (sm-meta env))
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
;   IEmptyableCollection
;   (-empty [_] (sm-empty env))
;
;   IHash
;   (-hash [_] (hash (p.ent/entity env)))
;
;   ISeqable
;   (-seq [_]
;         (some->> (seq (sm-keys env))
;                  (map #(SmartMapEntry. env %))))
;
;   ICounted
;   (-count [_] (count (p.ent/entity env)))
;
;   IAssociative
;   (-assoc [_ k v] (sm-assoc env k v))
;   (-contains-key? [_ k] (sm-contains? env k))
;
;   IFind
;   (-find [_ k] (sm-find env k))
;
;   IMap
;   (-dissoc [_ k] (sm-dissoc env k))
;
;   IKVReduce
;   (-kv-reduce [_ f init]
;               (reduce-kv (fn [cur k v] (f cur k (wrap-smart-map env v))) init (p.ent/entity env)))
;
;   IIterable
;   (-iterator [this]
;              (transformer-iterator (map #(SmartMapEntry. env %))
;                                    (-iterator (sm-keys env)) false))
;
;   IReduce
;   (-reduce [coll f] (iter-reduce coll f))
;   (-reduce [coll f start] (iter-reduce coll f start))
;
;   IFn
;   (-invoke [_ k] (sm-get env k))
;   (-invoke [_ k not-found] (sm-get env k not-found))
;
;   IPrintWithWriter
;   (-pr-writer [_ writer opts]
;               (-pr-writer (p.ent/entity env) writer opts)))

(defn smart-map
  ([resolvers] (smart-map resolvers {}))
  ([resolvers env]
   (->SmartMap {:env env
                :parser (gen-parser resolvers)
                :idx-info (reduce pc/register {} resolvers)})))
