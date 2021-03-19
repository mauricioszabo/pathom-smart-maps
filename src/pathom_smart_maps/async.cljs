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

(def SmartMap (js* "class SmartMap extends Promise {}"))

(defn- norm-cache [cache]
  (atom
   (cond
     (map? cache) cache
     (nil? cache) {}
     (instance? Atom cache) @cache
     :else cache)))

(defn- state-from-params
  [{:keys [cache cursor req-cache not-found-cache env] :as params}]
  (c/let [req-cache (norm-cache req-cache)
          cache (norm-cache cache)]
    (assoc params
           :env (assoc env
                       ::p/entity cache
                       ::p/request-cache req-cache)
           :cursor (or cursor [])
           :cache cache
           :req-cache req-cache
           :not-found-cache (or not-found-cache #{}))))

(defn- ->SmartMap [{:keys [cache cursor req-cache not-found-cache env] :as params}]
  (c/let [req-cache (norm-cache req-cache)
          cache (norm-cache cache)
          state (assoc params
                       :env (assoc env
                                   ::p/entity cache
                                   ::p/request-cache req-cache)
                       :cursor (or cursor [])
                       :cache cache
                       :req-cache req-cache
                       :not-found-cache (or not-found-cache #{}))
          state (atom state)]
    (doto (. SmartMap resolve state)
          (aset "_state" state))))

(defn- compare-to [^js this other]
  (if (instance? SmartMap other)
    (= (.-_state this) (.-_state other))
    (= (.-_state this) other)))

(defn- wrap-result [^js sm [key cached-value]]
  (c/let [state @(.-_state sm)]
    (->SmartMap (update state :cursor conj key))))
    ; (prn :WRAP cached-value (coll? cached-value))
    ; (if (coll? cached-value)
    ;   (->SmartMap (update state :cursor conj key))
    ;   (js/Promise.resolve cached-value))))

(declare sm-get)
; (defn- make-query! [^js sm k default]
;   (c/let [state (.-_state sm)
;           {:keys [parser env]} @state
;           new-sm (doto (. SmartMap resolve state)
;                        (aset "_state" (atom state)))]
;     (.then sm
;            (fn [resolve reject]
;              (prn :AND?)
;              (async/go
;                ;; FIXME - error case
;                (c/let [res (async/<! (parser env [k]))]
;                  (prn :RESULT-FROM-QUERY res)
;                  (if (#{::p/not-found ::p/reader-error} (get res k ::p/not-found))
;                    (swap! state update :not-found-cache conj k)
;                    (swap! (:cache @state) merge res))
;                  (resolve (sm-get sm k default))))))))
(defn- make-query! [^js sm k default]
  (c/let [state (.-_state sm)
          {:keys [parser env]} @state]
          ; new-sm (doto (. SmartMap resolve state)
          ;              (aset "_state" (atom state)))]
    (-> (SmartMap.
         (fn [resolve reject]
           ; (prn :BEFORE-GO)
           (async/go
             ;; FIXME - error case
             (c/let [res (async/<! (parser env [k]))]
               ; (prn :RESULT-FROM-QUERY res)
               (if (#{::p/not-found ::p/reader-error} (get res k ::p/not-found))
                 (swap! state update :not-found-cache conj k)
                 (swap! (:cache @state) merge res))
               ; (prn :WILL-RESOLVE)
               (resolve state)))))
        (doto (aset "_state" state))
        (.then #(do
                  ; (prn :PROMISE-THEN)
                  (sm-get sm k default))))))

(defn- sm-get [^js sm k default]
  (c/let [state (.-_state sm)
          {:keys [not-found-cache cache eql cursor]} @state
          cache (get-in @cache cursor)
          cached (find cache k)]
    (cond
      ;; FIXME: check if this should be a Promise
      cached (wrap-result sm cached)
      (contains? not-found-cache k) default
      :else (make-query! sm k default))))

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

(defn- sm-seq [^js this]
  (c/let [not-found (js/Object.)]
    (->> this
         .-_state
         deref
         :cache
         deref
         seq)))

(defn- sm-find [this k]
  (c/let [not-found (js/Object.)]
    (when (-contains-key? this k)
      (.then (get this k not-found) (fn [v]
                                      (if (= not-found v)
                                        nil
                                        (MapEntry. k v (hash [k v]))))))))

(defn- sm-doall [^js this]
  (c/let [not-found (js/Object.)
          kvs-prom (->> this
                        .-_state
                        get-keys-dependencies
                        keys
                        (map (fn [k]
                               (-> this
                                   (get k not-found)
                                   (.then (fn [v] [k v])))))
                        (js/Promise.all))]
    (.then kvs-prom (fn [kvs] (remove #(-> % second (= not-found)) kvs)))))

(defn- sm-reduce [sequence f seed]
  (reduce (fn [acc-prom [key val]]
            (.then acc-prom
                   (fn [acc]
                     (c/let [entry (MapEntry. key val (hash [key val]))]
                       (f acc entry)))))
          (js/Promise.resolve seed)
          sequence))

(defn- sm-then [old-state fun]
  (c/let [old-state @old-state
          cache (:cache old-state)
          value (:final-val old-state (get-in @cache (:cursor old-state)))]
    (fun value)))

(extend-type SmartMap
  Object
  (equiv [this other] (compare-to this other))
  (then [this fun]
    (c/let [this-state (.-_state this)
            super (.. js/Promise -prototype -then (bind this))
            state-var (atom nil)
            res-super
            (super (fn [state]
                     (reset! state-var state)
                     (sm-then state fun)))
            other-super (.. js/Promise -prototype -then (bind res-super))]
      (other-super (fn [elem]
                     (-> @@state-var
                         (assoc :final-val elem)
                         atom)))))

  IEquiv
  (-equiv [this other] (compare-to this other))

  IAssociative
  (-assoc [this k v] (sm-assoc this k v))
  (-contains-key? [this k] (sm-contains? this k))

  ICollection
  (-conj [this [k v]] (sm-assoc this k v))

  IMap
  (-dissoc [this k] (->SmartMap (sm-dissoc this k)))

  IEmptyableCollection
  (-empty [this] (c/let [state @(.-_state this)]
                   (->SmartMap (assoc state :cache {} :req-cache {} :not-found-cache #{}))))

  IReduce
  (-reduce
   ([this f] (-> this sm-doall (.then (fn [[fst & rst]] (sm-reduce rst f fst)))))
   ([this f seed] (-> this sm-doall (.then (fn [seq] (sm-reduce seq f seed))))))

  IKVReduce
  (-kv-reduce [this f init]
              (-> this
                  sm-doall
                  (.then #(sm-reduce % (fn [acc [k v]] (f acc k v)) init))))

  ISeqable
  (-seq [this] (sm-seq this))

  ICloneable
  (-clone [this] (-> @(.-_state this)
                     (update :cache deref)
                     (update :req-cache deref)
                     ->SmartMap))

  IWithMeta
  (-with-meta [this new-meta] (-> @(.-_state this)
                                  (assoc :meta new-meta)
                                  ->SmartMap))

  IMeta
  (-meta [this] (:meta @(.-_state this)))

  IFind
  (-find [this k] (sm-find this k)))

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
