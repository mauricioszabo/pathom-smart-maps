(ns pathom-smart-maps.async-test
  (:require [pathom-smart-maps.async :as smart]
            [com.wsscode.pathom.connect :as pc]
            [clojure.test :refer [deftest run-tests]]
            [matcher-combinators.matchers :as m]
            [promesa.core :as p]
            [check.async :refer [async-test check testing]]))

(def calls (atom []))
(pc/defresolver root-person [_ _]
  {::pc/input #{}
   ::pc/output [:person/gn :person/sn]}

  (swap! calls conj :root-person)
  {:person/gn "Name" :person/sn "Surname"})

(pc/defresolver full-name [env {:person/keys [gn sn]}]
  {::pc/input #{:person/gn :person/sn}
   ::pc/output [:person/full-name]}

  (swap! calls conj :full-name)
  {:person/full-name (str gn " " sn)})

(pc/defresolver an-error [_ _]
  {::pc/input #{}
   ::pc/output [:error/field]}

  (swap! calls conj :an-error)
  (throw (ex-info "That's an error. That's all" {})))

#_(-> ;smap
      (smart/smart-map [root-person])
      :person/sn
      (.then #(do (prn :FINAL-RES %) (+ % 20))))
      ; (.then #(do (prn :FINAL-RES-2 %) 20))
      ; (.then #(do (prn :FINAL-RES-3 %) 30)))
      ; type)
      ; (do nil))

(def smap (smart/smart-map [root-person]))
#_
(:person/gn smap)

(deftest root-resolver
  (async-test "resolving roots"
    (reset! calls [])
    (let [smap (smart/smart-map [root-person])]
      (check smap => {})

      (testing "gets the attributes, makes a call to EQL"
        (check (:person/gn smap) => "Name")
        (check @calls => [:root-person]))

      (testing "gets the attributes, does not make a call to EQL"
        (check (:person/gn smap) => "Name")
        (check @calls => [:root-person]))

      (testing "don't find unexisting attribute, keep default"
        (check (:person/my-name smap ::name) => ::name)
        (check @calls => [:root-person])))))

#_
(deftest destructuring-test
  (let [smart (smart/smart-map [root-person full-name])]
    (async-test "destructuring fields in promises"
      (smart/let [{:person/keys [gn]} smart]
        (check gn => "Name")))))

#_
(println
 (macroexpand-1
  '(smart/let [{:person/keys [gn]} smap]
     (check gn => "Name"))))

(deftest sub-resolvers
  (let [smart (smart/smart-map [root-person full-name])]
    (reset! calls [])
    (async-test "resolving a data that depends on another"
      (testing "will get the dependent data"
        (check (:person/full-name smart) => "Name Surname")
        (check @calls => [:root-person :full-name]))

      (testing "will cache all information"
        (check smart => {:person/gn "Name"
                         :person/sn "Surname"
                         :person/full-name "Name Surname"})
        (check @calls => [:root-person :full-name]))

      (testing "will dissoc info (and un-cache)"
        (-> smart (dissoc :person/gn) :person/full-name
            (check => "Name Surname"))
        (check @calls => [:root-person :full-name :root-person :full-name]))

      (testing "un-caches results if you assoc a parent data"
        (check (:person/full-name (assoc smart :person/gn "Other"))
               => "Other Surname")
        (check @calls => [:root-person :full-name :root-person :full-name :full-name]))

      (testing "un-caches results if you conj a parent data"
        (check (:person/full-name (conj smart [:person/gn "Another"]))
               => "Another Surname")
        (check @calls => [:root-person :full-name :root-person :full-name
                          :full-name :full-name]))

      (testing "behaves like a normal ClojureScript map"
        (check (empty smart) => {})
        (check (clone smart) => smart)))))

(deftest containing-errors []
  (let [smart (smart/smart-map [root-person an-error])]
    (reset! calls [])
    (async-test "when some resolver gets an error"
      (testing "will check if a key is not part of resolvers"
        (check (-contains-key? smart :inexistent/field) => false)
        (check @calls => []))

      (testing "will capture errors"
        (check (:error/field smart) => nil)
        (check @calls => [:an-error]))

      (testing "will cache errors"
        (check (:error/field smart) => nil)
        (check @calls => [:an-error])
        (check (-contains-key? smart :error/field) => false)))))

(deftest non-resolved-entities
  (let [smart (smart/smart-map [root-person full-name])]
    (async-test "will consider entities that WILL be resolved, but are not right now"
      (testing "implements find correctly"
        (check (find smart :person/invalid) => nil)
        (check (find smart :person/gn) => [:person/gn "Name"]))

      (testing "implements a version of doall"
        (check (smart/doall smart) => {:person/gn "Name"
                                       :person/sn "Surname"
                                       :person/full-name "Name Surname"}))

      (testing "reduce will only resolve on the end"
        (let [mapped (reduce (fn [acc [k v]] (str acc " | " k " - " v))
                             ""
                             (smart/smart-map [root-person]))]
          (check mapped => " | :person/gn - Name | :person/sn - Surname"))

        (let [mapped (reduce-kv (fn [acc k v]
                                  (str acc " | " k " - " v))
                             ""
                             (smart/smart-map [root-person]))]
          (check mapped => " | :person/gn - Name | :person/sn - Surname"))))))

      ; (testing "map will only resolve on the end"
      ;   (let [mapped (map (fn [[k v]] (str k " - " v)) (smart/smart-map [root-person]))]
      ;     (check (p/all mapped) => (m/in-any-order [":person/gn - Name"
      ;                                               ":person/sn - Surname"])))))))

(pc/defresolver children [_ _]
  {::pc/input #{}
   ::pc/output [{:person/children [:person/full-name :person/age]}]}
  {:person/children [{:person/full-name "Child One" :person/age 9}
                     {:person/full-name "Child Two" :person/age 12}]})

(pc/defresolver birthday [_ {:keys [person/age]}]
  {::pc/input #{:person/age}
   ::pc/output [:person/next-age]}

  {:person/next-age (inc age)})

(deftest nested-smart-maps
  (let [smart (smart/smart-map [children birthday])]
    (async-test "will consider some smart-maps results as other smart-maps"
      (testing "captures nested map"
        (check (:person/children smart)
               => (m/equals [{:person/full-name "Child One" :person/age 9}
                             {:person/full-name "Child Two" :person/age 12}])))

      (testing "resolves birthday for the first child only"
        (check (-> smart empty :person/children first :person/next-age) => 10)
        (check (:person/children smart)
               => (m/equals [{:person/full-name "Child One" :person/age 9 :person/next-age 10}
                             {:person/full-name "Child Two" :person/age 12}]))))))
;
; ; #_
(def smart (smart/smart-map [root-person children birthday]))
#_(.then smart prn)
#_
(-> smart
    :person/children
    first
    :person/next-age)
#_
(-> (smart/smart-map [root-person children birthday])
    :person/children)
    ; first)
    ; count)
    ; first
    ; #_:person/next-age)
;
; #_
; (-> smart :person/children #_first #_:person/next-age)
; #_
; (-> smart :person/gn type)
;
; #_
; (type
;  (js/Promise.race #js [(smart/smart-map [root-person children birthday])
;                        (. smart/SmartMapEntry resolve 20)]))
;
; #_
; (type
;  (.. smart/SmartMapEntry
;      (resolve [root-person children birthday])
;      (then (fn [res] (smart/smart-map res)))))
;
; #_
; (-> smart empty :person/children (.then #(count %)))
; #_
; (-> smart :person/children (.then #(count %)))
;
; #_
; (-> smart empty :person/children type)
; #_
; (-> smart :person/children type)
;
(defn- ^:dev/after-load run []
  (run-tests))
