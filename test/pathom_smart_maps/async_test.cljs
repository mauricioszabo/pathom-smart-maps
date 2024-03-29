(ns pathom-smart-maps.async-test
  (:require [pathom-smart-maps.async :as smart]
            [com.wsscode.pathom.connect :as connect]
            [clojure.test :refer [deftest run-tests]]
            [matcher-combinators.matchers :as m]
            [promesa.core :as p]
            [check.async :refer [async-test check testing]]))

(def calls (atom []))
(connect/defresolver root-person [_ _]
  {::connect/input #{}
   ::connect/output [:person/gn :person/sn]}

  (swap! calls conj :root-person)
  {:person/gn "Name" :person/sn "Surname"})

(connect/defresolver full-name [env {:person/keys [gn sn]}]
  {::connect/input #{:person/gn :person/sn}
   ::connect/output [:person/full-name]}

  (swap! calls conj :full-name)
  {:person/full-name (str gn " " sn)})

(connect/defresolver an-error [_ _]
  {::connect/input #{}
   ::connect/output [:error/field]}

  (swap! calls conj :an-error)
  (throw (ex-info "That's an error. That's all" {})))

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

(connect/defresolver children [_ _]
  {::connect/input #{}
   ::connect/output [{:person/children [:person/full-name :person/age]}]}
  {:person/children [{:person/full-name "Child One" :person/age 9}
                     {:person/full-name "Child Two" :person/age 12}]})

(connect/defresolver birthday [_ {:keys [person/age]}]
  {::connect/input #{:person/age}
   ::connect/output [:person/next-age]}

  {:person/next-age (inc age)})

(deftest nested-smart-maps
  (let [smart (smart/smart-map [children birthday])]
    (async-test "will consider some smart-maps results as other smart-maps"
      (testing "captures nested map"
        (check (:person/children smart)
               => (m/equals [{:person/full-name "Child One" :person/age 9}
                             {:person/full-name "Child Two" :person/age 12}])))

      (testing "resolves birthday for the first child only"
        (let [smart (empty smart)]
          (p/do!
           (check (-> smart :person/children first :person/next-age) => 10)
           (check (:person/children smart)
                  => (m/equals [{:person/full-name "Child One"
                                 :person/age 9
                                 :person/next-age 10}
                                {:person/full-name "Child Two"
                                 :person/age 12}]))))))))

(connect/defresolver array-root [_]
  {::connect/output [{:people [:person/name :person/age]}]}
  (js/Promise.resolve
   {:people [{:person/name "One" :person/age 10}
             {:person/name "Two" :person/age 20}
             {:person/name "Three" :person/age 30}]}))

(connect/defresolver array-next [{:keys [person/age]}]
  {::connect/output [:person/next-age]}
  (js/Promise.resolve {:person/next-age (inc age)}))

(deftest calculates-from-nested-array
  (async-test "Calculates nested data from a root array"
    (let [incomplete-map (smart/smart-map [array-root])
          complete-map (smart/smart-map [array-root array-next])]

      (p/do!
       (testing "captures nil for nested elements"
         (-> incomplete-map :people first :person/next-age
             (check => nil)))

       (testing "captures next-age for nested elements"
         (-> complete-map :people (nth 1) :person/next-age
             (check => 21)))))))

(defn- ^:dev/after-load run []
  (run-tests))
