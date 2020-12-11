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

(deftest root-resolver
  (async-test "resolving roots"
    (let [map (smart/smart-map [root-person])]
      (check map => {})
      (check (:person/gn map) => "Name"))))

(deftest destructuring-test
  (let [smart (smart/smart-map [root-person full-name])]
    (async-test "destructuring fields in promises"
      (smart/let [{:person/keys [gn]} smart]
        (check gn => "Name")))))

#_
(do
  (def smart (smart/smart-map [root-person full-name]))
  (select-keys smart [:person/gn :person/full-name])
  (seq smart)
  ; (p/do!)
  (:person/gn smart))
  ; (count smart))

(deftest sub-resolvers
  (let [smart (smart/smart-map [root-person full-name])]
    (reset! calls [])
    (async-test "resolving a data that depends on another"
      (testing "will get the dependent data"
        (check (:person/full-name smart) => "Name Surname")
        (check @calls => [:root-person :full-name])

       (testing "will cache all information"
         (check smart => {:person/gn "Name"
                          :person/sn "Surname"
                          :person/full-name "Name Surname"})
         (check @calls => [:root-person :full-name])))

      (testing "un-cache results if you assoc a parent data"
        (check (:person/full-name (assoc smart :person/gn "Other"))
               => "Other Surname")
        (check @calls => [:root-person :full-name :full-name]))

      (testing "un-cache results if you dissoc some data"
        (check (:person/full-name (dissoc smart :person/full-name))
               => "Name Surname")
        (check @calls => [:root-person :full-name :full-name :full-name])

        (check (:person/full-name (dissoc smart :person/gn))
               => "Name Surname")
        (check @calls => [:root-person :full-name :full-name :full-name
                          :root-person :full-name]))

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

(deftest assoc-ing
  (let [smart (-> [root-person full-name]
                  smart/smart-map
                  (assoc :person/gn "pre-added"))]
    (async-test "will cache some of the data into the pipeline"
      (check (:person/full-name smart) => "pre-added Surname"))))

(deftest non-resolved-entities
  (let [smart (smart/smart-map [root-person full-name])]
    (async-test "will consider entities that WILL be resolved, but are not right now"
      (check (seq smart) => (m/embeds [[:person/gn #(instance? js/Promise %)]]))
      (check (vals smart) => [#(instance? js/Promise %)
                              #(instance? js/Promise %)
                              #(instance? js/Promise %)])
      (check (count smart) => 3))))

(defn- ^:dev/after-load run []
  (run-tests))
