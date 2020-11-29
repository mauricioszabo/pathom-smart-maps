(ns pathom-smart-maps.async-test
  (:require [pathom-smart-maps.async :as smart]
            [com.wsscode.pathom.connect :as pc]
            [clojure.test :refer [deftest run-tests]]
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

(deftest root-resolver
  (async-test "resolving roots"
    (let [map (smart/smart-map [root-person])]
      (check map => {})
      (check (:person/gn map) => "Name"))))

#_
(do
  (def smart (smart/smart-map [root-person full-name]))
  (p/let [{:keys [person/full-name]} smart]
    full-name))

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
                          :root-person :full-name])))))

(defn- ^:dev/after-load run []
  (run-tests))
