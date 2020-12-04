(ns pathom-smart-maps.async
  (:refer-clojure :exclude [let])
  (:require [clojure.core :as core]
            [cljs.core :as cljs]))

(defn- accumulate [[bind val & rest] body]
  (if (nil? bind)
    (cons 'do body)

    (core/let [bind-elem [bind val]
               destr (cljs/destructure bind-elem)]
      (if (= destr bind-elem)
        (list '.then (list 'js/Promise.resolve val)
              (list `fn [bind] (accumulate rest body)))

        (core/let [new-symbols (->> bind
                                    (tree-seq coll? seq)
                                    (filter symbol?)
                                    (map (comp symbol name))
                                    (mapcat #(vector % %)))]
          `(core/let ~destr
            ~(accumulate (concat new-symbols rest) body)))))))

(defmacro let [bindings & body]
  (accumulate bindings body))
