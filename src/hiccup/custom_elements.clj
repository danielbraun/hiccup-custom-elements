(ns hiccup.custom-elements
  (:require [clojure [string :as string] walk]
            hiccup.compiler))

(defn- classes [m]
  (->> m
       (filter second)
       (map (comp #(cond->> %
                     (not (vector? %)) vector
                     true (remove nil?)
                     true (map (fn [x]
                                 (try (name x)
                                      (catch Exception e x))))
                     true (string/join "-"))
                  first))
       (string/join " ")))

(defmulti render-element (fn [el attrs children] el))

(defn unpack-custom-element [form]
  "Should be used within a pre-walk"
  (if-let [element (try (hiccup.compiler/normalize-element form)
                        (catch Exception e nil))]
    (if (and (vector? form)
             (keyword? (first form))
             (namespace (first form)))
      (apply render-element (-> element
                                (assoc 0 (keyword (namespace (first form))
                                                  (first element)))))
      form)
    form))

(defn compile-classes [form]
  (if (and (vector? form)
           (keyword? (first form))
           (map? (get-in form [1 :class])))
    (update-in form [1 :class] classes)
    form))

(do
  (def unpack-custom-elements
    (partial clojure.walk/prewalk
             (comp compile-classes
                   unpack-custom-element)))

  (defmethod render-element ::test [_ _ _]
    [:h1 {:class {:green true}} "Hello World"])
  (-> [:body {:class "green"}
       [::test]
       [:h1 {:class {:white true
                     [:x :y] true}}
        [:span {:id 3
                :style "HEYHEHE"
                :class "doh"}]]]
      unpack-custom-elements))


