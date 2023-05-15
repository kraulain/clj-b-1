(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error 
  "prints out an error message with optional args" 
  [& args] 
  (apply println args)
  :error)

;;recipes for how to bake various types items
;;could be expanded by adding more recipes maps
(def baking  {:recipes {:cake {:ingredients {:egg   2
                                             :flour 2
                                             :sugar 1
                                             :milk  1}
                               :steps [[:add :all]
                                       [:mix]
                                       [:pour]
                                       [:bake 25]
                                       [:cool]]}
                        :cookies {:ingredients {:egg 1
                                                :flour 1
                                                :butter 1
                                                :sugar 1}
                                  :steps [[:add :all]
                                          [:mix]
                                          [:pour]
                                          [:bake 30]
                                          [:cool]]}
                        :brownies {:ingredients {:egg 2
                                                 :flour 2
                                                 :butter 2
                                                 :cocoa 2
                                                 :sugar 1
                                                 :milk 1}
                                   :steps [[:add :butter]
                                           [:add :cocoa]
                                           [:add :sugar]
                                           [:mix]
                                           [:add :egg]
                                           [:add :flour]
                                           [:add :milk]
                                           [:mix]
                                           [:pour]
                                           [:bake 35]
                                           [:cool]]}}
              :ingredients {:egg {:storage :fridge
                                  :usage :squeezed}
                            :milk {:storage :fridge
                                   :usage :scooped}
                            :flour {:storage :pantry
                                    :usage :scooped}
                            :butter {:storage :fridge
                                     :usage :simple}
                            :sugar {:storage :pantry
                                    :usage :scooped}
                            :cocoa {:storage :pantry
                                    :usage :scooped}}})


;;Data used by dispatch function to know how to handle each type of ingredient
(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))
            :scooped (fn [ingredient amount]
                       (grab :cup)
                       (dotimes [i amount]
                         (scoop ingredient)
                         (add-to-bowl))
                       (release))})


(defn usage-type
  "Gets usage type for an ingredient out of the recipe map"
  [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :usage)))


(defn add
  "Adds a given number of ingredients to the mixing bowl"
  ([ingredient]
   (add ingredient 1))
  ([ingredient amount]
   (let [ingredient-type (usage-type ingredient)]
     (if (contains? usage ingredient-type)
       (let [f (get usage ingredient-type)]
         (f ingredient amount))
       (error "I do not know the ingredient" ingredient)))))


;;Map off actions one can perform when cooking a recipe
(def actions {:cool (fn [ingredients step]
                      (cool-pan))
              :mix  (fn [ingredients step]
                      (mix))
              :pour (fn [ingredients step]
                      (pour-into-pan))
              :bake (fn [ingredients step]
                      (bake-pan (second step)))
              :add  (fn [ingredients step]
                      (cond
                        (and (= 2 (count step))
                             (= :all (second step)))
                        (doseq [kv ingredients]
                          (add (first kv) (second kv)))
                        (and (= 2 (count step))
                             (contains? ingredients (second step)))
                        (add (second step) (get ingredients (second step)))
                        (= 3 (count step))
                        (add (second step) (get step 2))
                        :else
                        (error "I don't know how to add" (second step) (get step 2))))})


(defn perform 
  "apply the actions defined at each step for the ingredients"
  [ingredients step]
  (let [f (get actions (first step) (fn [ingredients step]
                                      (println "I do not know how to" (first step))))]
    (f ingredients step)))


(defn bake-recipe 
  "Takes a recipe and calls perform to apply each step"
  [recipe]
  (last
    (for [step (get recipe :steps)]
      (perform (get recipe :ingredients) step))))


(defn load-up-amount 
  "Loads up the amount of ingredients into the bowl"
  [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))


(defn unload-amount 
  "Fuction to upload an amount of ingredients from the bowl"
  [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))


(defn fetch-ingredient
  "Recursively fetches a given amount of ingredients from the appropriate storage"
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (let [ingredients (get baking :ingredients)
         info (get ingredients ingredient)]
     (if (contains? ingredients ingredient)
       (do
         (go-to (get info :storage))
         (load-up-amount ingredient amount)
         (go-to :prep-area)
         (unload-amount ingredient amount))
       (error "I don't know the ingredient" ingredient)))))


(defn storage-location 
  "returns the type of storage where he given ingredient is found"
  [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :storage)))


(defn fetch-list 
  "given a list of ingredients, fetch the right amount of each ingredient"
  [shopping]
  (let [by-location (group-by (fn [item-amount]
                                (storage-location (first item-amount)))
                              shopping)]
    (doseq [loc by-location]
      (go-to (first loc))
      (doseq [item-amount (second loc)]
        (load-up-amount (first item-amount) (second item-amount)))))
  (go-to :prep-area)
  (doseq [item-amount shopping]
    (unload-amount (first item-amount) (second item-amount))))


(defn add-ingredients 
  "Combine two lists of ingredients by adding their amounts"
  [a b]
  (merge-with + a b))


(defn multiply-ingredients 
  "multiple the amounts of each ingredients required for an order, by the number of items ordered"
  [n ingredients]
  (into {}
    (for [kv ingredients]
      [(first kv) (* n (second kv))])))


(defn order->ingredients 
  "takes an order and returns the total of all ingredients needed to make the order"
  [order]
  (let [recipes (get baking :recipes)
        items (get order :items)]
    (reduce add-ingredients {}
            (for [kv items]
              (let [recipe (get recipes (first kv))
                    ingredients (get recipe :ingredients)]
                (multiply-ingredients (second kv) ingredients))))))


(defn orders->ingredients 
  "computes the total ingredients of all the orders in a given list of orders"
  [orders]
  (reduce add-ingredients {}
    (for [order orders]
      (order->ingredients order))))



(defn bake [item]
  (let [recipes (get baking :recipes)]
    (bake-recipe (get recipes item))))


(defn day-at-the-bakery 
  "get all items from storage in a single trip then bake all of them"
  []
  (let [orders (get-morning-orders-day3)
        ingredients (orders->ingredients orders)]
    (fetch-list ingredients)
    (doseq [order orders]
      (let [items (get order :items)
            racks (for [kv items
                        i (range (second kv))]
                    (bake (first kv)))
            receipt {:orderid (get order :orderid)
                     :address (get order :address)
                     :rackids racks}]
        (delivery receipt)))))

(defn -main []
  (day-at-the-bakery))
