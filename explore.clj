(ns navigator.crawlers.parser.explore
  "Functions for exploring parse results")


(defn summarize
  "Summarise the results of a parse"
  [results]
  (let [products   (:products results)
        windows    (distinct (map :window products))
        promotions (count (filter :on-promotion products))
        per-window (reduce (fn [acc p]
                             (let [k (:window p)
                                   d {:products   1
                                      :promotions (if (:on-promotion p)
                                                    1
                                                    0)}]

                               (update acc k (partial merge-with +) d)))
                           {}
                           products)]

    (-> results
        (update :products count)
        (update :categories count)
        (update :failures count)
        (update :errors count)
        (assoc :windows windows)
        (assoc :promotions promotions)
        (assoc :per-window per-window))))


(defn paginate-products [results page per-page]
  (let [products (sort-by :id (:products results))]

    (->> products
         (drop (* page per-page))
         (take per-page))))


(defn find-product [results id]
  (->> (:products results)
       (filter (comp #{id} :id))))
