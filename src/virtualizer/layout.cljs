(ns virtualizer.layout)

(defn binary-search-start-index [positioned target-top]
  (let [cnt (count positioned)]
    (loop [low 0
           high (dec cnt)]
      (if (<= low high)
        (let [mid      (quot (+ low high) 2)
              item     (nth positioned mid)
              item-top (+ (:bottom item) (:height item))]
          (if (< item-top target-top)
            (recur (inc mid) high)
            (recur low (dec mid))))
        low))))


(defn calculate-layout [items width !prepared-cache measured-heights theme !estimate-fn-atom !measured-atom]
  (let [estimate-fn @!estimate-fn-atom
        events      (if (vector? items) (rseq items) (reverse items))
        events-seq  (seq events)]
    (loop [evs   events-seq
           total 0
           acc   (transient [])]
      (if evs
        (let [msg (first evs)
              id  (:id msg)
              h   (or (get measured-heights id)
                      (estimate-fn msg width !prepared-cache !measured-atom theme))]
          (recur (next evs)
                 (+ total h)
                 (conj! acc {:id id :bottom total :height h})))
        {:total total :items (persistent! acc)}))))
