(ns virtualizer.metrics)

(defn create-theme-observer [!theme-metrics !prepared-cache extract-metrics-fn]
  (if-not extract-metrics-fn
    nil
    (js/ResizeObserver.
     (fn [entries]
       (let [new-metrics (reduce extract-metrics-fn @!theme-metrics entries)]
         (when (not= new-metrics @!theme-metrics)
           (js/requestAnimationFrame
            (fn []
              (reset! !prepared-cache {})
              (reset! !theme-metrics new-metrics)))))))))

(defn create-item-observer [!latest-items !container-width !prepared-cache !measured !theme-metrics estimate-fn]
  (js/ResizeObserver.
   (fn [entries]
     (js/requestAnimationFrame
      (fn []
        (let [latest-items    @!latest-items
              container-width @!container-width
              theme-metrics   @!theme-metrics
              measured-cache  @!measured
              updates (reduce
                       (fn [acc entry]
                         (let [el    (.-target entry)
                               id    (.getAttribute el "data-item-id")
                               dom-h (.-height (.-contentRect entry))]
                           (if (and id (pos? dom-h))
                             (let [item             (or (get latest-items id) {:id id})
                                   current-measured (get measured-cache id)
                                   estimated        (estimate-fn item container-width !prepared-cache !measured theme-metrics)
                                   used-h           (or current-measured estimated)
                                   diff             (js/Math.abs (- dom-h used-h))
                                   dom-h-round      (js/Math.round dom-h)]
                               (if (and (> diff 4) (not= current-measured dom-h-round))
                                 (assoc acc id dom-h-round)
                                 acc))
                             acc)))
                       {}
                       entries)]
          (when (seq updates)
            (swap! !measured merge updates))))))))

(defn create-container-observer [!container-width]
  (js/ResizeObserver.
   (fn [entries]
     (js/requestAnimationFrame
      (fn []
        (let [rect (.-contentRect (aget entries 0))]
          (reset! !container-width (.-width rect))))))))
