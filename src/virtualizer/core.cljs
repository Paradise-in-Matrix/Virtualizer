(ns virtualizer.core
  (:require [reagent.core :as r]
            [virtualizer.metrics :as metrics]
            [virtualizer.layout :as layout]
            [virtualizer.scroll :as scroll]))

(defn virtualized-list [initial-props]
  (let [!scroll-ref          (atom nil)
        !container-width     (r/atom 400)
        !measured            (r/atom {})
        !latest-items        (atom {})
        !theme-metrics       (r/atom (:default-theme-metrics initial-props {}))
        !prepared-cache      (atom {})
        !current-estimate-fn (atom (:estimate-fn initial-props))
        scroll-state         (scroll/create-scroll-state)
        !protect-scroll?     (atom false)
        !last-item-count     (atom 0)
        wheel-handler        (fn [e] (when @!protect-scroll? (.preventDefault e)))

        metrics-obs          (when (:extract-metrics-fn initial-props)
                               (metrics/create-theme-observer !theme-metrics !prepared-cache (:extract-metrics-fn initial-props)))

        item-resize-obs      (metrics/create-item-observer
                              !latest-items !container-width !prepared-cache !measured !theme-metrics
                              (fn [& args] (apply @!current-estimate-fn args)))

        container-obs        (metrics/create-container-observer !container-width)]

    (r/create-class
     {:component-did-update
      (fn [this]
        (when-let [el @!scroll-ref]
          (scroll/apply-scroll-anchoring el scroll-state)
          (when @!protect-scroll?
            (js/setTimeout #(reset! !protect-scroll? false) 150))))

      :component-will-unmount
      (fn [this]
        (when-let [el @!scroll-ref]
          (.removeEventListener el "wheel" wheel-handler)
          (.removeEventListener el "touchmove" wheel-handler))
        (.disconnect container-obs)
        (.disconnect item-resize-obs)
        (when metrics-obs (.disconnect metrics-obs)))

      :reagent-render
      (fn [{:keys [items items-map loading-older? loading-newer?
                   older-dead? jump-target-id focus-mode? estimate-fn
                   on-load-older on-load-newer on-jump-live
                   render-item render-measuring-sticks render-empty-state
                   render-jump-button render-loading-overlay
                   wrapper-class scroll-container-class]
            :or {wrapper-class          "virtual-list-wrapper"
                 scroll-container-class "virtual-list-scroll-container"}}]

        (reset! !latest-items items-map)
        (reset! !current-estimate-fn estimate-fn)

        (let [width          @!container-width
              theme          @!theme-metrics
              measured-cache @!measured
              current-count  (count items)]
          (when (> current-count @!last-item-count)
            (reset! !protect-scroll? true))
          (reset! !last-item-count current-count)

          (let [layout-data    @(r/track layout/calculate-layout items width !prepared-cache measured-cache theme !current-estimate-fn !measured)
                total-height   (:total layout-data)
                positioned     (:items layout-data)
                cnt            (count positioned)

                dist-bottom    @(:!dist-bottom scroll-state)
                vh             (if-let [el @!scroll-ref] (.-clientHeight el) 800)
                overscan       2500
                w-start        (- dist-bottom overscan)
                w-end          (+ dist-bottom vh overscan)

                start-idx      (layout/binary-search-start-index positioned w-start)
                safe-start     (min cnt start-idx)

                visible-window (->> (subvec positioned safe-start)
                                    (take-while #(<= (:bottom %) w-end)))

                do-jump!       (fn []
                                 (reset! (:!at-bottom? scroll-state) true)
                                 (if focus-mode?
                                   (do
                                     (reset! (:!initialized? scroll-state) false)
                                     (when on-jump-live (on-jump-live)))
                                   (when-let [el @!scroll-ref]
                                     (js/requestAnimationFrame
                                      (fn [] (set! (.-scrollTop el) 0))))))]

            (reset! (:!current-height scroll-state) total-height)
            (reset! (:!current-positioned scroll-state) positioned)
            (reset! (:!current-focus scroll-state) focus-mode?)
            (reset! (:!current-was-loading-fwd scroll-state) @(:!prev-loading-fwd scroll-state))
            (reset! (:!prev-loading-fwd scroll-state) loading-newer?)

            (when (and @!scroll-ref (pos? cnt) (not @(:!initialized? scroll-state)))
              (js/requestAnimationFrame
               (fn []
                 (let [el @!scroll-ref]
                   (when el
                     (if jump-target-id
                       (let [target-item (some #(when (= (:id %) jump-target-id) %) positioned)]
                         (if target-item
                           (set! (.-scrollTop el) (- (:bottom target-item)))
                           (set! (.-scrollTop el) 0)))
                       (set! (.-scrollTop el) 0))
                     (reset! (:!initialized? scroll-state) true)
                     (reset! (:!at-bottom? scroll-state) (not jump-target-id)))))))

            (when (and @!scroll-ref
                       @(:!initialized? scroll-state)
                       (pos? cnt)
                       (not loading-older?)
                       (not older-dead?)
                       (< total-height (.-clientHeight @!scroll-ref)))
              (js/requestAnimationFrame
               (fn []
                 (when on-load-older (on-load-older)))))

            [:div {:class wrapper-class :style {:min-height "0" :display "flex" :flex-direction "column"}}
             (when render-measuring-sticks
               (render-measuring-sticks (fn [el] (when (and el metrics-obs) (.observe metrics-obs el)))))

             [:div {:class (str scroll-container-class (when jump-target-id " jumping-animation"))
                    :ref (fn [el]
                           (let [old-el @!scroll-ref]
                             (when (not= old-el el)
                               (when old-el
                                 (.disconnect container-obs)
                                 (.removeEventListener old-el "wheel" wheel-handler)
                                 (.removeEventListener old-el "touchmove" wheel-handler))
                               (reset! !scroll-ref el)
                               (when el
                                 (.observe container-obs el)
                                 (.addEventListener el "wheel" wheel-handler #js {:passive false})
                                 (.addEventListener el "touchmove" wheel-handler #js {:passive false})))))
                    :style {:overflow-anchor "none" :overflow-y "auto" :min-height "0"}
                    :on-scroll (fn [e]
                                 (scroll/evaluate-scroll-position!
                                  (.-currentTarget e) scroll-state
                                  {:on-load-older    on-load-older
                                   :on-load-newer    on-load-newer
                                   :loading-older?   loading-older?
                                   :older-dead?      older-dead?
                                   :focus-mode?      focus-mode?
                                   :loading-newer?   loading-newer?
                                   :has-items?       (pos? cnt)
                                   :initialized?     @(:!initialized? scroll-state)}))}

              (when loading-older?
                [:div {:style {:position "absolute" :top "10px" :left 0 :right 0 :z-index 10 :display "flex" :justify-content "center" :pointer-events "none"}}
                 (if render-loading-overlay (render-loading-overlay) "Loading...")])

              (if (zero? cnt)
                (when render-empty-state (render-empty-state))
                (let [first-vis  (first visible-window)
                      last-vis   (last visible-window)
                      bottom-gap (if first-vis (:bottom first-vis) total-height)
                      top-gap    (if last-vis (max 0 (- total-height (+ (:bottom last-vis) (:height last-vis)))) 0)]
                  (into [:<>]
                        (concat
                         [^{:key "bottom-gap"}
                          [:div {:style {:height (str bottom-gap "px") :flex-shrink 0 :width "100%"}}]]

                         (for [{:keys [id height] :as item} visible-window]
                           ^{:key id}
                           [:div {:style {:min-height (str height "px") :width "100%" :margin "0" :box-sizing "border-box"}}
                            [:div {:data-item-id id
                                   :ref (fn [el] (when el (.observe item-resize-obs el)))
                                   :class (when (= id jump-target-id) "is-jump-target")}
                             (render-item item)]])
                         [^{:key "top-gap"}
                          [:div {:style {:height (str top-gap "px") :overflow-anchor "none" :flex-shrink 0 :width "100%"}}]]))))

              (when (and focus-mode? loading-newer?)
                [:div {:style {:position "absolute" :bottom "10px" :left 0 :right 0 :z-index 10 :display "flex" :justify-content "center" :pointer-events "none"}}
                 (if render-loading-overlay (render-loading-overlay) "Loading...")])]

             (when (and render-jump-button (or @(:!show-jump? scroll-state) (not @(:!at-bottom? scroll-state))))
               (render-jump-button do-jump! focus-mode?))])))})))

