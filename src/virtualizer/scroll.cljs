(ns virtualizer.scroll
  (:require [taoensso.timbre :as log]
            [reagent.core :as r]
            [clojure.string :as str]
            [virtualizer.layout :as layout]))

(defn create-scroll-state []
  {:!anchor                   (atom {:id nil :offset 0 :anchor-bottom 0 :total-height 0})
   :!current-height           (atom 0)
   :!current-positioned       (atom [])
   :!current-focus            (atom false)
   :!current-was-loading-fwd  (atom false)
   :!prev-loading-fwd         (atom false)
   :!dist-bottom              (r/atom 0)
   :!at-bottom?               (r/atom true)
   :!show-jump?               (r/atom false)
   :!initialized?             (r/atom false)
   :!scroll-timer             (atom nil)})

(defn get-dist [target-el]
  (let [st    (js/Math.round (.-scrollTop target-el))
        max-s (- (.-scrollHeight target-el) (.-clientHeight target-el))]
    (if (<= st 0)
      (js/Math.abs st)
      (if (> st (/ max-s 2))
        (max 0 (- max-s st))
        0))))

(defn apply-scroll-anchoring [el state]
  (let [{:keys [!anchor !current-height !current-positioned !current-focus !current-was-loading-fwd]} state
        anchor-val   @!anchor
        total-height @!current-height
        positioned   @!current-positioned
        focus-mode?  @!current-focus
        was-loading? @!current-was-loading-fwd]

    (when (not= total-height (:total-height anchor-val))
      (let [old-anchor-item (some #(when (= (:id %) (:id anchor-val)) %) positioned)]
        (if old-anchor-item
          (let [expected-dist (+ (:bottom old-anchor-item) (:offset anchor-val))
                sync-dist     (get-dist el)
                sync-err      (- sync-dist expected-dist)]
            (when-not (<= (js/Math.abs sync-err) 1.5)
              (when-not (and (<= sync-dist 5) (not was-loading?) (not focus-mode?))
                (set! (.-scrollTop el) (+ (.-scrollTop el) sync-err)))))
          (log/warn "--> JS ANCHOR LOST: Target Anchor ID unmounted by virtualizer. Re-acquiring..."))))

    (let [dist-after-shift (get-dist el)
          idx              (layout/binary-search-start-index positioned (inc dist-after-shift))
          valid-anchors    (drop idx positioned)
          new-anchor       (or (first (remove #(let [id (str (:id %))]
                                                 (or (clojure.string/starts-with? id "virtual-")
                                                     (= id "read-marker")))
                                              valid-anchors))
                               (first valid-anchors))]
      (when new-anchor
        (reset! !anchor
                {:id            (:id new-anchor)
                 :offset        (- dist-after-shift (:bottom new-anchor))
                 :anchor-bottom (:bottom new-anchor)
                 :total-height  total-height})))))

(defn evaluate-scroll-position! [target scroll-state ctx]
  (let [scroll-top       (js/Math.round (.-scrollTop target))
        max-scroll       (- (.-scrollHeight target) (.-clientHeight target))
        dist-from-bottom (if (<= scroll-top 0)
                           (js/Math.abs scroll-top)
                           (if (> scroll-top (/ max-scroll 2))
                             (max 0 (- max-scroll scroll-top))
                             0))
        dist-from-top    (max 0 (- max-scroll dist-from-bottom))
        at-bottom?       (<= dist-from-bottom 30)

        {:keys [!dist-bottom !at-bottom? !scroll-timer !show-jump?]} scroll-state
        {:keys [on-load-older on-load-newer loading-older? older-dead? focus-mode? loading-newer? has-items? initialized?]} ctx]

    (when (not= @!dist-bottom dist-from-bottom)
      (reset! !dist-bottom dist-from-bottom))

    (when (and has-items? (not loading-older?) initialized?)
      (reset! !at-bottom? at-bottom?)
      (when @!scroll-timer
        (js/clearTimeout @!scroll-timer))
      (reset! !scroll-timer
              (js/setTimeout
               (fn []
                 (reset! !scroll-timer nil)
                 (reset! !show-jump? (> dist-from-bottom 600))
                 (when (and (<= dist-from-top 600) (not loading-older?) (not older-dead?))
                   (when on-load-older (on-load-older)))
                 (when (and focus-mode? (<= dist-from-bottom 600) (not loading-newer?))
                   (when on-load-newer (on-load-newer))))
               300)))))
