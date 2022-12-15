(ns svg-clj.viewers
  (:require [nextjournal.clerk.sci-viewer :as sv]
            [sci.core :as sci]
            [reagent.core :as r]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.transforms :as tf]
            [svg-clj.composites :as comp :refer [svg]]
            [svg-clj.path :as path]
            [svg-clj.parametric :as p]
            [svg-clj.layout :as lo]))

;; Here is the existing context:
;; https://github.com/nextjournal/clerk/blob/d08c26043efe19a92fe33dd9eb4499e304e4cff7/src/nextjournal/clerk/sci_viewer.cljs#L1013-L1023

(sci.impl.cljs/require-cljs-analyzer-api)

(defn- get-parent-svg
  [node]
  (let [parent (.-parentElement node)
        tag (.-tagName parent)]
    (when (not (#{"div" "span" "body" "html"} tag))
      (if (= tag "svg")
        parent
        (recur parent)))))

(defn- element-offset
  [el]
  {:x (-> el .getBoundingClientRect .-left int)
   :y (-> el .getBoundingClientRect .-top int)
   #_#_:a (-> js/window .-scrollY)})

;; https://cljdoc.org/d/reagent/reagent/1.1.1/doc/tutorials/creating-reagent-components
(defn mouse-pos
  ([] (mouse-pos js/document))
  ([el]
   (r/with-let [pointer (r/atom nil)
                handler #(reset! pointer
                                 (merge-with -
                                             {:x (.-clientX %)
                                              :y (.-clientY %)}
                                             (when (not= el js/document)
                                               (element-offset el))))
                _ (.addEventListener el "mousemove" handler)]
     @pointer
     (finally
       (.removeEventListener el "mousemove" handler)))))

(defn number-input
  ([state-target]
   (number-input state-target nil))
  ([state-target on-change-fn]
   (let [number-input-class
         (apply str
                (interpose " " ["dark:bg-[#1f2937]" "dark:text-[#cbd5e1]" "form-control" "w-16" "px-1" "py-1" "text-gray-500" "bg-clip-padding" "border-gray-300" "rounded" "transition" "ease-in-out" "focus:text-gray-700" "focus:bg-white" "focus:border-gray-600" "focus:outline-none"]))]
     [:input (merge
               {:class (str "inspected-value " number-input-class)
                :style {:margin 6
                        :text-align "center"}
                :type :number
                :placeholder @state-target
                :value @state-target
                :on-input #(reset! state-target (.. % -target -value))}
               {:on-change on-change-fn})])))

(defn movable-pt-1
  [w h]
  (r/with-let [down-pt (r/atom {:x 0 :y 0})
               old-local-pt (r/atom {:x 10 :y 10})
               local-pt (r/atom {:x 10 :y 10})
               move! (r/atom false)
               refa (r/atom nil)]
    [:<>
     [:span "Mouse Pos: " (str @(r/track mouse-pos))] [:br]
     [:span "Local Mouse Pos: " (when @refa (str @(r/track mouse-pos @refa)))] [:br]
     [:span "Down Pt: " (str @down-pt)] [:br]
     [:span "Local Pt: " (str @local-pt)] [:br]

     [:svg {:width w
            :height h
            :style {:background "honeydew"}
            :ref (fn [el] (reset! refa el))
            :on-mouse-move #(when @move!
                              (reset! local-pt (merge-with + @old-local-pt
                                                           (merge-with - @(r/track mouse-pos @refa)
                                                                       @down-pt))))
            :on-mouse-up #(reset! move! false)}
      (let [pt (r/atom (vec (vals @local-pt)))]
        (-> (el/circle 5)
            (tf/translate @pt)
            (tf/style {:on-mouse-down #(do (reset! down-pt @(r/track mouse-pos @refa))
                                           (reset! old-local-pt {:x (first @pt)
                                                                 :y (second @pt)})
                                           (reset! move! true))})))]]))

(defn movable-pt-2
  [parent-el r]
  (r/with-let [down-pt (r/atom {:x 0 :y 0})
               old-local-pt (r/atom {:x 10 :y 10})
               local-pt (r/atom {:x 10 :y 10})
               move! (r/atom false)
               el-pt (r/track mouse-pos parent-el)]
    (let [pt (r/atom (vec (vals @local-pt)))
          handler1 #(when @move!
                      (reset! local-pt (merge-with + @old-local-pt (merge-with - @el-pt @down-pt))))
          handler2 #(reset! move! false)
          handler3 #(do (reset! down-pt @el-pt)
                        (reset! old-local-pt {:x (first @pt)
                                              :y (second @pt)})
                        (reset! move! true))]
      (.addEventListener parent-el "mousemove" handler1)
      (.addEventListener parent-el "mouseup" handler2)
      (el/g
        (-> (el/text (str @el-pt))
            (tf/style {:display "none"}))
        (-> (el/circle r)
            (tf/translate @pt)
            (tf/style {:on-mouse-down handler3}))))))

(defn movable-pt-3
  [r]
  (let [parent-el! (r/atom nil)]
    (r/create-class
      {:display-name "movable-pt"
       :component-did-mount (fn [this] (let [node (reagent.dom/dom-node this)
                                             parent (get-parent-svg node)]
                                         (js/console.log (.-tagName parent))
                                         (reset! parent-el! parent)))
       :reagent-render
       (fn [r]
         (el/g
           (when @parent-el!
             (r/with-let [parent-el @parent-el!
                          down-pt (r/atom {:x 0 :y 0})
                          old-local-pt (r/atom {:x 10 :y 10})
                          local-pt (r/atom {:x 10 :y 10})
                          move! (r/atom false)
                          el-pt (r/track mouse-pos parent-el)]
               (let [pt (r/atom (vec (vals @local-pt)))
                       handler1 #(when @move!
                                   (reset! local-pt (merge-with + @old-local-pt (merge-with - @el-pt @down-pt))))
                       handler2 #(reset! move! false)
                       handler3 #(do (reset! down-pt @el-pt)
                                     (reset! old-local-pt {:x (first @pt)
                                                           :y (second @pt)})
                                     (reset! move! true))]
                   (.addEventListener parent-el "mousemove" handler1)
                   (.addEventListener parent-el "mouseup" handler2)
                   [:<>
                     (-> (el/text (str @el-pt))
                         (tf/style {:display "none"}))
                     (-> (el/circle r)
                         (tf/translate @pt)
                         (tf/style {:on-mouse-down handler3}))])))))})))

(defn movable-pt
  [pt! r]
  (let [parent-el! (r/atom nil)]
    (r/create-class
      {:display-name "movable-pt"
       :component-did-mount (fn [this] (let [node (reagent.dom/dom-node this)
                                             parent (get-parent-svg node)]
                                         (reset! parent-el! parent)))
       :reagent-render
       (fn [pt! r]
         (el/g
           (when @parent-el!
             (r/with-let [parent-el @parent-el!
                          [x y] @pt!
                          down-pt (r/atom {:x 0 :y 0})
                          old-local-pt (r/atom {:x 0 :y 0})
                          local-pt (r/atom {:x x :y y})
                          move! (r/atom false)
                          el-pt (r/track mouse-pos parent-el)]
               (let [pt (r/atom (vec (vals @local-pt)))
                     handler1 #(when @move!
                                 (do (reset! local-pt (merge-with + @old-local-pt (merge-with - @el-pt @down-pt)))
                                     (reset! pt! @pt)))
                     handler2 #(reset! move! false)
                     handler3 #(do (reset! down-pt @el-pt)
                                   (reset! old-local-pt {:x (first @pt)
                                                         :y (second @pt)})
                                   (reset! move! true))]
                 (.addEventListener parent-el "mousemove" handler1)
                 (.addEventListener parent-el "mouseup" handler2)
                 [:<>
                  (-> (el/text (str @el-pt))
                      (tf/style {:display "none"}))
                  (-> (el/circle r)
                      (tf/translate @pt)
                      (tf/style {:on-mouse-down handler3}))])))))})))

(defn moveable
  [pt! el]
  (let [parent-el! (r/atom nil)]
    (r/create-class
      {:display-name "movable-pt"
       :component-did-mount (fn [this] (let [node (reagent.dom/dom-node this)
                                             parent (get-parent-svg node)]
                                         (reset! parent-el! parent)))
       :reagent-render
       (fn [pt! r]
         (el/g
           (when @parent-el!
             (r/with-let [parent-el @parent-el!
                          [x y] @pt!
                          down-pt (r/atom {:x 0 :y 0})
                          old-local-pt (r/atom {:x 0 :y 0})
                          local-pt (r/atom {:x x :y y})
                          move! (r/atom false)
                          el-pt (r/track mouse-pos parent-el)]
               (let [pt (r/atom (vec (vals @local-pt)))
                     handler1 #(when @move!
                                 (do (reset! local-pt (merge-with + @old-local-pt (merge-with - @el-pt @down-pt)))
                                     (reset! pt! @pt)))
                     handler2 #(reset! move! false)
                     handler3 #(do (reset! down-pt @el-pt)
                                   (reset! old-local-pt {:x (first @pt)
                                                         :y (second @pt)})
                                   (reset! move! true))]
                 (.addEventListener parent-el "mousemove" handler1)
                 (.addEventListener parent-el "mouseup" handler2)
                 [:<>
                  (-> (el/text (str @el-pt))
                      (tf/style {:display "none"}))
                  (-> el
                      (tf/translate @pt)
                      (tf/style {:on-mouse-down handler3}))])))))})))

;; example usage
#_(svg
    (r/with-let [pts (r/atom (vec (p/regular-polygon-pts 40 8)))
                 el (-> (el/circle 6) (tf/style {:fill "red"}))]
      (el/g
        (-> (el/rect 400 400) (tf/style {:fill "honeydew"}))
        (-> (el/polygon @pts)
            (tf/style {:stroke "green" :fill "none" :stroke-width 2}))
        (into [:<>]
              (map (fn [idx]
                     (let [pt! (r/cursor pts [idx])] [movable pt! el])) (range (count @pts)))))))

(def slider-render-fn
  (let [base-style {:margin-right 8
                    :display "inline-block"
                    :text-align "end"
                    :min-width 20}]
    (fn [global-state k sync-fn]
      (r/with-let [state (r/cursor global-state [k])
                   tmp (r/atom 1)
                   minv (r/atom 1)
                   maxv (r/atom 100)
                   refa (r/atom nil)]
        [:<>
         ;; var name
         [:span {:class "inspected-value"
                 :style (merge base-style {:text-align "end"})} (str (name k) " ")]
         ;; var value
         [:span {:class "inspected-value"
                 :style base-style} @state]
         ;; minimum input
         [number-input minv]
         ;; slider input
         [:input {:style {:height "0.5em"}
                  :type :range
                  #_#_:value @state
                  :min @minv
                  :max @maxv
                  :on-change #(reset! state (js/parseInt (.. % -target -value)))
                  :on-touch-end sync-fn
                  :on-mouse-up sync-fn}]
         ;; maximum input
         [number-input maxv]
         ;; var value input
         [number-input tmp sync-fn]]))))

(def moveable-pt-render-fn
  (let [base-style {:margin-right 8
                    :display "inline-block"
                    :text-align "end"
                    :min-width 20}]
    (fn [global-state k sync-fn]
      (r/with-let [state (r/cursor global-state [k])
                   elem (-> (el/circle 6) (tf/style {:fill "red"
                                                     :on-mouse-up sync-fn}))]
        [:<>
         ;; var name
         [:span {:class "inspected-value"
                 :style (merge base-style {:text-align "end"})} (str (name k) " ")]
         ;; var value
         [:span {:class "inspected-value"
                 :style base-style} (str @state)]
         ;; moveable-pt input
         (svg
           (el/g
             (-> (el/rect 200 200) (tf/style {:stroke "black" :fill "none"}))
             (-> (el/line [0 -100] [0 100]) (tf/style {:stroke "red" :fill "none"}))
             (-> (el/line [-100 0] [100 0]) (tf/style {:stroke "green" :fill "none"}))
             (-> (el/line [0 (second @state)] @state) (tf/style {:stroke "gray" :fill "none"}))
             (-> (el/line [(first @state) 0] @state) (tf/style {:stroke "gray" :fill "none"}))
             [:<> [moveable state elem]]))]))))

(def moveable-pts-render-fn
  (let [base-style {:margin-right 8
                    :display "inline-block"
                    :text-align "end"
                    :min-width 20}]
    (fn [global-state k sync-fn]
      (r/with-let [state (r/cursor global-state [k])
                   elem (-> (el/circle 6) (tf/style {:fill "red"
                                                     :on-mouse-up sync-fn}))]
        [:<>
         ;; var name
         [:span {:class "inspected-value"
                 :style (merge base-style {:text-align "end"})} (str (name k) " ")]
         ;; var value
         [:span {:class "inspected-value"
                 :style base-style} (str @state)]
         ;; moveable-pt input
         (svg
           (el/g
             (-> (el/rect 200 200) (tf/style {:stroke "black" :fill "none"}))
             (-> (el/line [0 -100] [0 100]) (tf/style {:stroke "red" :fill "none"}))
             (-> (el/line [-100 0] [100 0]) (tf/style {:stroke "green" :fill "none"}))
             (-> (el/polygon @state) (tf/style {:stroke "blue" :fill "none"}))
             (into [:<>]
              (map (fn [idx]
                     (let [pt! (r/cursor state [idx])] [moveable pt! elem])) (range (count @state))))))]))))

(swap! sv/!sci-ctx
       sci/merge-opts
       {:namespaces
        {'svg-clj.viewers {'slider-render-fn slider-render-fn
                           'moveable-pt-render-fn moveable-pt-render-fn
                           'moveable-pts-render-fn moveable-pts-render-fn}
         'svg-clj.utils (sci/copy-ns svg-clj.utils (sci/create-ns 'svg-clj.utils))
         'svg-clj.elements (sci/copy-ns svg-clj.elements (sci/create-ns 'svg-clj.elements))
         'svg-clj.transforms (sci/copy-ns svg-clj.transforms (sci/create-ns 'svg-clj.transforms))
         'svg-clj.composites (sci/copy-ns svg-clj.composites (sci/create-ns 'svg-clj.composites))
         'svg-clj.path (sci/copy-ns svg-clj.path (sci/create-ns 'svg-clj.path))
         'svg-clj.parametric (sci/copy-ns svg-clj.parametric (sci/create-ns 'svg-clj.parametric))
         'svg-clj.layout (sci/copy-ns svg-clj.layout (sci/create-ns 'svg-clj.layout))}

        :aliases {'sv 'svg-clj.viewers
                  'utils 'svg-clj.utils
                  'el 'svg-clj.elements
                  'tf 'svg-clj.transforms
                  'c 'svg-clj.composites
                  'path 'svg-clj.path
                  'p 'svg-clj.parametric
                  'lo 'svg-clj.layout}})
