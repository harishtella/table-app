(ns ^:figwheel-always table-app.core
    (:require
      [table-app.function-handler :as f]
      [reagent.core :as r]
      [reagent-table.core :as rt]
      [goog.events :as events]
      [goog.i18n.NumberFormat.Format])
    (:import
      (goog.i18n NumberFormat)
      (goog.i18n.NumberFormat Format)))

(def nff (NumberFormat. Format/DECIMAL))

(defn format-number
  [num]
  (.format nff (str num)))

;; -------------------------
;; Table data and helper functions

(defn temp-row-wise-fn [row]
  (rand-int 100))

(defn make-new-column [table row-wise-fn]
  (let [add-new-col-fn (fn [row]
                        (let [new-column-val (row-wise-fn row)]
                             (assoc row :function-output new-column-val)))]
       (map add-new-col-fn table)))

;; some dummy data
(def table-data (r/atom [{:Name    "Lizard"
                          :Colour  "Dark Green"
                          :Skin    "Leathery"
                          :Weight  100
                          :Age     10
                          :Hostile false}
                         {:Name    "Lion"
                          :Colour  "Gold"
                          :Skin    "Furry"
                          :Weight  190000
                          :Age     4
                          :Hostile true}
                         {:Name    "Giraffe"
                          :Colour  "Green"
                          :Skin    "Hairy"
                          :Weight  1200000
                          :Age     8
                          :Hostile false}]))

;the column model
(def columns [{:path [:Name]
               :header "Name"
               :key :Name}  ; convention - use field name for reagent key
              {:path [:Colour]
               :header "Colour"
               :key :Colour}
              {:path [:Skin]
               :header "Skin Type"
               :key :Skin}
              {:path [:Weight]
               :header "Weight"
               :format #(format-number %)
               :attrs (fn [data] {:style {:text-align "right"
                                          :display "block"}})
               :key :Weight}
              {:path [:Age]
               :header "Age"
               :attrs (fn [data] {:style {:text-align "right"
                                          :display "block"}})
               :key :Age}
              {:path [:Hostile]
               :header "Hostile?"
               :format #(if % "true" "false")
               :key :Hostile}
              {:path [:function-output]
               :header "function-output"
               :key :function-output}])

(defn- row-key-fn
  "Return the reagent row key for the given row"
  [row row-num]
  row-num)

(defn- cell-data
  "Resolve the data within a row for a specific column"
  [row cell]
  (let [{:keys [path expr]} cell]
    (and path
      (get-in row path))))

(defn- cell-fn
 "Return the cell hiccup form for rendering.
 - render-info the specific column from :column-model
 - row the current row
 - row-num the row number
 - col-num the column number in model coordinates"
 [render-info row row-num col-num]
 (let [{:keys [format attrs]
        :or   {format identity
               attrs (fn [_] {})}} render-info
       data    (cell-data row render-info)
       content (format data)
       attrs   (attrs data)]
   [:span
    attrs
    content]))

;; -------------------------
;; Views

(defn lister [items]
  [:ul
   (for [item items]
     ^{:key item} [:li "Item " item])])

(defn home-page []
  [:div
   [:h2 "Welcome to Reagent"]
   [lister (range 10)]])

;;----------------

(def function-text (r/atom "foo"))

(defn function-input [value]
  [:input {:type "text"
           :value @value
           :size 50
           :on-change #(reset! value (-> % .-target .-value))}])

(defn function-submit [table-data]
  (let [swap-fn #(make-new-column % temp-row-wise-fn)]
   [:input {:type "button" :value "compute!"
            :on-click #(swap! table-data swap-fn)}]))

(defn function-area []
  [:div
   [:div "Enter a function here: " [function-input function-text]]
   [:br]
   [function-submit table-data]])

;; -------------------------
;; Table component

;; draggable has been set to false, but not sure if it always working
(def table-state (atom {:draggable false}))

(defn table []
 [:div.container {:style {:font-size 16 :margin-top 10} :height "100%"}
    [rt/reagent-table table-data {:table {:class "table table-hover table-striped table-bordered table-transition"
                                          :style {:border-spacing 0
                                                  :border-collapse "separate"}}
                                  :table-container {:style {:border "none"}}
                                  :th {:style {:border "1px solid white" :background-color "black"}}
                                  :table-state  table-state
                                  :column-model columns
                                  :row-key      row-key-fn
                                  :render-cell  cell-fn
                                  :caption [:caption "My amazing data set"]}]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [:div [table] [function-area]] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
