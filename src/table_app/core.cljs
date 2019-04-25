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

(defn add-new-column [table]
  (let [add-col-fn (fn [map] (assoc map :function-output (rand-int 100)))]
   nil))

;; generate some dummy data
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
               :key :Hostile}])

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
           :on-change #(reset! value (-> % .-target .-value))}])

(defn function-submit []
  [:input {:type "button" :value "compute!"
           :on-click #(add-new-column table-data)}])

(defn function-area []
  [:div
   [:p "Enter a function here: " [function-input function-text]]
   [function-submit]])

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
                                  :scroll-height "80vh"
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
