(ns ^:figwheel-always table-app.core
    (:require
      [table-app.function-handler :as fh]
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

(defn make-new-column [table function-string]
  (let [row-wise-fn (partial fh/evaluate-expr function-string)
        add-new-col-fn (fn [row]
                           (let [new-column-val (row-wise-fn row)]
                                (assoc row :function-output new-column-val)))]
       (map add-new-col-fn table)))

;; some dummy data
(def table-data (r/atom [{:name    "Lizard"
                          :colour  "Dark Green"
                          :skin    "Leathery"
                          :weight  100
                          :age     10
                          :hostile false}
                         {:name    "Lion"
                          :colour  "Gold"
                          :skin    "Furry"
                          :weight  190000
                          :age     4
                          :hostile true}
                         {:name    "Giraffe"
                          :colour  "Green"
                          :skin    "Hairy"
                          :weight  1200000
                          :age     8
                          :hostile false}]))

;the column model
(def columns [{:path [:name]
               :header "name"
               :key :name}  ; convention - use field name for reagent key
              {:path [:colour]
               :header "colour"
               :key :colour}
              {:path [:skin]
               :header "skin"
               :key :skin}
              {:path [:weight]
               :header "weight"
               :format #(format-number %)
               :attrs (fn [data] {:style {:text-align "right"
                                          :display "block"}})
               :key :weight}
              {:path [:age]
               :header "age"
               :attrs (fn [data] {:style {:text-align "right"
                                          :display "block"}})
               :key :age}
              {:path [:hostile]
               :header "hostile"
               :format #(if % "true" "false")
               :key :hostile}
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
  (if-let [{:keys [path]} cell]
    (get-in row path)))

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
;; Function area component

(def function-text (r/atom ""))

(defn function-input [value]
  [:input {:type "text"
           :value @value
           :size 50
           :on-change #(reset! value (-> % .-target .-value))}])

(defn function-submit [table-data]
  [:input {:type "button" :value "compute!"
           :on-click #(swap! table-data make-new-column @function-text)}])

(defn function-area []
  [:div
   [:div "Enter a function here: " [function-input function-text]]
   [:br]
   [function-submit table-data]])

;; -------------------------
;; Table component

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
                                  :render-cell  cell-fn}]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [:div [table] [function-area]] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
