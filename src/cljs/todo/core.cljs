(ns todo.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to todo"]
   [:div [:a {:href "/about"} "go to about page"]]])

(defn about-page []
  [:div [:h2 "About todo"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

; (defn current-page []
;   [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/about" []
  (reset! page #'about-page))

(def state (atom {:title "Todo"
                  :add-todo "foo"
                  :update-todo ""
                  :todos []}))

(defn add-todo-form [state]
  [:div
    [:input {:type "text"
             :value (:add-todo @state)
             :on-change #(swap! state assoc :add-todo (-> % .-target .-value))}]
    [:button {:on-click #(swap! state assoc :todos (conj (:todos @state) {:text (:add-todo @state)
                                                                          :is-update false}))}
             "Add Todo"]])



(defn delete-todo
  [todo]
  (fn [_]
    (swap! state 
           assoc 
           :todos 
           (remove #(= (:text todo) (:text %)) (:todos @state)))))

(defn set-update-todo
  [todo]
  (fn [_]
    (swap! state 
           assoc 
           :todos 
           (map #(if (= (:text todo) (:text %)) (assoc % :is-update true) %) (:todos @state)))
    (swap! state assoc :update-todo (:text todo))))

(defn update-todo
  [todo new-val]
  (fn [e]
    (let [new-todo (-> todo
                       (assoc :text new-val) 
                       (assoc :is-update false)) 
          new-todos (replace {todo new-todo} (:todos @state))]
      (swap! state assoc :todos new-todos)
      (swap! state assoc :update-todo ""))))

(defn todo-text-element
  [todo]
  (if (= (:is-update todo) false)
    [:div (:text todo)]
    [:div
      [:input {:type "text"
               :value (:update-todo @state) 
               :on-change #(swap! state assoc :update-todo (-> % .-target .-value))}]
      [:button {:on-click (update-todo todo (:update-todo @state))}
               "save"]]))
    
(defn todo-element
  [todo]
  [:li {:key todo} [:div (todo-text-element todo)
                         [:button {:style {:margin-left "12px"}
                                   :on-click (delete-todo todo)} 
                                  "Delete"]
                         [:button {:on-click (set-update-todo todo)}
                                  "Update"]]]) 
                         
(defn todo-list [state]
  [:ul
    (doall (for [todo (:todos @state)]
             (todo-element todo)))])

(defn current-page []
  [:div 
    [:h1 (:title @state)] 
    (add-todo-form state) 
    (todo-list state)])
;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
