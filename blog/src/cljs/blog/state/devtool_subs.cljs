(ns blog.state.devtool-subs
  (:require [re-frame.core :refer [reg-sub]])
  (:require-macros [reagent.ratom :refer [reaction]]))

(reg-sub :devtool-visible?
         (fn [db _]
           (or (get db :show-devtool?)
                         false)))
