(ns ^:figwheel-no-load table-app.dev
  (:require
    [table-app.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
