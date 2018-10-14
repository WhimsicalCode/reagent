(ns reagent.debug
  (:require [goog.string.format]
            [goog.string :as gstring])
  (:require-macros [reagent.debug :refer [log ]]))

(def ^:const has-console (exists? js/console))

(def ^boolean tracking false)

(defonce warnings (atom nil))

(defonce track-console
  (let [o #js{}]
    (set! (.-warn o)
          (fn [& args]
            (swap! warnings update-in [:warn] conj (apply str args))))
    (set! (.-error o)
          (fn [& args]
            (swap! warnings update-in [:error] conj (apply str args))))
    o))

(defn track-warnings [f]
  (set! tracking true)
  (reset! warnings nil)
  (f)
  (let [warns @warnings]
    (reset! warnings nil)
    (set! tracking false)
    warns))

(defonce trace? (atom false))

(defonce renders (atom nil))

(defn reset-trace! [on?]
  (reset! renders nil)
  (reset! trace? on?))

(defn update-trace [{:keys [t c] :or {:t 0 :c 0}} start]
  {:t (+ t (- (js/performance.now) start))
   :c (inc c)})

(def format gstring/format)

(defn log-renders
  ([] (log-renders nil))
  ([top-n]
   (let [traces (->> @renders
                     (map (fn [[name trace]] (assoc trace :n name)))
                     (sort-by :t)
                     (reverse))
         traces (cond->> traces
                         top-n (take top-n))]
     (doseq [{:keys [n t c]} traces]
       (log (format "%s Total Time: %.3f Avg Time: $.3f Count: %d" n t (/ t c) c))))))
