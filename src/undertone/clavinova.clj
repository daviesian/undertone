(ns undertone.clavinova
  (:use overtone.core
        undertone.midi))

(def clav (midi-out "Clavinova"))

(defn clav-vol [device channel vol]
  (midi-control device 0x07 vol))

(def clav-patches
  {:grand-piano-1     {:lsb 122 :patch 0}
   :grand-piano-1-var {:lsb 123 :patch 0}

   :grand-piano-2     {:lsb 112 :patch 0}
   :grand-piano-2-var {:lsb 112 :patch 1}

   :e-piano-1         {:lsb 122 :patch 5}
   :e-piano-1-var     {:lsb 1 :patch 88}

   :e-piano-2         {:lsb 122 :patch 4}
   :e-piano-2-var     {:lsb 123 :patch 4}

   :harpsichord       {:lsb 122 :patch 6}
   :harpsichord-var   {:lsb 123 :patch 6}

   :e-clavichord      {:lsb 122 :patch 7}
   :e-clavichord-var  {:lsb 123 :patch 7}

   :vibraphone        {:lsb 122 :patch 11}
   :vibaphone-var     {:lsb 122 :patch 12}

   :church-organ      {:lsb 123 :patch 19}
   :church-organ-var  {:lsb 122 :patch 19}

   :jazz-organ        {:lsb 122 :patch 16}
   :jazz-organ-var    {:lsb 123 :patch 16}

   :strings           {:lsb 122 :patch 48}
   :strings-var       {:lsb 122 :patch 49}

   :choir             {:lsb 122 :patch 52}
   :choir-var         {:lsb 123 :patch 52}

   :guitar            {:lsb 122 :patch 24}
   :guitar-var        {:lsb 122 :patch 25}

   :wood-bass         {:lsb 122 :patch 32}
   :wood-bass-var     {:lsb 124 :patch 32}

   :e-bass            {:lsb 122 :patch 33}
   :e-bass-var        {:lsb 122 :patch 35}
   })

;(midi-program-change clav 0 (clav-patches :strings))


;(midi-program-change clav 0 122 49)
;(midi-note-on clav 60 90)
;(midi-note-off clav 60)
;(midi-program-change clav 0 122 0)


(def pressed-keys (atom #{}))
(def pedal-down (atom false))
(def sounding-notes (atom {}))

(defn pedal-updated [k r old new]
  (let [pedal-released (not new)]
    (when pedal-released
      (swap! sounding-notes #(let [dead-notes (filter (fn [x] (not (contains? @pressed-keys x))) (keys %))]
                               (apply dissoc (cons % dead-notes)))))))

(add-watch pedal-down :pedal-updated #'pedal-updated)

(on-event [:midi :note-on] (fn [{note :note vel :velocity}]
                             (swap! pressed-keys #(conj % note))
                             ;(swap! sounding-notes #(dissoc % note)) ; Why do this?!
                             (swap! sounding-notes #(assoc % note vel)))
          ::note-ons)

(on-event [:midi :note-off] (fn [{note :note}]
                              (swap! pressed-keys #(disj % note))
                              (when (not @pedal-down)
                                (swap! sounding-notes #(dissoc % note))))
          ::note-offs)

(on-event [:midi :control-change] (fn [{controller :data1 value :data2}]
                                    (when (= 64 controller)
                                      (let [down (< 63 value)]
                                        (compare-and-set! pedal-down (not down) down))))
          ::control-change)
