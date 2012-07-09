(ns undertone.buses
  (:use overtone.live
        undertone.synth))

(defsynth test-synth [b 80]
  (out b (pan2 (sin-osc))))

(def t (test-synth))

(defsynth play-80 []
  (out 0 (in 80 2)))

(play-80)

(ctl t :b 82)
