(ns undertone.guitar
  (:use overtone.live))

(definst guitar []
  (sound-in 1))

(guitar)
(clear-fx guitar)
(inst-fx! guitar fx-freeverb)
(inst-fx! guitar fx-echo)
(inst-fx! guitar fx-distortion)

(ctl fx-echo :delay-time 0.1)
(stop)

(volume (/  32))
(stop)
