(ns undertone.guitar
  (:use overtone.live))

(definst guitar []
  (+ (* 10 (sound-in 1))
   (* 3 (sound-in 0))))

(guitar)
(clear-fx guitar)
(inst-fx! guitar fx-freeverb)
(inst-fx! guitar fx-echo)
(inst-fx! guitar fx-distortion)

(ctl fx-echo :delay-time 0.1)
(stop)

(volume (/  32))
(stop)
