(ns undertone.looper
  (:use overtone.live
        undertone.midi))

(defsynth guitar
  [out-bus 0 in-bus 0 vol 1]
  (let [snd (pan2 (* vol (in:ar (+ (num-output-buses:ir) in-bus))))]
    (out 0 snd)
    (out out-bus snd)))

(defsynth pha [out-bus 0 buf 0 rate 1]
  (out out-bus (phasor:ar 0 rate 0 (buf-samples buf) 0)))

(defsynth looper [buf 0 pha-bus 0]
  (out 0 (buf-rd 2 buf (in pha-bus))))

(defsynth recorder [buf 0 pha-bus 0 in-bus 0]
  (buf-wr (+ (in in-bus) (* 0.75 (buf-rd 2 buf (in pha-bus))))  buf (in pha-bus)))

(def g-bus (audio-bus 2))
(def phase-bus (audio-bus 1))


(defn create-looper-buffer [len]
  (let [b (buffer (* 44100 len) 2)]
    (looper b phase-bus)
    b))


(defn start-new-looper [len]
  (let [b (create-looper-buffer len)]
    (pha :pos :head phase-bus b)
    (guitar :pos :head g-bus 1 3)
    (recorder :pos :tail b phase-bus g-bus)))

(defn add-looper-track [len]
  (let [b (create-looper-buffer len)]
    (kill (node-tree-matching-synth-ids "recorder" 0))
    (recorder :pos :tail b phase-bus g-bus)))

;; Do this first
(start-new-looper 2)

;; Whenever you're happy with the sound, do this and record a new track.
;; Previous tracks will stop fading away.
(add-looper-track 2)

(stop)

(on-controller-change
  8
  #(ctl (node-tree-matching-synth-ids "pha" 0) :rate (scale-range % 0 127 0 2 )))

(kill (node-tree-matching-synth-ids "recorder" 0))
