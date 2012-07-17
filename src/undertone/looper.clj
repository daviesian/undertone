(ns undertone.looper
  (:use overtone.live
        undertone.midi))

(defsynth guitar
  [out-bus 0 in-bus 0 vol 1]
  (let [snd (pan2 (* vol (in:ar (+ (num-output-buses:ir) in-bus))))]
    (out 0 snd)
    (out out-bus snd)))

(defsynth pha [out-bus 0 out-buf 0 length 0 rate 1]
  (let [current-pos (phasor:ar 0 rate 0 length 0)]
    (out out-bus current-pos)
    (buf-wr [current-pos] out-buf 0)))

(defsynth looper [buf 0 pha-bus 0]
  (out 0 (buf-rd 2 buf (in pha-bus)))
  )

(defsynth recorder [target-buf 0 pha-bus 0 in-bus 0 decay 0.75 monitor 0]
  (let [previous-sig (buf-rd 2 target-buf (in pha-bus))
        new-input    (* 16 (in in-bus))
        position     (in pha-bus)
        monitor-sig  (select monitor [(* 0 new-input) new-input])]
    (buf-wr (+ new-input (* decay previous-sig)) target-buf position)
    (out 0 monitor-sig)
    ))



(def pha-bus (audio-bus 1))
(def pha-buf (buffer 1))



(defn create-and-loop-buffer [len]
  (let [b (buffer (* 44100 len) 2)]
    (looper :buf b
            :pha-bus pha-bus)
    b))

(defn start-new-looper [len]
  (let [b (create-and-loop-buffer len)]
    (pha :position :head
         :out-bus pha-bus
         :length (* 44100 len)
         :out-buf pha-buf)
    (recorder :position :tail
     :target-buf b
     :pha-bus pha-bus
     :in-bus (+ @output-bus-count* 1)
     :decay 0.75
     :monitor 1)))

(defn add-looper-track [len]
  (let [b (create-and-loop-buffer len)]
    (kill (node-tree-matching-synth-ids "recorder" 0))
    (recorder :pos :tail
              :target-buf b
              :pha-bus pha-bus
              :in-bus (+ @output-bus-count* 1)
              :decay 0.75
              :monitor 1)))


(defn set-loop-end-now []
  (let [pos (buffer-get pha-buf)]
    (ctl (node-tree-matching-synth-ids "pha" 0) :length pos)))

;; Do this first
(start-new-looper 2)

(set-loop-end-now)

;; Whenever you're happy with the sound, do this and record a new track.
;; Previous tracks will stop fading away.
(add-looper-track 2)

(stop)

(on-controller-change
  8
  #(ctl (node-tree-matching-synth-ids "pha" 0) :rate (scale-range % 0 127 0 2 )))

(volume  1)
