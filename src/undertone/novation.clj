(ns undertone.novation
  (:use overtone.live
        undertone.midi
        overtone.inst.drum))

(def DEVICE-NAME "ReMOTE ZeRO SL")

(defn get-novation-device []
  (midi-out DEVICE-NAME))
(defn unsigned-int-to-signed-byte [i]
  (byte (if (> i 127)
          (- i 256)
          i)))

(defn novation-send-sysex [data]
  (let [device (get-novation-device)]
    (midi-sysex device (map unsigned-int-to-signed-byte data))))

(defn novation-clear-screen [screen]
  (let [msg [0xf0 0x00 0x20 0x29 0x03 0x03 0x11 0x04 0x03 0x00 0x02 0x02]
        msg (concat msg [(if (= screen :left) 4 5) 0xf7])]
    (novation-send-sysex msg)))

(defn novation-send-text [screen row col text]
  (let [row-base (screen {:left 1 :right 2})
        row (+ row-base (row {:top 0 :bottom 2}))
        msg [0xf0 0x00 0x20 0x29 0x03 0x03 0x11 0x04 0x03 0x00 0x02 0x01 col row 0x04]
        msg (concat msg (map int (seq text)) [0xf7])]
    (novation-send-sysex msg)))


(when (get-novation-device)
  (novation-clear-screen :left)
  (novation-clear-screen :right)
  (novation-send-text :right :top 20 "Look! I can write to the screen!"))


(on-event [:midi :note-on] (bound-fn [{note :note vel :velocity :as msg}]
                             (only-for-device msg DEVICE-NAME
                               (println "Note" note "Vel" vel)
                               (case note
                                 36 (kick)
                                 37 (snare)
                                 38 (noise-snare)
                                 39 (open-hat)
                                 40 (closed-hat)
                                 41 (closed-hat2)
                                 42 (soft-hat)
                                 43 (bing))
                               )
                             )
          :novation-note-on)


(add-watch (atom-for-controller 74) :stop-button (fn [k r old new]
                                                   (when (= 1 new)
                                                     (stop)
                                                     (novation-send-text :left :top 29 "--- STOPPED ---")
                                                     (future (do (Thread/sleep 1000) (novation-clear-screen :left))))))

(add-watch (atom-for-controller 23) :slider8lcd (fn [k r old new]
                                                  (novation-send-text :right :bottom 64 (str "   " new "     "))))



(defsynth boring-synth [freq 440 vol 1]
  (out 0 (pan2 (* vol (sin-osc freq)))))


(print-next-control-input)



(comment (let [synth (ping)]
           (control-synth-param 8 :pot8 synth :note 22 88)
           (control-synth-param 23 :slider8 synth :osc 40 600)))
