(ns undertone.midi
  (:use overtone.core))

(defn midi-all-notes-off
  ([device]
     (doseq [c (range 15)]
       (midi-all-notes-off device c)))
  ([device channel]
     (let [msg (javax.sound.midi.ShortMessage.)]
       (.setMessage msg javax.sound.midi.ShortMessage/CONTROL_CHANGE channel 0x7b 0)
       (midi-send-msg (:receiver device) msg -1))))



(defn midi-program-change
  ([device channel-programs]
     (doseq [[c p] channel-programs]
       (println c p)
       (midi-program-change device c p)))
  ([device channel program]
     (midi-program-change device channel (or (:msb program) 0) (or (:lsb program) 0) (:patch program)))
  ([device channel msb lsb patch]
     (let [msg (javax.sound.midi.ShortMessage.)]
       (.setMessage msg javax.sound.midi.ShortMessage/CONTROL_CHANGE channel 0x00 msb)
       (midi-send-msg (:receiver device) msg -1)
       (.setMessage msg javax.sound.midi.ShortMessage/CONTROL_CHANGE channel 0x20 lsb)
       (midi-send-msg (:receiver device) msg -1)
       (.setMessage msg javax.sound.midi.ShortMessage/PROGRAM_CHANGE channel patch 0)
       (midi-send-msg (:receiver device) msg -1))))


(on-event :reset (fn [e]
                   (doseq [d midi-devices]
                     (midi-all-notes-off d)))
          ::midi-killer)

(defn string-contains? [string pattern]
  (> (.indexOf string pattern) -1))

(defmacro only-for-device [midi-msg device-name & body]
  `(when (string-contains? (get-in ~midi-msg [:device :name]) ~device-name)
    ~@body))

(def controller-atoms (atom {}))

(def print-next-control-input? (atom false))
(defn print-next-control-input []
  (reset! print-next-control-input? true))

(on-event [:midi :control-change] (fn [msg]
                                    (let [{controller :data1 val :data2} msg
                                          ctl-atom                       (get @controller-atoms controller)
                                          device-name                    (get-in msg [:device :name])]
                                      (when ctl-atom
                                        (swap! ctl-atom (fn [old-val] val)))
                                      (when @print-next-control-input?
                                        (println "Controller" controller "Value" val ctl-atom)
                                        (reset! print-next-control-input? false))
                                      ))
          :controller-atom-update)

(defn atom-for-controller [controller]
  (get (swap! controller-atoms
              #(if (contains? % controller)
                 %
                 (assoc % controller (atom 0))))
       controller)
  )

(defn wait-for-control-change []
  (let [controller (promise)]
    (oneshot-event [:midi :control-change]
                   #(deliver controller (:data1 %))
                   :oneshot-next-controller)
    @controller))

(defn control-synth-param
  ([controller key synth param]
     (control-synth-param controller key synth param 0 1))
  ([controller key synth param min max]
     (let [a (atom-for-controller controller)]
       (ctl synth param (scale-range @a 0 127 min max))
       (add-watch a key (fn [k r old new]
                          (ctl synth param (scale-range new 0 127 min max)))))
     synth))
