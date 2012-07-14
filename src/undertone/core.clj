(ns undertone.core
  (:use overtone.live
        undertone.clavinova
        undertone.novation
        undertone.synth
        undertone.midi
        )
  (:import (java.util Random)))

(demo (sin-osc))

(defmacro $
  ([single]
     (if (seq? single)
       `($ ~@single)          ; The single form is a list. Recurse.
       single))               ; The single form is a symbol. Leave as-is
  ([first & more]
     (let [[second & rest] more]
       (cond (= first '$)
             `(~@more) ; This is an escape from the infix notation. Leave this form as-is.
             (= first 'clojure.core/deref)
             `(~first ~@more)
             :else             ; Rewrite infix to prefix, left-to-right.
             `(~second ($ ~first) ($ ~@rest))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some MIDI Stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn echo-note [note velocity delay]
  (let [vel ($ velocity * ($ scale-range   42  0 127 0.5 1.5))
        vel (if ($ vel > 127) 127 vel)
        n   ($ note + 12)]
    (after-delay delay #(
                         (midi-note-on clav n vel)
                         (after-delay delay (fn [] (midi-note-on clav n 0)))
                         (when ($ vel > 30)
                           (echo-note n vel delay))))))

(on-event [:midi :note-on] (fn [{note :note velocity :velocity timestamp :timestamp}]
                             (println "Note: " note ", Velocity: " velocity ", Timestamp: " timestamp)
                             (echo-note note velocity 150)
                             )
          ::echo)

(remove-handler ::echo)





(defsynth my-sin-synth [note 62 vel 0.9 gate 1]
  (let [env        (envelope [0 1 0] [0.1 0.1] :linear 1)
        eg         (env-gen env gate :action FREE)
        sig        (sin-osc ($ (1 + 0.05 * ($ sin-osc 10)) * ($ midicps note)))]
    (out 0 (pan2
            ($ eg * sig * vel)))))


(defn intervals [notes]
  (if ($ ($ count notes) < 2)
    #{}
    (let [sorted-notes (sort notes)
          pairs        (partition 2 (interleave sorted-notes (rest sorted-notes)))]
      (map (fn [[a b]] (- b a)) pairs)
      )))

(def live-insts (atom {}))

(defn start-inst [note vel]
  (let [i (my-sin-synth note (/ vel 128))]
    (swap! live-insts #(assoc % note i))))

(defn kill-inst [note]
  (let [i (get @live-insts note)]
    (node-control i [:gate 0])
    (swap! live-insts #(dissoc % note))))

(defn notes-updated [& args]
  (let [notes (keys @sounding-notes)
        chord (find-chord notes)]

    (println "Keys:" @pressed-keys)
    (println "Sounding Notes:" @sounding-notes)
    (println "Intervals: " (intervals notes))
    (println "Chord: " chord)

    (println)
    ))

(defn insts-update [k r old new]
  (let [new-notes (filter #(not ($ old contains? %)) (keys new))
        dead-notes (filter #(not ($ new contains? %)) (keys old))]
    (doseq [note new-notes]
      (start-inst note (get new note)))
    (doseq [note dead-notes]
      (kill-inst note))))

(add-watch sounding-notes :notes-updated #'notes-updated)
(add-watch sounding-notes :insts-updated #'insts-update)

(remove-watch sounding-notes :notes-updated)
(remove-watch sounding-notes :insts-updated)


(defn mixer-notes-updated [k r old new]
  (let [new-notes (filter #(not (contains? old %)) (keys new))
        old-notes (filter #(not (contains? new %)) (keys old))
        channels  2]
    (doall (for [n old-notes
                 i (range channels)]
             (do
               (midi-note-off clav n (+ 2 i)))))
    (doall (for [n new-notes
                 i (range channels)]
             (do
               (midi-note-on clav n 90 (+ 2 i))
               )))))


#_(midi-program-change clav {2 (clav-patches :strings)
                           3 (clav-patches :choir)})

(add-watch (atom-for-controller 16) :midi-vol (fn [k r old new]
                                                  (midi-control clav 0x07 new 2)))

(add-watch (atom-for-controller 17) :midi-vol (fn [k r old new]
                                                  (midi-control clav 0x07 new 3)))

(remove-watch (atom-for-controller 16) :midi-vol)
(remove-watch (atom-for-controller 17) :midi-vol)

(add-watch sounding-notes :mixer-notes-updated #'mixer-notes-updated)
(remove-watch sounding-notes :mixer-notes-updated)
;; INSTS

(defn piano-mirror [ns]
  (let [ns @sounding-notes]
    (doseq [n (keys ns)]
      (my-overpad {:note n :attack 0.1 :release 0.2}))))

(stop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some working sequence stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:dynamic true} *out-bus* 0)

(defn deref-if-var [s]
  (if (var? s) @s s))

(defn infinite-rand-chain [variations]
  (apply concat (repeatedly #(rand-nth (deref-if-var variations)))))

(defn drum [n]
  (cond
    (= n :kick) (my-sample-player (sample (freesound-path 777)) :vol 3 :bus *out-bus*)
    (= n :hat) (my-closed-hat :bus *out-bus*)
    (= n :snare) (my-sample-player (sample (freesound-path 26903)) :vol 0.8 :bus *out-bus*)
    :else (println "Unknown drum:" n)))

(defn funky-sequence []
  [#{:kick :hat}
   :hat
   [:hat :hat]
   :hat
   #{:kick :snare :hat}
   :hat
   :hat
   :hat])

(defn infinite-drum-track-generator []
  (apply concat (repeatedly #'funky-sequence)))

(def pad-timing-variations
  (let [- nil
        | 0
        + 1
        * 2]
    [[ | - - | - - | - - + - - + - - * - - * - - | - - | - | - + - + - ]
     [ | - - | - - + - - + - - * - - * - - | - - | - - | - - | - - + - ]
     [ | - - | - - | - + - + - * - - * - - * - - * - - | - - | - - + - ]
     [ | - - | - - | - - + - - * - * - | - - | - - | - - | - - | - + - ]
     ]))

(defn chord-variations [c v]
  (let [params {:release 0.9 :attack 0.1}]
    (set (map (fn [n] (assoc params :note n))
              (get-in
               {:b3 {0 (take 2 (chord :b3 :minor))
                     1 (take 2 (chord :b3 :sus2))
                     2 (take 2 (chord :b3 :sus4))}
                :a3 {0 (take 2 (chord :a3 :major))
                     1 (take 2 (chord :a3 :major))
                     2 (take 2 (chord :a3 :major))}
                :g3 {0 (take 2 (chord :g3 :major))
                     1 (take 2 (chord :g3 :major))
                     2 (take 2 (chord :g3 :sus2))}
                :d4 {0 (take 2 (chord :d4 :major))
                     1 (take 3 (chord :d4 :major))
                     2 (take 3 (chord :d4 :major))}}
               [c v])))))

(def infinite-chord-track
  (let [bar-len 32]
    (cycle (concat (repeat bar-len :b3)
                   (repeat 6 :a3)
                   (repeat ($ bar-len - 6) :g3)
                   (repeat bar-len :b3)
                   (repeat bar-len :d4)))))

(defn clever-mask-thing [s indices variations]
  (map (fn [d i]
         (when i
           (if (sequential? i)
             (for [t i] (variations d t))
             (variations d i))))
       s indices))

(defn infinite-pad-track-generator []
  (clever-mask-thing infinite-chord-track (infinite-rand-chain #'pad-timing-variations) #'chord-variations))

(defn my-midi-player [params]
  (let [len      (or (:sustain params) 0.5)
        note     (:note params)
        velocity (or (:velocity params) 90)
        device   (:device params)
        channel  (or (:channel params) 0)]
    (midi-note-on device note velocity channel)
    (after-delay (* 1000 len) #(midi-note-off device note channel))))

;(my-midi-player {:device clav :note 40})

(defn play-inst [player notes]
  (when notes
    (if (set? notes)
      (doseq [n notes] (player n))
      (player notes))))

(defn play-tracks
  "Plays a list of tracks, where each track is a map containing
   at least:

   :inst  - a function that will be called for every note in
   :notes - a (potentially infinite) list of notes, one for
            every beat, or nil for silence.

   If note is a single element, it will be played on the beat.  If note
   is a set of elements, they will be played on the beat.  If note is a
   list of elements, the beat will be split up and the notes will be
   played with even spacing in time.

   Completes when the end of any of the tracks is reached."
  ([bpm mixer tracks] (play-tracks bpm mixer tracks (now)))
  ([bpm mixer tracks start-time]
     (let [beat-interval  ($ 60000 / bpm)
           next-beat-time ($ start-time + beat-interval)]
       ;; Generate new tracks for the recursive call.
       ;; While doing so, play the head of each track.
       (let [new-tracks (for [[track-num {:keys [inst notes mixer-track] :as track}] (map-indexed vector tracks)]
                          (let [notes       (deref-if-var notes)
                                notes       (if (fn? notes) (notes) notes)
                                note        (first notes)
                                rest-notes  (next notes)
                                mixer-track (or mixer-track track-num)]
                            (binding [*out-bus* (:bus (nth (:tracks mixer) mixer-track))]
                              (if (sequential? note)
                                ;; This is a list of notes. Split this beat evenly and
                                ;; play them sequentially
                                (let [n-count      (count note)
                                      n-with-index (map-indexed vector note)]
                                  (doseq [[i n] n-with-index]
                                    (at ($ start-time + (i * (beat-interval / n-count))) (play-inst inst n))))

                                ;; This is either a single note or set of notes.
                                (at start-time (play-inst inst note))))

                            ;; Return the rest of the track with this note removed from the head of :notes.
                            (assoc track :notes rest-notes)))]
         (dorun new-tracks)
         (when (not-any? #(nil? (:notes %)) new-tracks)
           (binding [overtone.music.time/*apply-ahead* 0]
             (apply-at next-beat-time play-tracks [bpm
                                                   mixer
                                                   new-tracks
                                                   next-beat-time])))))))



(def my-tracks
  [{:name "Chords" :inst #(play-synth-with-bus *out-bus* my-overpad %) :notes #'infinite-pad-track-generator}
   {:name "Drums"  :inst drum    :notes #'infinite-drum-track-generator}
   {:name "Bass"   :inst #(play-synth-with-bus *out-bus* my-overpad %) :notes (cycle (concat [{:note (note :b2) :release 10 :amp 1.5}]
                                                                                             (repeat 31 nil)
                                                                                             [{:note (note :a2) :release 5 :amp 1.5}]
                                                                                             (repeat 5 nil)
                                                                                             [{:note (note :b2) :release 10 :amp 1.5}]
                                                                                             (repeat 23 nil)
                                                                                             [{:note (note :a2) :release 2 :amp 1.5}]
                                                                                             (repeat 1 nil)
                                                                                             [{:note (note :b2) :release 10 :amp 1.5}]
                                                                                             (repeat 31 nil)
                                                                                             [{:note (note :d3) :release 10 :amp 1.5}]
                                                                                             (repeat 31 nil)))}
   #_{:name "Piano Bass" :inst #(my-midi-player (assoc % :device clav)) :notes (cycle (concat [{:note (note :b2) :sustain 5}]
                                                                                             (repeat 31 nil)
                                                                                             [{:note (note :a2) :sustain 1}]
                                                                                             (repeat 5 nil)
                                                                                             [{:note (note :b2) :sustain 4}]
                                                                                             (repeat 23 nil)
                                                                                             [{:note (note :a2) :sustain 1}]
                                                                                             (repeat 1 nil)
                                                                                             [{:note (note :b2) :sustain 5}]
                                                                                             (repeat 31 nil)
                                                                                             [{:note (note :d3) :sustain 5}]
                                                                                             (repeat 31 nil)))}
   ])



(defsynth bus-out
  [src-bus 80 vol 1]
  (out 0 (* vol (in src-bus 2)) ))

(defn create-mixer [num-channels]
  (let [mixer-group (group "Multi-Track Mixer")
        start-bus 80]
    {:group mixer-group
     :tracks
     (doall (map #(let [src-bus (+ start-bus (* 2 %))
                        s (bus-out :src-bus src-bus)]
                    (group-append-node mixer-group s)
                    {:node s
                     :bus src-bus})
                 (range num-channels)))}))

(defn midi-ctl-mixer-tracks [mixer param f & ctlrs]
  (doseq [[{n :node} c] (map vector (:tracks mixer) ctlrs)]
    (on-controller-change c #(ctl n param (f %)))))

(when (resolve 'mixer)
  (kill (:group @(resolve 'mixer))))

(def mixer (create-mixer 8))

(midi-ctl-mixer-tracks mixer :vol #(/ % 127) 16 17 18)
(add-watch (atom-for-controller 19) :midi-vol (fn [k r old new]
                                                (clav-vol clav 0 new)))
;mixer

#_(midi-program-change clav 0 {:msb 0 :lsb 122 :patch 33})
(play-tracks ($ 4 * 130) mixer my-tracks)

(volume (/    26   64))
