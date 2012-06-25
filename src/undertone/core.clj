(ns undertone.core
  (:use overtone.live
        overtone.inst.synth
        overtone.inst.drum
        ;overtone.inst.sampled-piano
        ;overtone.midi

        )
  (:import (java.util Random))
  ;(:import (javax.sound.midi Receiver Transmitter MidiSystem MidiMessage MidiDevice$Info MidiDevice))
  )

(demo (sin-osc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Here's some initial experiments with playing sequences
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn play-seq-at [notes synth interval time]
  (let [note            (first notes)
        remaining-notes (rest notes)
        next-time       (+ time interval)]
    (at time (synth note))
        (apply-at next-time play-seq-at [remaining-notes synth interval next-time])))

(defn play-seq [notes synth interval]
  (play-seq-at notes synth interval (+ 100 (now))))


(defn cycle-seq [notes synth interval]
  (play-seq (cycle notes) synth interval))

(let [start-time (+ (now) 100)]
  (play-seq-at (cycle [1]) closed-hat 400 start-time)
  (play-seq-at (cycle [62]) ks1 300 start-time))

(cycle-seq (chord :d3 :major) ks1 100)

(def C (chord :c4 :major))
(def Am (chord :a3 :minor))
(def F (chord :f3 :major))
(def G (chord :g3 :major))

(def chord-seq {1 C
                2 G
                3 Am
                4 F})

(defn play-chord-seq [chord-seq time]
  (let [chord (first chord-seq)
        rest  (rest chord-seq)
        next-time (+ time 1000)]
    (doseq [n chord]
      (at time (sampled-piano n)))
    (when rest
      (apply-at next-time  play-chord-seq [rest next-time]))))

(play-chord-seq (cycle [C Am F G]) (+ 100 (now)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some MIDI Stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def clav (midi-out "Clavinova"))

(on-event [:midi :note-on] (fn [{note :note velocity :velocity timestamp :timestamp}]
                             (println "Note: " note ", Velocity: " velocity ", Timestamp: " timestamp)
                             (echo-note note velocity 150)
                             )
          ::echo)

(remove-handler ::echo)

(defn echo-note [note velocity delay]
  (let [vel (* velocity 0.8)
        n (+ note 12)]
    (after-delay delay #(
                         (midi-note-on clav n vel)
                         (after-delay delay (fn [] (midi-note-on clav n 0)))
                         (when (> vel 30)
                           (echo-note n vel delay))))))

(defn intervals [notes]
  (if (< (count notes) 2)
    #{}
    (let [sorted-notes (sort notes)
          pairs        (partition 2 (interleave sorted-notes (rest sorted-notes)))]
      (map (fn [[a b]] (- b a)) pairs)
      )))

(def current-notes (atom #{}))

(defn notes-updated [k r old new]
  (println "New notes: " new)
  (println "Intervals: " (intervals new))
  (println "Chord: " (find-chord new))
  (println))

(add-watch current-notes :notes-updated #'notes-updated)

(on-event [:midi :note-on] (fn [{note :note}]
                             (swap! current-notes #(conj % note)))
          ::note-ons)

(on-event [:midi :note-off] (fn [{note :note}]
                             (swap! current-notes #(disj % note)))
          ::note-offs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some working sequence stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lookup-if-sym [s]
  (if (symbol? s) (eval s) s))

(defonce my-rand-gen (Random. (now)))

(defn my-rand-nth [coll]
  (let [r (Random. (now))
        c (count coll)]
    (nth coll (.nextInt my-rand-gen c))))

(defn infinite-rand-chain [variations]
  (apply concat (repeatedly #(my-rand-nth variations))))

(def infinite-drum-track
  (cycle [1 nil nil nil]))

(def pad-timing-variations
  (let [- nil
        | 0
        + 1
        * 2]
    [[ | - - | - - + - - + - - * - - * - - | - - | - - | - | - + - + - ]
     [ | - - | - - + - - + - - * - - * - - | - - | - - | - - | - - + - ]]))

(def infinite-chord-track
  (let [bar-len 32]
    (cycle (concat (repeat bar-len [(chord :b3 :minor)
                                    (chord :b3 :sus2)
                                    (chord :b3 :sus4)])
                   (repeat bar-len [(chord :a3 :major)
                                    (chord :a3 :major)
                                    (chord :a3 :major)])))))

(def infinite-pad-track
  (map (fn [[chord-vars chord-var-idx]]
         (when chord-var-idx (nth chord-vars chord-var-idx)))
       (partition 2 (interleave infinite-chord-track (infinite-rand-chain pad-timing-variations)))))


(play-piece (* 4 130) piece (+ (now) 100))

(def piece
  {:insts {:drum (fn [x] (when x (closed-hat)))
           :pad (fn [n] (when n
                         (if (seq? n)
                           (doseq [note n]
                             (overpad note))
                           (overpad n))))}
   :tracks [[:pad  infinite-pad-track]
            [:drum infinite-drum-track]]})

(defn play-piece [bpm piece start-time]
  (let [beat-interval (/ 60000 bpm)
        next-beat-time (+ start-time beat-interval)
        tracks (:tracks piece)
        insts (:insts piece)]
    (at start-time
      (doseq [t tracks]
          (let [i-fn (insts (first t))
                [note & rest-notes] (lookup-if-sym (second t))]
            (when i-fn
              (i-fn note)))))
    (let [new-tracks (for [[i ns] tracks]
                       [i (rest (lookup-if-sym ns))])]
      (apply-at next-beat-time play-piece [bpm
                                           (assoc piece :tracks new-tracks)
                                           next-beat-time]))))
