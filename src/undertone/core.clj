(ns undertone.core
  (:use overtone.live
        overtone.inst.synth
        overtone.inst.drum
        ;overtone.inst.sampled-piano
        )
  (:import (java.util Random)))

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
    (midi-note-on clav n vel)
    (kick)
    (comment (after-delay delay #(
                                  (midi-note-on clav n vel)
                                  (after-delay delay (fn [] (midi-note-on clav n 0)))
                                  (when (> vel 30)
                                    (echo-note n vel delay)))))))

(defn intervals [notes]
  (if (< (count notes) 2)
    #{}
    (let [sorted-notes (sort notes)
          pairs        (partition 2 (interleave sorted-notes (rest sorted-notes)))]
      (map (fn [[a b]] (- b a)) pairs)
      )))

(def pressed-keys (atom #{}))
(def pedal-down (atom false))
(def sounding-notes (atom #{}))

(defn notes-updated [& args]
  (let [notes @sounding-notes
        chord (find-chord notes)]

    (println "Keys:" @pressed-keys)
    (println "Sounding Notes:" @sounding-notes)
    (println "Intervals: " (intervals notes))
    (println "Chord: " chord)

    (println)
    ))

(defn pedal-updated [k r old new]
  (let [pedal-released (not new)]
    (when pedal-released
      (swap! sounding-notes #(set (filter (fn [n] (contains? @pressed-keys n)) %)))
      (notes-updated))))

(add-watch sounding-notes :notes-updated #'notes-updated)
(add-watch pedal-down :pedal-updated #'pedal-updated)

(on-event [:midi :note-on] (fn [{note :note}]
                             ;(overpad {:attack 0.1 :release 0.3 :note note})
                             (swap! pressed-keys #(conj % note))
                             (swap! sounding-notes #(conj % note)))
          ::note-ons)

(on-event [:midi :note-off] (fn [{note :note}]
                              (swap! pressed-keys #(disj % note))
                              (when (not @pedal-down)
                                (swap! sounding-notes #(disj % note))))
          ::note-offs)

(on-event [:midi :control-change] (fn [{controller :data1 value :data2}]
                                    (when (= 64 controller)
                                      (let [down (< 63 value)]
                                        (compare-and-set! pedal-down (not down) down))))
          ::control-change)


;; INSTS

(defn piano-mirror [ns]
  (let [ns @current-notes]
    (doseq [n ns]
      (overpad {:note n :attack 0.1 :release 0.2}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some working sequence stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deref-if-var [s]
  (if (var? s) @s s))

(defn infinite-rand-chain [variations]
  (apply concat (repeatedly #(rand-nth (deref-if-var variations)))))

(defn drum [n]
  (cond
    (= n :kick) (sample-player (sample (freesound-path 777)) :vol 3)
    (= n :hat) (closed-hat)
    (= n :snare) (sample-player (sample (freesound-path 26903)) :vol 0.8)
    :else (println "Unknown drum:" n)))

(defn funky-sequence []
  [#{:kick :hat}
   :hat
   [:hat :hat]
   :hat
   #{:snare :hat}
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
     ]))


(defn chord-variations [c v]
  (let [params {:release 0.3 :attack 0.1}]
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
                   (repeat (- bar-len 6) :g3)
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

   If note is a single element, it will be played on the beat.
   If note is a set of elements, they will be played on the beat.
   If note is a list of elements, the beat will be split up and
   the notes will be played with even spacing in time.

   Completes when the end of any of the tracks is reached."
  ([bpm tracks] (play-tracks bpm tracks (now)))
  ([bpm tracks start-time]
     (let [beat-interval  (/ 60000 bpm)
           next-beat-time (+ start-time beat-interval)]
       ;; Generate new tracks for the recursive call.
       ;; While doing so, play the head of each track.
       (let [new-tracks (for [{:keys [inst notes] :as track} tracks]
                          (let [notes         (deref-if-var notes)
                                notes         (if (fn? notes) (notes) notes)
                                note       (first notes)
                                rest-notes (next notes)]
                            (if (sequential? note)
                              ;; This is a list of notes. Split this beat evenly and
                              ;; play them sequentially
                              (let [n-count      (count note)
                                    n-with-index (map-indexed vector note)]
                                (doseq [[i n] n-with-index]
                                  (at (+ start-time (* i (/ beat-interval n-count))) (play-inst inst n))))

                              ;; This is either a single note or set of notes.
                              (at start-time (play-inst inst note)))

                            ;; Return the rest of the track with this note removed from the head of :notes.
                            (assoc track :notes rest-notes)))]
         (dorun new-tracks)
         (when (not-any? #(nil? (:notes %)) new-tracks)
           (binding [overtone.music.time/*apply-ahead* 300]
             (apply-at next-beat-time play-tracks [bpm
                                                   new-tracks
                                                   next-beat-time])))))))

(def my-tracks
  [{:name "Chords" :inst overpad :notes #'infinite-pad-track-generator}
   {:name "Drums"  :inst drum    :notes #'infinite-drum-track-generator}
   {:name "Bass"   :inst overpad :notes (cycle (concat [{:note (note :b2) :release 10 :amp 1.5}]
                                                       (repeat 31 nil)
                                                       [{:note (note :a2) :release 5 :amp 1.5}]
                                                       (repeat 5 nil)
                                                       [{:note (note :b2) :release 10 :amp 1.5}]
                                                       (repeat 25 nil)
                                                       [{:note (note :b2) :release 10 :amp 1.5}]
                                                       (repeat 31 nil)
                                                       [{:note (note :d3) :release 10 :amp 1.5}]
                                                       (repeat 31 nil)))}
   ])

(play-tracks (* 4 130) my-tracks)
