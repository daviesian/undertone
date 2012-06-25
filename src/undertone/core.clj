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

;; INSTS

(definst my-kick
  [freq       {:default 50 :min 40 :max 140 :step 1}
   env-ratio  {:default 3 :min 1.2 :max 8.0 :step 0.1}
   freq-decay {:default 0.02 :min 0.001 :max 1.0 :step 0.001}
   amp-decay  {:default 0.5 :min 0.001 :max 1.0 :step 0.001}
   vol        {:default 1}]
  (let [fenv (* (env-gen (envelope [env-ratio 1] [freq-decay] :exp)) freq)
        aenv (env-gen (perc 0.005 amp-decay) :action FREE)]
    (* vol (sin-osc fenv (* 0.5 Math/PI)) aenv)))


(defsynth my-overpad
  [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
    (out 0 (pan2  audio))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some working sequence stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lookup-if-var [s]
  (if (var? s) @s s))

(defonce my-rand-gen (Random. (now)))

(defn my-rand-nth [coll]
  (let [r (Random. (now))
        c (count coll)]
    (nth coll (.nextInt my-rand-gen c))))

(defn infinite-rand-chain [variations]
  (apply concat (repeatedly #(my-rand-nth (lookup-if-var variations)))))

(defn funky-sequence []

  [[:kick :hat]
   :hat
   [:hat]
   :hat
   [:snare :hat]
   :hat
   [:hat]
   :hat])

(defn infinite-drum-track-generator []
  (apply concat (repeatedly #'funky-sequence)))

(def pad-timing-variations
  (let [- nil
        | 0
        + 1
        * 2]
    [[ | - - | - - | - - + - - + - - * - - * - - | - - | - | - + - + - ]
     [ | - - | - - + - - + - - * - - * - - | - - | - - | - - | - - + - ]]))

(def chord-variations
  (let [params {:release 1.5 :attack 0.2}]
    {:b3 {0 (map (fn [n] (assoc params :note n)) (take 2 (chord :b3 :minor)))
          1 (map (fn [n] (assoc params :note n)) (take 2 (chord :b3 :sus2)))
          2 (map (fn [n] (assoc params :note n)) (take 2 (chord :b3 :sus4)))}
     :a3 {0 (map (fn [n] (assoc params :note n)) (take 2 (chord :a3 :major)))
          1 (map (fn [n] (assoc params :note n)) (take 2 (chord :a3 :major)))
          2 (map (fn [n] (assoc params :note n)) (take 2 (chord :a3 :major)))}
     :g3 {0 (map (fn [n] (assoc params :note n)) (take 2 (chord :g3 :major)))
          1 (map (fn [n] (assoc params :note n)) (take 2 (chord :g3 :major)))
          2 (map (fn [n] (assoc params :note n)) (take 2 (chord :g3 :sus2)))}
     :d4 {0 (map (fn [n] (assoc params :note n)) (take 2 (chord :d4 :major)))
          1 (map (fn [n] (assoc params :note n)) (take 3 (chord :d4 :major)))
          2 (map (fn [n] (assoc params :note n)) (take 3 (chord :d4 :major)))}}))

(def infinite-chord-track
  (let [bar-len 32]
    (cycle (concat (repeat bar-len :b3)
                   (repeat 6 :a3)
                   (repeat (- bar-len 6) :g3)
                   (repeat bar-len :b3)
                   (repeat bar-len :d4)))))

(defn clever-dot-product-thing [s indices variations]
  (map (fn [d i]
         (when i (get-in @variations [d i])))
       s indices))

(defn infinite-pad-track-generator []
  (clever-dot-product-thing infinite-chord-track (infinite-rand-chain #'pad-timing-variations) #'chord-variations))


(defn play-drum [n]
  (cond
    (= n :kick) (my-kick :vol 4)
    (= n :hat) (closed-hat)
    (= n :snare) (do
                   (sample-player (sample (freesound-path 26903)) :vol 0.8))
    :else (println "urgh" n)))
(play-drum :snare)

(defn play-inst [player notes]
  (when notes
    (if (sequential? notes)
      (doseq [d notes]
        (player d))
      (player notes))))

(def piece
  {:insts {:drum #(play-inst play-drum %)
           :pad #(play-inst my-overpad %)
           }
   :tracks [[:pad  #'infinite-pad-track-generator]
            [:drum #'infinite-drum-track-generator]
            ]})


(defn play-piece [bpm piece start-time]
  (let [beat-interval  (/ 60000 bpm)
        next-beat-time (+ start-time beat-interval)
        tracks         (:tracks piece)
        insts          (:insts piece)]
    (let [new-tracks (for [[inst ns] tracks]
                       (let [ns         (if (var? ns) @ns ns)
                             ns         (if (fn? ns) (ns) ns)
                             i-fn       (insts inst)
                             note       (first ns)
                             rest-notes (next ns)]
                         (at start-time (i-fn note))
                         [inst rest-notes]))]
      (dorun new-tracks)
      (when (not-any? #(nil? (second %)) new-tracks)
        (apply-at next-beat-time play-piece [bpm
                                             (assoc piece :tracks new-tracks)
                                             next-beat-time])))))

(play-piece (* 4 128) piece (+ (now) 100))
