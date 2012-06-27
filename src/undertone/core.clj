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
                                        (compare-and-set! pedal-down (not down) down)))
                                    ;(println "Controller" controller ":" value ":" @pedal-down)
                                    )
          ::control-change)


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

(defn piano-mirror [ns]
  (let [ns @current-notes]
    (doseq [n ns]
      (overpad {:note n :attack 0.1 :release 0.2}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some working sequence stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lookup-if-var [s]
  (if (var? s) @s s))

(defn infinite-rand-chain [variations]
  (apply concat (repeatedly #(rand-nth (lookup-if-var variations)))))

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

(defn clever-dot-product-thing [s indices variations]
  (map (fn [d i]
         (when i
           (if (sequential? i)
             (for [t i] (variations d t))
             (variations d i))))
       s indices))

(defn infinite-pad-track-generator []
  (clever-dot-product-thing infinite-chord-track (infinite-rand-chain #'pad-timing-variations) #'chord-variations))



(defn play-drum [n]
  (cond
    (= n :kick) (sample-player (sample (freesound-path 777)) :vol 3);(my-kick :vol 4)
    (= n :hat) (closed-hat)
    (= n :snare) (sample-player (sample (freesound-path 26903)) :vol 0.8)
    :else (println "urgh" n)))


(defn play-inst [player notes]
  (when notes
    (if (set? notes)
      (doseq [d notes]
        (player d))
      (player notes))))

(def piece
  {:insts {:drum #(play-inst play-drum %)
           :pad #(play-inst overpad %)
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
                         (if (sequential? note)
                           (let [n-count      (count note)
                                 n-with-index (map (fn [n i] [n i]) note (range))]
                             (doseq [[n i] n-with-index]
                               (at (+ start-time (* i (/ beat-interval n-count))) (i-fn n))
                               ))
                           (at start-time (i-fn note)))
                         [inst rest-notes]))]
      (dorun new-tracks)
      (when (not-any? #(nil? (second %)) new-tracks)
        (binding [overtone.music.time/*apply-ahead* 10]
          (apply-at next-beat-time play-piece [bpm
                                               (assoc piece :tracks new-tracks)
                                               next-beat-time]))))))

(play-piece (* 4 130) piece (+ (now) 100))
