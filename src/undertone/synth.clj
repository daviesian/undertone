(ns undertone.synth
  (:use [overtone.live]
        [overtone.inst.synth]
        [undertone.novation]))

;(demo (sin-osc))

(defn midi-val-to-freq [val min-freq max-freq]
  (+ min-freq
     (* (- max-freq min-freq)
        (- (Math/pow 2 (/ val 127)) 1))))


;(print-next-control-input)


(defsynth filter-test [q 0.01 lc 440]
  (let [sig (brown-noise)
        sig (rlpf sig lc q)
        ;sig (rlpf sig hc 2)
        ]
    (out 0 (pan2 sig))))

(def s (filter-test))

(stop)

(ctl s :lc (midi-val-to-freq  30.0  100 10000))
(ctl s :q  0.33464566 )


(defsynth env-test [a 0.1 d 0.1 s 0.2 r 0.1 gate 1]
  (let [e   (env-gen (adsr a d s r) gate 1 0 1 FREE)
        sig (* e (lpf (saw) (* 4 440)))]
    (out 0 sig)
    ))

adsr


(let [s (env-test 0.1 0.2  0.2 0.1)]
  (Thread/sleep 1000)
  (ctl s :gate 0))



(defsynth my-mono-player
  "Plays a single channel audio buffer."
  [buf 0 rate 1.0 start-pos 0.0 loop? 0 vol 1 bus 0]
  (out bus (* vol
            (pan2
             (scaled-play-buf 1 buf rate
                              1 start-pos loop?
                              FREE)))))

(defsynth my-stereo-player
  "Plays a dual channel audio buffer."
  [buf 0 rate 1.0 start-pos 0.0 loop? 0 vol 1 bus 0]
  (out bus (* vol
            (scaled-play-buf 2 buf rate
                             1 start-pos loop?
                             FREE))))

(defsynth my-overpad
  [note 60 amp 0.7 attack 0.001 release 2 bus 0]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
    (out bus (pan2 audio))))

(defn my-sample-player
  "Play the specified sample with either a mono or stereo player
  depending on the number of channels in the sample. Accepts same args
  as both players, namely:
  [buf 0 rate 1.0 start-pos 0.0 loop? 0 vol 1]"
  [smpl & pargs] {:pre [(sample? smpl)]}
  (let [{:keys [path args]}     smpl
        {:keys [id n-channels]} (get @loaded-samples* [path args])]
    (cond
      (= n-channels 1) (apply my-mono-player id pargs)
      (= n-channels 2) (apply my-stereo-player id pargs))))

(defn play-synth-with-bus [bus synth args]
  (synth (assoc args :bus bus)))

(defsynth my-closed-hat
  [amp    {:default 0.3 :min 0.001 :max 1 :step 0.01}
   t      {:default 0.1 :min 0.1 :max 1.0 :step 0.01}
   low    {:default 6000 :min 3000 :max 12000 :step 1}
   hi     {:default 2000 :min 1000 :max 8000 :step 1}
   bus 0]
  (let [low (lpf (white-noise) low)
        hi (hpf low hi)
        env (line 1 0 t :action FREE)]
    (out bus (pan2 (* amp env hi)))))
