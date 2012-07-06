(ns undertone.synth
  (:use [overtone.live]

        [undertone.novation]))

;(demo (sin-osc))

(defn midi-val-to-freq [val min-freq max-freq]
  (+ min-freq
     (* (- max-freq min-freq)
        (- (Math/pow 2 (/ val 127)) 1))))


(print-next-control-input)


(defsynth filter-test [q 0.01 lc 440]
  (let [sig (brown-noise)
        sig (rlpf sig lc q)
        ;sig (rlpf sig hc 2)
       ]
    (out 0 (pan2 sig))))

;(def s (filter-test))


(add-watch (atom-for-controller 8) :fc (fn [k r old new]
                                         (ctl s :lc (midi-val-to-freq new 100 1000))))
(add-watch (atom-for-controller 9) :fc (fn [k r old new]
                                         (ctl s :q (/ new 256))))
