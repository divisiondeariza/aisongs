(ns aisong.utils
  (:require  [overtone.live  :refer :all]
             [leipzig.melody :refer :all]
             [overtone.music.pitch :as pitch]))

(defn apply-drum
  "Applies drums phrase until the end of melody phrase"
  [drums mel]
  (let [duration-phrase    (->> mel duration)
        duration-drums     (->> drums duration)
        repeat-drums-times (inc (quot duration-phrase duration-drums))]
    (->> drums
         (repeat repeat-drums-times)
         (reduce then)
         (take-while #(-> % :time (< duration-phrase)))
         (with mel))))

(defn chordvector2chord
  [chordvector]
  (let [root          (first  chordvector)
        chord-name    (second chordvector)]
    (pitch/chord root chord-name)))

(defn chordvector2strum
  ([chordvector strum-duration offset-start]
   (chordvector2strum chordvector strum-duration offset-start identity))
  ([chordvector strum-duration offset-start f_transform]
   (let [bars          (last chordvector)
         total-repeats (/ bars strum-duration)
         durations     (take total-repeats (repeat strum-duration))]
     (->> (chordvector2chord chordvector)
          (f_transform)
          (repeat total-repeats)
          (vector durations)
          (apply phrase)
          (where :time #(+ offset-start %))
          (all :duration (- strum-duration offset-start))))))

(defn chordvector2arpeggio
  "Generates an arpeggio from a chord vector"

  ([chordvector notes-length f-transform]
   (let [bars        (last chordvector)
         total-notes (/ bars notes-length)
         durations   (take total-notes (repeat notes-length))]
     (->> (chordvector2chord chordvector)
          (sort)
          (f-transform)
          (cycle)
          (take total-notes)
          (vector durations)
          (apply phrase)))))

(defn pattern2phrase
  "converts a pattern into a leipzig phrase.
   e.g. (pattern2phrase 0.25 [:inst [1 0 0 1])
  should return:
  [{:time 0,   :duration 0.25, :drum :inst}
   {:time 0.75 :duration 0.25, :drum :inst}]"

  ([length [instrument pattern]]
   (pattern2phrase length [instrument pattern] 0))

  ([length [instrument pattern] offset]
   (->> pattern
        (map (fn [step]
               (if (vector? step)
                 (pattern2phrase (/ length (count step)) [instrument step] (+ offset))
                 {:duration length, :drum (if (= step 0) nil instrument)})))
        (flatten)
        (mapthen (fn [step] [(assoc step :time 0)]))
        ;; Times don't sum up exactly with non 2 power rhythms, let's hope this doesn't break anything.
        )))

(defmacro defdrum-freesound
  [d-name drum-id & args]
  (let [args-map (apply hash-map args)
        params `[~'level   ~(:level   args-map 1)
                 ~'rate    ~(:rate    args-map 1)
                 ~'attack  ~(:attack  args-map 0)
                 ~'decay   ~(:decay   args-map 1)
                 ~'sustain ~(:sustain args-map 1)
                 ~'release ~(:release args-map 0.1)
                 ~'curve   ~(:curve   args-map -4)
                 ~'gate    ~(:gate    args-map 1)]]
    `(definst ~d-name
       ~params
       (let [env# (env-gen (adsr ~'attack ~'decay ~'sustain ~'release ~'level ~'curve)
                           :gate ~'gate
                           :action FREE)]
         (pan2 (~'* env# (freesound-inst ~drum-id)))))))

(defn pattern-map2phrase
  [pattern-map]
  (->> pattern-map
       (map (partial pattern2phrase 0.25))
       (apply with)
       )
  )

(defn play-on [metro first-beat times-after-beat instrument]
  (at (metro
      (+ first-beat (first  times-after-beat)))
      (instrument))
  (when (> (count (rest times-after-beat)) 0)
    (play-on metro first-beat (rest times-after-beat) instrument)
    ))

(defn if-nth-times [n actual-count then else?]
  (if (= 0 (mod actual-count n)) then else?)
  )

(defn freesound-inst [id]
  (play-buf 1  (load-sample (freesound-path id)) :action FREE  ))

(defn play-chord [a-chord inst]
  (doseq [note a-chord] (inst note)))
