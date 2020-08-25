(ns aisong.songs.1
  (:require   [clojure.repl :refer :all]
              [aisong.instruments :as instruments]
              [aisong.patterns.dembow :as dembow]
              [aisong.utils :refer [pattern2phrase
                                      pattern-map2phrase
                                      chordvector2arpeggio
                                      chordvector2strum
                                      apply-drum]]
              [leipzig.live :as live]
              [leipzig.melody :refer :all]
              [leipzig.chord :as chord]
              [overtone.music.pitch :as pitch]
              [aisong.note-sequences :refer [phrase2noteseq coconet]]
              [overtone.osc :as osc]))


;;;;;;;;;;;;;;;;;;;;;;;;; Music Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def chords-map { :verse (concat
                          (->> [[:C3   :major  2]
                                [:B3   :major7 2]
                                [:Ab3  :major  2]
                                [:G3   :major  2]]
                               (repeat 2)
                               (apply concat))
                          (vector [:D3   :minor  2]
                                  [:F3   :minor7 4]
                                  [:C3   :major  2]
                                  [:D3   :maj9   1]
                                  [:G3   :major7 1])

                          )
                 :cliche [[:B3   :major  2]
                          [:F#3  :major  1]
                          [:G#3  :minor  3]
                          [:E3   :major  2]]})

(defn remap [m f]
  (into {} (for [[k v] m] [k (f v)])))

(def piano-progression (remap chords-map (fn [ch] (->> ch
                                                      (map #(chordvector2strum % 1 0))
                                                      (reduce #(then %2 %1))
                                                      (all :part :chords)))))

(def arpeggio-map (remap chords-map (fn [ch] (->> ch
                                                 (map #(chordvector2arpeggio % 1/4
                                                                             (fn [chord] (concat chord (reverse chord)))))
                                                 (reduce #(then %2 %1))
                                                 (all :part :piano)))))

(def bass-map (remap chords-map (fn [ch] (->> ch
                                             (map #(chordvector2arpeggio % 1/4
                                                                         (fn [chord] (for [n [0 -1 1 2]] (nth chord n nil)))))
                                             (reduce #(then %2 %1))
                                             (where :pitch #(- % 12))
                                             (all :part :bass)))))

(def sequential-dembow-phrase (->> (mapthen pattern-map2phrase
                                            (flatten [(repeat 2 dembow/claps-hats)]))
                                   (all :part :dembow)))



(def drums (let [basic-pattern {:kick      [1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0]
                                :snare     [0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0]
                                :close-hat [0 0 1 0 0 1 0 0 0 1 0 0 0 0 1 0]}
                 last-pattern  {:kick      [1 0 0 0 0 0 0 1 1 0 1 0 0 0 0 0]
                                :snare     [0 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0]
                                :close-hat [0 0 1 0 0 1 0 0 0 1 0 0 0 0 1 0]}]
             (->> basic-pattern
                  (repeat 3)
                  (#(conj (into [] %) last-pattern))
                  (apply merge-with into)
                  pattern-map2phrase
                  (all :part :dembow))))

;;;;;;;;;;;;;;;;;;;;;;;;; Instruments & Actions ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod live/play-note :dembow [{drum-key :drum}]
  (when-let [ drum (get dembow/drumkit drum-key)]
    (drum)))

(defmethod live/play-note :coconet [{pitch :pitch duration :duration instrument :instrument}]
  (when pitch
    (case instrument
      (1)   (instruments/traditional-blade :note pitch :amp 0.8)
      (2)   (instruments/retro-prophet :note pitch :amp 0.3)
      (0)   (instruments/basic-dpulse :note pitch :amp 0.05)
                                        ;()   (instruments/traditional-blade :note pitch :amp 0.8)
      ;(1)   (instruments/piano pitch :level 0.5 )
      (3)   (instruments/basic-fm :note (- pitch 12) :amp 0.8 :release (* duration 1))
      (nil? instrument)
      )))

(defmethod live/play-note :chords [{pitch :pitch duration :duration}]
  (when pitch
    (instruments/traditional-pluck :note pitch :amp 0.5)))

(defmethod live/play-note :bass [{pitch :pitch duration :duration}]
  (when pitch
    (instruments/basic-fm :note (- pitch 12) :amp 0.3 :sustain duration :release 0)))

;;;;;;;;;;;;;;;;;;;;;;  Magenta Models ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def coconet-seq (remap (merge-with with bass-map arpeggio-map)
                        (fn [ch] (->> ch
                                     coconet
                                     (all :part :coconet)
                                     (sort-by :time)
                                     (apply vector)))))

;;;;;;;;;;;;;;;;;;;;;  Main Song ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def main-song
  (->> [piano-progression bass-map]
       (map #(:verse %))
       (apply with)
       (apply-drum drums)
       ;(with  sequential-dembow-phrase)
       (tempo (bpm 100))
       )
  
  )

(def main-song (->> sequential-dembow-phrase
                    (tempo (bpm 90))))



;;;;;;;;;;;;;;;;;;;;;;; Jam & Recording ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def main-song (->> ;(apply with (map :cliche [coconet-seq piano-progression]))
                (reduce #(then %2 %1) [(:cliche coconet-seq)
                                       (:intro coconet-seq)])
                (apply-drum drums)
                (tempo (bpm 90))))

(live/jam (var main-song))


(live/stop)

 ((bpm 90) 1)


(duration (:intro coconet-seq))
