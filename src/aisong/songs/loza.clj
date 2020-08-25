(ns aisong.songs.loza
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

(def chords-map { 
                 :verse [ [:E3   :minor  4]
                          [:B3   :minor  1]
                          [:F#3  :minor  1]
                          [:B3   :minor  2]]
                 :chorus [[:E3   :minor  4]
                          [:C3   :major  1]
                          [:A3   :minor  1]
                          [:B3   :minor  2]]})

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
      (1 2 3)   (instruments/basic-saw :note pitch :amp 0.2)
      (0)   (instruments/basic-dpulse :note pitch :amp 0.05)
                                        ;()   (instruments/traditional-blade :note pitch :amp 0.8)
      ;(1)   (instruments/piano pitch :level 0.5 )
      ;(3)   (instruments/basic-fm :note (- pitch 12) :amp 0.8 :release (* duration 1))
      (nil? instrument)
      )))

(defmethod live/play-note :coconet-piano [{pitch :pitch duration :duration instrument :instrument}]
  (when pitch
    (case instrument
      (1 2 3) (instruments/piano :note pitch :level 0.2)
      nil)))

(defmethod live/play-note :chords [{pitch :pitch duration :duration}]
  (when pitch
    (instruments/piano :note pitch :level 0.2)))

(defmethod live/play-note :bass [{pitch :pitch duration :duration}]
  (when pitch
    (instruments/basic-fm :note (- pitch 0) :amp 0.8 :sustain duration :release 0 :cutoff 60)))

;;;;;;;;;;;;;;;;;;;;;;  Magenta Models ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(instruments/basic-fm)
(def coconet-piano (remap (merge-with with bass-map arpeggio-map)
                        (fn [ch] (->> ch
                                     coconet
                                     (all :part :coconet-piano)
                                     (sort-by :time)
                                     (apply vector)))))

;;;;;;;;;;;;;;;;;;;;;  Main Song ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-section 
  [key & notemaps]
  (->> notemaps
       (map #(key %))
       (apply with)
       (apply-drum drums)))

(def main-song
  (->> (reduce #(then %2 %1)
               (concat
                (repeat 2 (:verse bass-map))
                (repeat 2 (create-section :verse piano-progression bass-map))
                (repeat 1 (create-section :verse
                                          (-> coconet-piano
                                              (update :verse
                                                      (fn [verse] (->> verse
                                                                       (filter #(.contains [1 2 3] (:instrument %)))
                                                                       (all :part :coconet)))))
                                          bass-map))
                (repeat 2 (create-section :verse piano-progression bass-map))
                (repeat 2 (create-section :verse piano-progression coconet-piano bass-map))
                (repeat 2 (create-section :verse piano-progression bass-map))
                (repeat 1 (:verse bass-map))
                (repeat 3 (create-section :chorus piano-progression bass-map))
                (repeat 1 (:verse bass-map))
                (repeat 2 (create-section :verse piano-progression bass-map))
                (repeat 1 (create-section :chorus piano-progression coconet-piano bass-map))
                (repeat 8 (create-section :chorus piano-progression bass-map))))
       (tempo (bpm 90))))

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

(last main-song) 