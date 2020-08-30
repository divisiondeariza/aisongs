(ns aisong.songs.protogenerator
    (:require ;[clojure.repl :refer :all]
     [aisong.instruments :as instruments]
     [aisong.patterns.dembow :as dembow]
     [aisong.utils :refer [;pattern2phrase
                           pattern-map2phrase
                           chordvector2arpeggio
               ;                     chordvector2strum
                           apply-drum
               ;                     generate-markov-chain
                           remap]]
              [leipzig.live :as live]
     [leipzig.melody :refer :all]
     [overtone.live :as overtone]
              ;[leipzig.chord :as chord]
              ;[overtone.music.pitch :as pitch]
     [aisong.magenta.coconet :refer [coconet]]
     [overtone.osc :as osc]
     [overtone.at-at :as at-at]
     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;; Music Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def chords-map {:verse (->> [[:E3   :minor  4]
                              [:B3   :minor  1]
                              [:F#3  :minor  1]
                              [:B3   :minor  2]]
                             (repeat 4)
                             (apply concat))
                 :chorus [[:E3   :minor  4]
                          [:C3   :major  1]
                          [:A3   :minor  1]
                          [:B3   :minor  2]]})

(def arpeggio-map (remap 
                   chords-map 
                   (fn [ch] (->> ch
                                 (map #(chordvector2arpeggio % 1/2
                                                             (fn [chord] (concat chord (reverse chord)))))
                                 (reduce #(then %2 %1))
                                 (all :part :piano)))))


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
                  (all :part :drums))))


(def c-sequences (remap arpeggio-map
                        (fn [ch] (->> ch
                                      coconet
                                      (all :part :coconet-piano)
                                      (sort-by :time)
                                      (apply vector)))))



(def osc-client (osc/osc-client "localhost" 57120))

(defn send-note [note instrument]
  (osc/osc-send osc-client "/note" note instrument))

(def my-pool (at-at/mk-pool))

(defmethod live/play-note :coconet-piano [{pitch :pitch duration :duration instrument :instrument}] 
  (when pitch
    (case instrument
      ;(1)   (instruments/basic-saw :note pitch :amp 0.3)
      ;(0 1 2)   (instruments/pitch-follow-2 :note pitch :level 0.3 :sustain duration)
      (0 1 2) (at-at/at 0 #(send-note (int (- pitch 0)) (int instrument)) my-pool)
      ;(0 2) (instruments/piano :note pitch :level 0.2)
      (3) (instruments/basic-fm :note (- pitch 12) :amp 0.3 :cutoff 60)
      nil)))

(defmethod live/play-note :drums [{drum-key :drum}]
  (when-let [drum (get dembow/drumkit drum-key)]
    (drum)))


(def main-song (->> (:verse c-sequences)
                    (apply-drum drums)
                    (tempo  (bpm 90))))


(live/jam (var main-song))

(send-note (int 50) (int 1))
(live/stop)  







